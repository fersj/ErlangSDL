-module(sdl_generator).

-export([main/1, generate_code/0, generate_code/1]).

-import(bbmustache, [render/2]).

-record(generator_info, {erl_file_gen, hrl_file_gen, erl_module_name, port_name,  c_file_gen, c_handler_file, c_lib_import}).
-record(macro_spec, {name, value}).
-record(type_spec, {erlang_name, c_name, type_descr, option}).
-record(fun_spec, {erlang_name, c_name, params, type_descr, option}).
-record(struct_member, {erlang_name, c_name, type_descr, option}).

-define(SEPARATOR_ERL, "%--------------------------------------------------------\n\n").
-define(SEPARATOR_C, "//--------------------------------------------------------\n\n").

%% -----------------------------------------------------------------------------
%  escript Entry point.
main(Args) ->
  case length(Args) of
    % Si no se incluye un el nombre del fichero de especificación como argumento se
    % toma por defecto el nombre de fichero "spec_file.txt".
    0 -> generate_code();
    1 -> generate_code(lists:nth(1, Args))
  end,
  erlang:halt(0).

%% -----------------------------------------------------------------------------
%  Lee un fichero de especificación y genera el código Erlang y el código C. Si no
%  se especifica ningun fichero se toma uno con nombre por defecto. El código generado
%  se escribe en ficheros Erlang (.erl, .hrl) y C (.c).
generate_code() ->
  generate_code("spec_file.txt").
generate_code(Filename) ->
  case file:open("resources/native_types_erl_list.txt", [read]) of
    {ok, InputDeviceNF} ->
      {ok, NativeFucsList} = io:read(InputDeviceNF, ""),
      file:close(InputDeviceNF);
    {error, ErrorNF} ->
      NativeFucsList = [],
      io:format("Error reading file: ~p~n", [ErrorNF])
  end,

  % Se lee el fichero de especificación y obtenemos la información de las distintas
  % tuplas que contiene.
  case file:open(Filename, [read]) of
    {ok, InputDeviceS} ->
      {ok, {spec, Info, {erlang_header, EHeader}, {c_header, CHeader}, MacroList, TypeList, FunList}} = io:read(InputDeviceS, ""),
      file:close(InputDeviceS),
      % Se borran los ficheros generados, en caso de existir, para generarlos de
      % nuevo desde cero.
      file:delete(Info#generator_info.erl_file_gen),
      file:delete(Info#generator_info.hrl_file_gen),
      file:delete(Info#generator_info.c_file_gen),

      % Se registra un proceso generator_helper para mantener alguna información global
      % necesaria para la generación de código.
      register(generator_helper, spawn(fun() -> generator_helper(length(NativeFucsList)+1, Info) end)),

      % ERLANG CODE
      generate_init_erl(Info#generator_info.erl_module_name),
      write_file(Info#generator_info.erl_file_gen, EHeader++"\n\n"++?SEPARATOR_ERL, [append]),
      generate_export(NativeFucsList, TypeList, FunList),
      generate_macros(MacroList),
      generate_init_port(Info#generator_info.port_name, Info#generator_info.c_handler_file),
      generate_native_types_parser_erl(),
      generate_types_parser_erl(TypeList),
      generate_functions_erl(FunList, TypeList),

      reset_fun_code(1),

      % C CODE
      generate_init_c(Info#generator_info.c_lib_import),
      write_file(Info#generator_info.c_file_gen, CHeader++"\n\n"++?SEPARATOR_C, [append]),
      generate_native_types_parser_c(),
      generate_types_parser_c(TypeList),
      generate_functions_c(FunList),
      generate_main_c();
    {error, ErrorS} ->
      io:format("Error reading file: ~p~n", [ErrorS])
  end,
  end_helper(),
  ok.

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el codigo con la definición del módulo y la
%  importación del fichero de cabecera (.hrl).
generate_init_erl(ModuleName) ->
  Content = read_file("resources/init_erl_code.erl"),
  NewContent = bbmustache:render(Content, #{"Module"=>ModuleName}),
  write_file(get_erl_filename(), NewContent, [append]).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el código correspondiente a los export de
%  las funciones generadas.
%  TODO Añadir los bytelist_to_XXX, XXX_to_bytelist y parse_XXX... etc para quitar el export_all.
generate_export(NativeFucsList, TypeList, FunList) ->
  generate_export(NativeFucsList, TypeList, FunList, lists:reverse(NativeFucsList)++["init_port/0"]).
generate_export(_NativeFucsList, [], [], Result) ->
  Line = "-export([\n\t"++string:join(lists:reverse(Result), ",\n\t")++"]).\n\n",
  write_file(get_erl_filename(), Line, [append]);
generate_export(NativeFucsList, [], [{_,Name,_,Params,_,_}|FunList], Result) ->
  P = [ T || {_,T,O} <- Params, not lists:member(return, O) ],
  Elem = atom_to_list(Name)++"/"++integer_to_list(length(P)),
  generate_export(NativeFucsList, [], FunList, [Elem|Result]);
generate_export(NativeFucsList, [T|TypeList], FunList, Result) ->
  #type_spec{erlang_name=ErlName, type_descr=TypeDescr} = T,
  case is_tuple(TypeDescr) of
    true when (element(1,TypeDescr)==pointer) ->
      Deref = "pointer_deref_"++atom_to_list(ErlName)++"/1",
      DerefArray = "pointer_deref_"++atom_to_list(ErlName)++"_array/2",
      DerefAssign = "pointer_deref_"++atom_to_list(ErlName)++"_assign/2",
      DerefArrayAssign = "pointer_deref_"++atom_to_list(ErlName)++"_array_assign/3",
      generate_export(NativeFucsList, TypeList, FunList, [DerefArrayAssign|[DerefAssign|[DerefArray|[Deref|Result]]]]);
    true when (element(1,TypeDescr)==struct) and (element(2,TypeDescr)/=opaque) ->
      Deref = "pointer_deref_"++atom_to_list(ErlName)++"/1",
      DerefArray = "pointer_deref_"++atom_to_list(ErlName)++"_array/2",
      DerefAssign = "pointer_deref_"++atom_to_list(ErlName)++"_assign/2",
      DerefArrayAssign = "pointer_deref_"++atom_to_list(ErlName)++"_array_assign/3",
      New = "new_"++atom_to_list(ErlName)++"/0",
      NewArray = "new_"++atom_to_list(ErlName)++"_array/1",
      Delete = "delete_"++atom_to_list(ErlName)++"/1",
      PreResult = [Delete|[NewArray|[New|[DerefArrayAssign|[DerefAssign|[DerefArray|[Deref|Result]]]]]]],
      NewResult = generate_export_setters_getters(element(2,TypeDescr), atom_to_list(ErlName), PreResult),
      generate_export(NativeFucsList, TypeList, FunList, NewResult);
    true when (element(1,TypeDescr)==union) and (element(2,TypeDescr)/=opaque) ->
      New = "new_"++atom_to_list(ErlName)++"/0",
      NewArray = "new_"++atom_to_list(ErlName)++"_array/1",
      Delete = "delete_"++atom_to_list(ErlName)++"/1",
      PreResult = [Delete|[NewArray|[New|Result]]],
      NewResult = generate_export_setters_getters(element(2,TypeDescr), atom_to_list(ErlName), PreResult),
      generate_export(NativeFucsList, TypeList, FunList, NewResult);
    true when element(2,TypeDescr)==opaque ->
      DerefArray = "pointer_deref_"++atom_to_list(ErlName)++"_array/2",
      DerefArrayAssign = "pointer_deref_"++atom_to_list(ErlName)++"_array_assign/3",
      NewArray = "new_"++atom_to_list(ErlName)++"_array/1",
      Delete = "delete_"++atom_to_list(ErlName)++"/1",
      NewResult = [Delete|[NewArray|[DerefArrayAssign|[DerefArray|Result]]]],
      generate_export(NativeFucsList, TypeList, FunList, NewResult);
    true ->
      generate_export(NativeFucsList, TypeList, FunList, Result);
    false ->
      generate_export(NativeFucsList, TypeList, FunList, Result)
  end.

%% -----------------------------------------------------------------------------
%  Genera los export de las funciones getXXX y setXXX de los union y structs. Devuelve
%  una lista con los nombres de las funciones de la forma: nombre_funcion/aridad.
generate_export_setters_getters([], _StructName, Result) -> Result;
generate_export_setters_getters([M|Members], StructName, Result) ->
  #struct_member{erlang_name=AttribName} = M,
  Get = StructName++"_get_"++atom_to_list(AttribName)++"/1",
  Set = StructName++"_set_"++atom_to_list(AttribName)++"/2",
  generate_export_setters_getters(Members, StructName, [Set|[Get|Result]]).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero de cabecera Erlang (.hrl) el código generado correspondiente
%  a las macros definidas en el fichero de especificación.
generate_macros([]) ->
  write_file(get_hrl_filename(), "\n", [append]);
generate_macros([Macro|MacroList]) ->
  Line = "-define(" ++ Macro#macro_spec.name ++ ", " ++ Macro#macro_spec.value ++ ").\n",
  case write_file(get_hrl_filename(), Line, [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end,
  generate_macros(MacroList).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el código de las funciones para el manejo del
%  puerto de comunicación Erlang-C.
generate_init_port(PortName, CHandlerFile) ->
  case file:read_file("resources/port_code.erl") of
    {ok, Content} -> ok;
    {error, Error} ->
      io:format("Error reading file: ~p~n", [Error]),
      Content = <<"">>
  end,
  Map = #{"PortName" => PortName,
          "CHandlerPath" => CHandlerFile},
  InitPort = bbmustache:render(Content, Map),
  write_file(get_erl_filename(), InitPort, [append]).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el código de las funciones para el manejo de
%  tipos básicos.
generate_native_types_parser_erl() ->
  {ok, File} = file:read_file("resources/native_types.erl"),
  Content = unicode:characters_to_list(File),
  write_file(get_erl_filename(), Content++?SEPARATOR_ERL, [append]).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el código generado para el manejo de tipos
%  definidos en el fichero de especificación.
generate_types_parser_erl([]) ->
  write_file(get_erl_filename(), ?SEPARATOR_ERL, [append]);
generate_types_parser_erl([TypeSpec|TypeList]) ->
  #type_spec{erlang_name=ErlName, type_descr=TypeDescr} = TypeSpec,
  case is_tuple(TypeDescr) of
    true ->
      case tuple_size(TypeDescr) of
        2 ->
          {Type, Desc} = TypeDescr;
        3 ->
          case is_tuple(element(2,TypeDescr)) of
            true -> {_, {Type, _}, Desc} = TypeDescr;
            false -> {_, Type, Desc} = TypeDescr
          end
      end;
    false ->
      Type = TypeDescr,
      Desc = undefined
  end,
  ContentMap = #{"ErlName"=>ErlName, "Type"=>Type, "Desc"=>Desc},

  % Se comprueba el tipo de dato y en función de eso se procede a generar su código.
  case TypeDescr of
    {T, opaque} when (T==struct) or (T==union) ->
      FinalMap = ContentMap,
      case file:read_file("resources/opaque_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {struct, MemberList} ->
      Args = string:join([atom_to_list(A) || {_,A,_,_,_} <- MemberList], ", "),
      DerefName = "pointer_deref_"++atom_to_list(ErlName),
      DerefArrayName = "pointer_deref_"++atom_to_list(ErlName)++"_array",
      DerefAssignName = "pointer_deref_"++atom_to_list(ErlName)++"_assign",
      DerefArrayAssignName = "pointer_deref_"++atom_to_list(ErlName)++"_array_assign",
      NewName = "new_"++atom_to_list(ErlName),
      NewArrayName = "new_"++atom_to_list(ErlName)++"_array",
      DeleteName = "delete_"++atom_to_list(ErlName),
      NewContentMap = #{"Args" => Args,
                        "Code" => integer_to_list(get_fun_code(DerefName)),
                        "CodeArray" => integer_to_list(get_fun_code(DerefArrayName)),
                        "CodeAssign" => integer_to_list(get_fun_code(DerefAssignName)),
                        "CodeArrayAssign" => integer_to_list(get_fun_code(DerefArrayAssignName)),
                        "CodeNew" => integer_to_list(get_fun_code(NewName)),
                        "CodeNewArray" => integer_to_list(get_fun_code(NewArrayName)),
                        "CodeDelete" => integer_to_list(get_fun_code(DeleteName))},
      FinalMap = maps:merge(ContentMap, NewContentMap),
      RecordLine = bbmustache:render(<<"-record({{ErlName}}, {{{{Args}}}}).\n">>, FinalMap),
      write_file(get_hrl_filename(), RecordLine, [append]),
      ContentPart1 = generate_struct_to_bytelist(MemberList, ErlName),
      ContentPart2 = generate_bytelist_to_struct(MemberList, ErlName),
      ContentPart3 = generate_parse_struct(MemberList, ErlName),
      case file:read_file("resources/struct_fun_templates.erl") of
        {ok, ContentPart4} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          ContentPart4 = <<"">>
      end,
      ContentPart5 = generate_getters_setters_erl(MemberList, ErlName),
      Content = <<ContentPart1/binary, ContentPart2/binary, ContentPart3/binary,
                  ContentPart4/binary, ContentPart5/binary>>;
    {union, MemberList} ->
      NewName = "new_"++atom_to_list(ErlName),
      NewArrayName = "new_"++atom_to_list(ErlName)++"_array",
      DeleteName = "delete_"++atom_to_list(ErlName),
      NewContentMap = #{"CodeNew" => integer_to_list(get_fun_code(NewName)),
                        "CodeNewArray" => integer_to_list(get_fun_code(NewArrayName)),
                        "CodeDelete" => integer_to_list(get_fun_code(DeleteName))},
      FinalMap = maps:merge(ContentMap, NewContentMap),
      case file:read_file("resources/union_fun_templates.erl") of
        {ok, ContentPart1} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          ContentPart1 = <<"">>
      end,
      ContentPart2 = generate_getters_setters_erl(MemberList, ErlName),
      Content = <<ContentPart1/binary, ContentPart2/binary>>;
    {enum, ElemList} ->
      FinalMap = ContentMap,
      GetInt = generate_enum_get_int(ElemList),
      GetAtom = generate_enum_get_atom(ElemList),
      case file:read_file("resources/enum_fun_templates.erl") of
        {ok, EnumFuns} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          EnumFuns = <<"">>
      end,
      Content = <<GetInt/binary, GetAtom/binary, EnumFuns/binary>>;
    {fixed_array, _TD, _Num} ->
      % TODO añadir funciones new, delete, deref...
      FinalMap = ContentMap,
      case file:read_file("resources/fixed_array_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {string, _Cod} ->
      FinalMap = ContentMap,
      case file:read_file("resources/string_cod_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {pointer, _TD} ->
      FinalMap = ContentMap,
      case file:read_file("resources/pointertype_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {T, _NBits} when (T==int) or (T==float) ->
      FinalMap = ContentMap,
      case file:read_file("resources/type_nbits_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    _ ->
      FinalMap = ContentMap,
      case file:read_file("resources/generic_type_fun_templates.erl") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end
  end,
  write_file(get_erl_filename(), binary_to_list(bbmustache:render(Content,FinalMap)), [append]),
  generate_types_parser_erl(TypeList).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para la función struct_to_bytelist que serializa el tipo
%  struct de nombre "StructName". Devuelve el código generado en forma de bitstring.
generate_struct_to_bytelist(Members, StructName) ->
  Init = <<"{{ErlName}}_to_bytelist(Value) ->\n"
          "\tReturn = [">>,
  generate_struct_to_bytelist(Members, StructName, Init).
generate_struct_to_bytelist([M|[]], StructName, Result) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  case Type of
    fixed_array ->
      Line = <<"{{TD}}_array_to_bytelist(Value#{{SN}}.{{EN}}, {{Size}})],\n"
                "\tlists:flatten(Return).\n\n">>,
      Map = #{"TD"=>element(2,TypeDescr), "SN"=>StructName, "EN"=>ErlName, "Size"=>element(3,TypeDescr)};
    _ ->
      Line = <<"{{TD}}_to_bytelist(Value#{{SN}}.{{EN}})],\n"
                "\tlists:flatten(Return).\n\n">>,
      Map = #{"TD"=>Type, "SN"=>StructName, "EN"=>ErlName}
  end,
  Final = bbmustache:render(Line, Map),
  <<Result/binary, Final/binary>>;
generate_struct_to_bytelist([M|Members], StructName, Result) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  case Type of
    fixed_array ->
      Line = <<"{{TD}}_array_to_bytelist(Value#{{SN}}.{{EN}}, {{Size}}),\n\t">>,
      Map = #{"TD"=>element(2,TypeDescr), "SN"=>StructName, "EN"=>ErlName, "Size"=>element(3,TypeDescr)};
    _ ->
      Line = <<"{{TD}}_to_bytelist(Value#{{SN}}.{{EN}}),\n\t">>,
      Map = #{"TD"=>Type, "SN"=>StructName, "EN"=>ErlName}
  end,
  NewLine = bbmustache:render(Line, Map),
  generate_struct_to_bytelist(Members, StructName, <<Result/binary, NewLine/binary>>).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para la función bytelist_to_struct que deserializa el tipo
%  struct de nombre "StructName". Devuelve el código generado en forma de bitstring.
generate_bytelist_to_struct(Members, StructName) ->
  InitLines = <<"bytelist_to_{{ErlName}}(Bytelist) ->\n"
          "\tR0 = Bytelist,\n">>,
  generate_bytelist_to_struct(Members, [], StructName, InitLines, 1).
generate_bytelist_to_struct([M|[]], MemberNames, StructName, Result, Cnt) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  MN = atom_to_list(ErlName)++"="++string:titlecase(atom_to_list(ErlName)),
  case Type of
    fixed_array ->
      Line = <<"\t{{{EN}}, _} = parse_{{TD}}_array(R{{CntPrev}}, {{Size}}),\n">>,
      Map = #{"TD"=>element(2,TypeDescr), "EN"=>string:titlecase(atom_to_list(ErlName)), "CntPrev"=>Cnt-1, "Size"=>element(3,TypeDescr)};
    _ ->
      Line = <<"\t{{{EN}}, _} = parse_{{TD}}(R{{CntPrev}}),\n">>,
      Map = #{"TD"=>Type, "EN"=>string:titlecase(atom_to_list(ErlName)), "CntPrev"=>Cnt-1}
  end,
  NewLine = bbmustache:render(Line, Map),
  MNames = string:join(lists:reverse([MN|MemberNames]), ", "),
  FinalLine = bbmustache:render(<<"\t#{{SN}}{{{{M}}}}.\n\n">>, #{"SN"=>StructName, "M"=>MNames}),
  <<Result/binary, NewLine/binary, FinalLine/binary>>;
generate_bytelist_to_struct([M|Members], MemberNames, StructName, Result, Cnt) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  MN = atom_to_list(ErlName)++"="++string:titlecase(atom_to_list(ErlName)),
  case Type of
    fixed_array ->
      Line = <<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}_array(R{{CntPrev}}, {{Size}}),\n">>,
      Map = #{"TD"=>element(2,TypeDescr), "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1, "Size"=>element(3,TypeDescr)};
    _ ->
      Line = <<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}(R{{CntPrev}}),\n">>,
      Map = #{"TD"=>Type, "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1}
  end,
  NewLine = bbmustache:render(Line, Map),
  generate_bytelist_to_struct(Members, [MN|MemberNames], StructName, <<Result/binary, NewLine/binary>>, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para la función parse_struct que deserializa el tipo struct
%  de nombre "StructName" devolviendo la lista de bytes sobrante. Devuelve el código
%  generado en forma de bitstring.
generate_parse_struct(Members, StructName) ->
  InitLines = <<"parse_{{ErlName}}(Bytelist) ->\n"
          "\tR0 = Bytelist,\n">>,
  generate_parse_struct(Members, [], StructName, InitLines, 1).
generate_parse_struct([], MemberNames, StructName, Result, Cnt) ->
  MNames = string:join(lists:reverse(MemberNames), ", "),
  Line = bbmustache:render(<<"\t{#{{SN}}{{{{M}}}}, R{{C}}}.\n\n">>, #{"SN"=>StructName, "M"=>MNames, "C"=>Cnt-1}),
  <<Result/binary, Line/binary>>;
generate_parse_struct([M|Members], MemberNames, StructName, Result, Cnt) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  MN = atom_to_list(ErlName)++"="++string:titlecase(atom_to_list(ErlName)),
  case Type of
    fixed_array ->
      Line = <<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}_array(R{{CntPrev}}, {{Size}}),\n">>,
      Map = #{"TD"=>element(2,TypeDescr), "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1, "Size"=>element(3,TypeDescr)};
    _ ->
      Line = <<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}(R{{CntPrev}}),\n">>,
      Map = #{"TD"=>Type, "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1}
  end,
  NewLine = bbmustache:render(Line, Map),
  generate_parse_struct(Members, [MN|MemberNames], StructName, <<Result/binary, NewLine/binary>>, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para las funciones get y set de cada elemento del tipo
%  struct de nombre "StructName". Devuelve el código generado en forma de bitstring.
generate_getters_setters_erl(MemberList, StructName) ->
  generate_getters_setters_erl(MemberList, MemberList, StructName, <<"">>).
generate_getters_setters_erl(MemberList, [], StructName, Result) ->
  NewLines = generate_array_getters_erl(MemberList, StructName),
  <<Result/binary, NewLines/binary>>;
generate_getters_setters_erl(MemberList, [M|MemberListIt], StructName, Result) ->
  #struct_member{erlang_name=AttribName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  case Type of
    fixed_array ->
      LineGet = "bytelist_to_"++atom_to_list(element(2,TypeDescr))++"_array(DataList, "++integer_to_list(element(3,TypeDescr))++")",
      LineSet = "AList = "++atom_to_list(element(2,TypeDescr))++"_array_to_bytelist(Attrib, "++integer_to_list(element(3,TypeDescr))++")";
    _ ->
      LineGet = "bytelist_to_"++atom_to_list(Type)++"(DataList)",
      LineSet = "AList = "++atom_to_list(Type)++"_to_bytelist(Attrib)"
  end,
  Get = <<"{{StructName}}_get_{{Attrib}}(Pointer) ->\n"
          "\tCode = int_to_bytelist({{CodeGet}}),\n"
          "\tPList = pointer_to_bytelist(Pointer),\n"
          "\tResultCall = call_port_owner(?PORT_NAME, [Code, PList]),\n"
          "\tcase ResultCall of\n"
          "\t\t{datalist, DataList} ->\n"
          "\t\t\t{{LineGet}};\n"
          "\t\tMsg ->\n"
          "\t\t\t{error, Msg}\n"
          "\tend.\n\n">>,
  Set = <<"{{StructName}}_set_{{Attrib}}(Pointer, Attrib) ->\n"
          "\tCode = int_to_bytelist({{CodeSet}}),\n"
          "\tPList = pointer_to_bytelist(Pointer),\n"
          "\t{{LineSet}},\n"
          "\tResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),\n"
          "\tcase ResultCall of\n"
          "\t\t{datalist, _DataList} ->\n"
          "\t\t\tok;\n"
          "\t\tMsg ->\n"
          "\t\t\t{error, Msg}\n"
          "\tend.\n\n">>,
  Map = #{"StructName" => StructName,
          "Attrib" => AttribName,
          "CodeGet" => integer_to_list(get_fun_code(atom_to_list(StructName)++"_get_"++atom_to_list(AttribName))),
          "CodeSet" => integer_to_list(get_fun_code(atom_to_list(StructName)++"_set_"++atom_to_list(AttribName))),
          "LineGet" => LineGet,
          "LineSet" => LineSet},
  NewLines = bbmustache:render(<<Get/binary, Set/binary>>, Map),
  generate_getters_setters_erl(MemberList, MemberListIt, StructName, <<Result/binary, NewLines/binary>>).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para las funciones get y set extra para obtener y modificar
%  los elementos de tipo array de un struct en forma de listas Erlang. Devuelve el
%  código generado en forma de bitstring.
generate_array_getters_erl(MemberList, StructName) ->
  LengthMembers = [{Name, Index} ||
                    {Name, [Index]} <- [{Name,[Index || {length_of, Index}<-Opt]} || {_,Name,_,_,Opt}<-MemberList, length(Opt)>0]],
  generate_array_getters_erl(MemberList, StructName, LengthMembers, <<"">>).
generate_array_getters_erl(_MemberList, _StructName, [], Result) -> Result;
generate_array_getters_erl(MemberList, StructName, [{AttribS,Index}|LengthMembers], Result) ->
  #struct_member{erlang_name=AttribP, type_descr={pointer,TypeP}} = lists:nth(Index, MemberList),
  GetArray = <<"{{StructName}}_get_arraylist_{{AttribP}}(Pointer) ->\n"
                  "\tArrayPtr = {{StructName}}_get_{{AttribP}}(Pointer),\n"
                  "\tSize = {{StructName}}_get_{{AttribS}}(Pointer),\n"
                  "\t{{TypeP}}_array_to_list(ArrayPtr, Size).\n\n">>,
  SetArray = <<"{{StructName}}_set_arraylist_{{AttribP}}(Pointer, List) ->\n"
                  "\tArrayPtr = {{StructName}}_get_{{AttribP}}(Pointer),\n"
                  "\tSize = {{StructName}}_get_{{AttribS}}(Pointer),\n"
                  "\t{{StructName}}_set_arraylist_{{AttribP}}(ArrayPtr, List, Size, 0).\n"
                "{{StructName}}_set_arraylist_{{AttribP}}(_ArrayPtr, _List, Size, Index) when Size==Index -> ok;\n"
                "{{StructName}}_set_arraylist_{{AttribP}}(ArrayPtr, [Value|List], Size, Index) ->\n"
                  "\tpointer_deref_{{TypeP}}_array_assign(ArrayPtr, Index, Value),\n"
                  "\t{{StructName}}_set_arraylist_{{AttribP}}(ArrayPtr, List, Size, Index+1).\n\n">>,
  Map = #{"StructName" => StructName,
          "AttribP" => AttribP,
          "AttribS" => AttribS,
          "TypeP" => TypeP},
  NewLines = bbmustache:render(<<GetArray/binary, SetArray/binary>>, Map),
  generate_array_getters_erl(MemberList, StructName, LengthMembers, <<Result/binary, NewLines/binary>>).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para las funciones get_int de un tipo enum. Devuelve el
%  código generado en forma de bitstring.
generate_enum_get_int(ElemList) ->
  Line = <<"{{ErlName}}_get_int(Atom) ->\n"
          "\tcase Atom of\n">>,
  generate_enum_get_int(ElemList, Line, 1).
generate_enum_get_int([], Result, _Cnt) ->
  Lines = <<"\t\t_ -> undefined\n"
            "\tend.\n\n">>,
  <<Result/binary, Lines/binary>>;
generate_enum_get_int([E|ElemList], Result, Cnt) ->
  case tuple_size(E) of
    2 -> Map = #{"Atom"=>atom_to_list(element(1,E)), "Int"=>integer_to_list(Cnt)};
    3 -> Map = #{"Atom"=>atom_to_list(element(1,E)), "Int"=>integer_to_list(element(3,E))};
    _ -> Map = undefined
  end,
  Line = <<"\t\t{{Atom}} -> {{Int}};\n">>,
  NewLine = bbmustache:render(Line, Map),
  generate_enum_get_int(ElemList, <<Result/binary, NewLine/binary>>, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera el código Erlang para las funciones get_atom de un tipo enum. Devuelve el
%  código generado en forma de bitstring.
generate_enum_get_atom(ElemList) ->
  Line = <<"{{ErlName}}_get_atom(Int) ->\n"
          "\tcase Int of\n">>,
  generate_enum_get_atom(ElemList, Line, 1).
generate_enum_get_atom([], Result, _Cnt) ->
  Lines = <<"\t\t_ -> undefined\n"
            "\tend.\n\n">>,
  <<Result/binary, Lines/binary>>;
generate_enum_get_atom([E|ElemList], Result, Cnt) ->
  case tuple_size(E) of
    2 -> Map = #{"Atom"=>atom_to_list(element(1,E)), "Int"=>integer_to_list(Cnt)};
    3 -> Map = #{"Atom"=>atom_to_list(element(1,E)), "Int"=>integer_to_list(element(3,E))};
    _ -> Map = undefined
  end,
  Line = <<"\t\t{{Int}} -> {{Atom}};\n">>,
  NewLine = bbmustache:render(Line, Map),
  generate_enum_get_atom(ElemList, <<Result/binary, NewLine/binary>>, Cnt+1).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero Erlang (.erl) el código generado para las funciones definidas
%  en el fichero de especificación.
generate_functions_erl([], _TypeList) -> ok;
generate_functions_erl([Fun|FunList], TypeList) ->
  #fun_spec{erlang_name=ErlName, params=Params, type_descr=Descr, option=Opt} = Fun,
  {StrParams, StrParamsNoFuns, FunParams} = fun_params_to_string(Params),
  VarList = case ["Param"++integer_to_list(I) || I <- lists:seq(1,length(StrParamsNoFuns))] of
              [] -> ["[]"];
              List -> List
            end,
  Header = <<"{{ErlName}}({{HParams}}) ->\n">>,
  Body1 = <<"\tCode = int_to_bytelist({{Code}}),\n"
            "{{BodyParams}}"
            "{{{FunWrappers}}}"
            "\tResultCall = call_port_owner(?PORT_NAME, [Code, {{Vars}}], [{{FunVars}}]),\n">>,
  case Descr of
    {pointer, PtrType} ->
      RetType = pointer;
    _ ->
      RetType = Descr,
      PtrType = undefined
  end,
  RPTypes = [T || {_,T,O} <- Params, lists:member(return, O)],
  case length(RPTypes) of
    0 -> RetParams = "\t\t\tRetParam1;\n";
    _ -> RetParams = generate_fun_return_params_erl([RetType|RPTypes])
  end,
  case lists:member(auto_managed, Opt) of
    true ->
      case find_type_spec_by_erlname(PtrType, TypeList) of
        undefined ->
          GCLines = "\t\t\tcase RetParamAux of\n"
                    "\t\t\t\t{raw_pointer, P} ->\n"
                    "\t\t\t\t\tRetParam1 = erlang_gc:manage_ptr(?MODULE, delete_"++atom_to_list(PtrType)++", P);\n"
                    "\t\t\t\tError -> RetParam1 = Error\n"
                    "\t\t\tend,\n";
        RetSpec ->
          Destructor = case [F || {destructor, F}<-RetSpec#type_spec.option] of
                          [] -> "delete_"++atom_to_list(PtrType);
                          [D] -> atom_to_list(D)
                        end,
          GCLines = "\t\t\tcase RetParamAux of\n"
                    "\t\t\t\t{raw_pointer, P} ->\n"
                    "\t\t\t\t\tRetParam1 = erlang_gc:manage_ptr(?MODULE, "++Destructor++", P);\n"
                    "\t\t\t\tError -> RetParam1 = Error\n"
                    "\t\t\tend,\n"
      end;
    false ->
      GCLines = "\t\t\tRetParam1 = RetParamAux,\n"
  end,
  if
    (RetType==void) and (length(RPTypes)==0) ->
      Body2 = <<"\tcase ResultCall of\n"
                "\t\t{datalist, _DataList} ->\n"
                "\t\t\tok;\n"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    (RetType==void) and (length(RPTypes)>0) ->
      Body2 = <<"\tcase ResultCall of\n"
                "\t\t{datalist, DataList} ->\n"
                "\t\t\tR0 = DataList,\n"
                "{{RetParams}}"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    true ->
      case length(RPTypes) of
        0 ->
          Body2 = <<"\tcase ResultCall of\n"
                    "\t\t{datalist, DataList} ->\n"
                    "\t\t\t{RetParamAux, _R1} = parse_{{RetType}}(DataList),\n"
                    "{{{GCLines}}}"
                    "{{RetParams}}"
                    "\t\tMsg ->\n"
                    "\t\t\t{error, Msg}\n"
                    "\tend.\n\n">>;
        _ ->
          Body2 = <<"\tcase ResultCall of\n"
                    "\t\t{datalist, DataList} ->\n"
                    "\t\t\t{RetParamAux, R1} = parse_{{RetType}}(DataList),\n"
                    "{{{GCLines}}}"
                    "{{RetParams}}"
                    "\t\tMsg ->\n"
                    "\t\t\t{error, Msg}\n"
                    "\tend.\n\n">>
      end
  end,

  Map = #{"ErlName" => atom_to_list(ErlName),
          "HParams" => string:join(StrParams, ", "),
          "BodyParams" => generate_body_parameters_to_send(Params, StrParamsNoFuns),
          "Code" => integer_to_list(get_fun_code(atom_to_list(ErlName))),
          "Vars" => string:join(VarList,", "),
          "RetType" => atom_to_list(RetType),
          "RetParams" => RetParams,
          "GCLines" => GCLines,
          "FunWrappers" => generate_fun_wrappers_erl(FunParams),
          "FunVars" => string:join([N++"_Wrapper" || {N,_,_}<-FunParams], ", ")},
  FunContent = bbmustache:render(<<Header/binary, Body1/binary, Body2/binary>>, Map),
  FunContentArrays = generate_fun_with_array_size_erl(ErlName, Params, Descr),
  write_file(get_erl_filename(), <<FunContent/binary, FunContentArrays/binary>>, [append]),
  generate_functions_erl(FunList, TypeList).

%% -----------------------------------------------------------------------------
%  Genera a partir de la lista de parámetros de una función una tupla que contiene:
%    -Una lista con los nombres de variable generados para cada parámetro.
%    -Una lista similar a la anterior pero excluyendo los parámetros funcionales.
%    -Una lista solo con los nombres de los parámetros funcionales.
%  La información que contiene esta tupla es utilizada en la generación del código
%  de las funciones en Erlang.
fun_params_to_string(Params) ->
  fun_params_to_string(Params, [], [], [], 1).
fun_params_to_string([], Result, ResultNoFuns, FunVars, _Cnt) ->
  {lists:reverse(Result), lists:reverse(ResultNoFuns), lists:reverse(FunVars)};
fun_params_to_string([{_,P,O}|Params], Result, ResultNoFuns, FunVars, Cnt) ->
  case lists:member(return, O) of
    true ->
      NewResult = Result,
      NewResultNoFuns = ResultNoFuns,
      NewFunVars = FunVars;
    false ->
      case P of
        {pointer, Type} ->
          Elem = "P_"++string:titlecase(atom_to_list(Type)) ++ "_" ++ integer_to_list(Cnt),
          NewResult = [Elem|Result],
          NewResultNoFuns = [Elem|ResultNoFuns],
          NewFunVars = FunVars;
        {function, PTypes, RType} ->
          Elem = "Fun"++integer_to_list(length(FunVars)+1),
          NewFunVars = [{Elem, PTypes, RType}|FunVars],
          NewResultNoFuns = ResultNoFuns,
          NewResult = [Elem|Result];
        _ ->
          Elem = string:titlecase(atom_to_list(P)) ++ "_" ++ integer_to_list(Cnt),
          NewResult = [Elem|Result],
          NewResultNoFuns = [Elem|ResultNoFuns],
          NewFunVars = FunVars
      end
  end,
  fun_params_to_string(Params, NewResult, NewResultNoFuns, NewFunVars, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera a partir de la lista de parámetros de una función una lista cuyos elementos
%  contienen:
%    -Nombre asignado al parámetro.
%    -Número de parámetro en fucnión de su posición en la cabecera de la función.
%    -Tipo del parámetro.
%    -Opciones del parámetro.
%  Esta infomación es generada con el fin de ser utilizada en la generación de funciones
%  Erlang, que reciben como parámetro listas que son interpretadas como arrays.
fun_params_to_tuple_info(Params) ->
  fun_params_to_tuple_info(Params, [], 1).
fun_params_to_tuple_info([], Result, _Cnt) -> lists:reverse(Result);
fun_params_to_tuple_info([{_,P,O}|Params], Result, Cnt) ->
  case lists:member(return, O) of
    true ->
      NewResult = Result;
    _ ->
      case P of
        {pointer, Type} ->
          Name = "P_"++string:titlecase(atom_to_list(Type))++"_"++integer_to_list(Cnt),
          NewResult = [{Name,Cnt,Type,O}|Result];
        {function, _PTypes, _RType} ->
          Name = "Fun"++integer_to_list(Cnt),
          NewResult = [{Name,Cnt,function,O}|Result];
        Type ->
          Name = string:titlecase(atom_to_list(Type)) ++ "_" ++ integer_to_list(Cnt),
          NewResult = [{Name,Cnt,Type,O}|Result]
      end
  end,
  fun_params_to_tuple_info(Params, NewResult, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera la parte del código Erlang de la función correspondiente a la serialización
%  de sus parámetros. Devuelve el código generado en forma de string.
generate_body_parameters_to_send(Params, StrParams) ->
  generate_body_parameters_to_send(Params, StrParams, "", 1).
generate_body_parameters_to_send(_Params, [], Result, _Cnt) -> Result;
generate_body_parameters_to_send([{_,T,O}|Params], [S|StrParams], Result, Cnt) ->
  case lists:member(return,O) of
    true ->
      Line = "";
    _ ->
      case T of
        {pointer, _Type} ->
          Line = "\tParam" ++ integer_to_list(Cnt) ++ " = pointer_to_bytelist("++S++"),\n";
        {function, _, _} ->
          Line = "";
        _ ->
          Line = "\tParam" ++ integer_to_list(Cnt) ++ " = " ++ atom_to_list(T) ++ "_to_bytelist("++S++"),\n"
      end
  end,
  generate_body_parameters_to_send(Params, StrParams, Result++Line, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera la parte del código Erlang correspondiente a los wrappers necesarios para
%  la ejecución de fucniones de orden superior. Devuelve el código de las funciones
%  wrapper para cada parámetro funcional en formato bitstring.
generate_fun_wrappers_erl(FunVars) ->
  generate_fun_wrappers_erl(FunVars, <<"\n">>).
generate_fun_wrappers_erl([], Result) -> Result;
generate_fun_wrappers_erl([{FunName,PTypes,RType}|FunVars], Result) ->
  FunWrapper = <<"\t{{FunName}}_Wrapper = fun(Buf) ->\n"
            "{{ParseArgs}}"
            "\t\tResult = {{FunName}}({{Args}}),\n"
            "\t\t{{RetType}}_to_bytelist(Result)\n"
            "\tend,\n\n">>,
  {ParseArgs, ArgNames} = generate_fun_wrapper_args_parser_erl(PTypes),
  Map = #{"FunName" => FunName,
          "ParseArgs" => ParseArgs,
          "Args" => string:join(ArgNames, ", "),
          "RetType" => RType},
  NewFunWrapper = bbmustache:render(FunWrapper, Map),
  generate_fun_wrappers_erl(FunVars, <<Result/binary, NewFunWrapper/binary>>).

%% -----------------------------------------------------------------------------
%  Genera, a partir de la lista de tipos de una función wrapper, una tupla que contiene:
%    -Un bitstring con el código generado para serializar los parámetros de una función
%     wrapper.
%    -Una lista con los nombre de variable generados para cada parámetro.
%  Esta información es utilizada para generar el código Erlang de las funciones wrapper
%  de las funciones de orden superior.
generate_fun_wrapper_args_parser_erl(PTypes) ->
  generate_fun_wrapper_args_parser_erl(PTypes, <<"\t\tR0 = Buf,\n">>, [], 1).
generate_fun_wrapper_args_parser_erl([P|[]], Result, ParamList, Cnt) ->
  ParamName = "P"++integer_to_list(Cnt),
  Line = <<"\t\t{{{Param}}, {{R}}} = parse_{{Type}}({{Bytelist}}),\n">>,
  Map = #{"Param" => ParamName,
          "R" => "_R"++integer_to_list(Cnt),
          "Type" => P,
          "Bytelist" => "R"++integer_to_list(Cnt-1)},
  NewLine = bbmustache:render(Line, Map),
  {<<Result/binary, NewLine/binary>>, lists:reverse([ParamName|ParamList])};
generate_fun_wrapper_args_parser_erl([P|PTypes], Result, ParamList, Cnt) ->
  ParamName = "Param"++integer_to_list(Cnt),
  Line = <<"\t\t{{{Param}}, {{R}}} = parse_{{Type}}({{Bytelist}}),\n">>,
  Map = #{"Param" => ParamName,
          "R" => "R"++integer_to_list(Cnt),
          "Type" => P,
          "Bytelist" => "R"++integer_to_list(Cnt-1)},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_wrapper_args_parser_erl(PTypes, <<Result/binary, NewLine/binary>>, [ParamName|ParamList], Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera la parte del código de la función en Erlang correspondiente a la deserialización
%  de sus parámetros de salida (en caso de tenerlos). Devuelve el código generado en
%  forma de string.
generate_fun_return_params_erl([void|ParamList]) ->
  generate_fun_return_params_erl(ParamList, <<"">>, [], 1);
generate_fun_return_params_erl([_|ParamList]) ->
  generate_fun_return_params_erl(ParamList, <<"">>, ["RetParam1"], 2).
generate_fun_return_params_erl([P|[]], Result, ParamNames, Cnt) ->
  Line = <<"\t\t\t{RetParam{{Cnt}}, _R{{Cnt}}} = parse_{{Type}}(R{{PrevCnt}}),\n">>,
  NewPName = "RetParam"++integer_to_list(Cnt),
  case P of
    {pointer, T} -> Type = T;
    T -> Type = T
  end,
  PN = string:join(lists:reverse([NewPName|ParamNames]), ", "),
  Map = #{"Cnt"=>integer_to_list(Cnt), "Type"=>Type, "PrevCnt"=>integer_to_list(Cnt-1), "PNames"=>PN},
  FinalLine = <<"\t\t\t{{{{PNames}}}};\n">>,
  NewResult = bbmustache:render(<<Result/binary, Line/binary, FinalLine/binary>>, Map),
  binary_to_list(NewResult);
generate_fun_return_params_erl([P|ParamList], Result, ParamNames, Cnt) ->
  Line = <<"\t\t\t{RetParam{{Cnt}}, R{{Cnt}}} = parse_{{Type}}(R{{PrevCnt}}),\n">>,
  NewPName = "RetParam"++integer_to_list(Cnt),
  case P of
    {pointer, T} -> Type = T;
    T -> Type = T
  end,
  Map = #{"Cnt"=>integer_to_list(Cnt), "Type"=>Type, "PrevCnt"=>integer_to_list(Cnt-1)},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_return_params_erl(ParamList, <<Result/binary, NewLine/binary>>, [NewPName|ParamNames], Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera el código para funciones Erlang adicionales que reciben como parámetro
%  listas que son interpretadas como arrays haciendo uso de las otras funciones
%  de manejo de arrays generadas. Devuelve el código en forma de bitstring.
generate_fun_with_array_size_erl(FunName, Params, Descr) ->
  TupleParams = fun_params_to_tuple_info(Params),
  LengthParams = [{Name, Index, IndexPtr} ||
                  {Name, Index, [IndexPtr]} <- [{Name,Index,[IndexPtr || {length_of, IndexPtr}<-Opt]} || {Name,Index,_,Opt}<-TupleParams, length(Opt)>0]],
  case LengthParams of
    [] -> <<"">>;
    _ ->
      LengthIndexes = [I || {_,I,_}<-LengthParams],
      PtrIndexes = [I || {_,_,I}<-LengthParams],
      HParams1 = [{Name,Index} || {Name,Index,_,_}<-TupleParams, lists:member(Index, LengthIndexes)==false],
      HParams2 = [case lists:member(Index, PtrIndexes) of
                    true -> "List"++integer_to_list(Index);
                    false -> Name
                  end ||
                  {Name,Index}<-HParams1],
      Line = <<"{{FunName}}({{HParamsMod}}) ->\n">>,
      Map = #{"FunName"=> FunName, "HParamsMod"=>string:join(HParams2, ", ")},
      Header = bbmustache:render(Line, Map),
      generate_fun_with_array_size_erl(FunName, TupleParams, Descr, LengthParams, Header, [])
  end.
generate_fun_with_array_size_erl(_FunName, _TupleParams, Descr, [], Result, []) ->
  case Descr of
    void ->
      Line = <<"\tok.\n\n">>;
    _ ->
      Line = <<"\tValue.\n\n">>
  end,
  <<Result/binary, Line/binary>>;
generate_fun_with_array_size_erl(FunName, TupleParams, Descr, [], Result, [{PtrName,PtrType}|PtrToDelete]) ->
  Line = <<"\tdelete_{{Type}}({{PtrName}}),\n">>,
  Map = #{"Type"=>PtrType, "PtrName"=>PtrName},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_with_array_size_erl(FunName, TupleParams, Descr, [], <<Result/binary, NewLine/binary>>, PtrToDelete);
generate_fun_with_array_size_erl(FunName, TupleParams, Descr, [{Name,Index,IndexPtr}|[]], Result, PtrToDelete) ->
  Lines = <<"\t{{ParamPtr}} = list_to_{{Type}}_array({{List}}),\n"
            "\t{{ParamSize}} = length({{List}}),\n">>,
  case Descr of
    void ->
      FunLine = <<"\t{{FunName}}({{HParams}}),\n">>;
    _ ->
      FunLine = <<"\tValue = {{FunName}}({{HParams}}),\n">>
  end,
  PtrName = element(1, lists:nth(IndexPtr,TupleParams)),
  PtrType = element(3, lists:nth(IndexPtr,TupleParams)),
  ListName = "List"++integer_to_list(IndexPtr),
  StrParams = [N || {N,_,_,_}<-TupleParams],
  Map = #{"Type" => PtrType,
          "ParamPtr" => PtrName,
          "ParamSize" => Name,
          "List" => ListName,
          "FunName" => FunName,
          "HParams" => string:join(StrParams, ", ")},
  NewLines = bbmustache:render(<<Lines/binary, FunLine/binary>>, Map),
  case lists:member(free_after, element(4, lists:nth(Index, TupleParams))) of
    true -> NewPtrToDelete = [{PtrName,PtrType}|PtrToDelete];
    false -> NewPtrToDelete = PtrToDelete
  end,
  generate_fun_with_array_size_erl(FunName, TupleParams, Descr, [], <<Result/binary, NewLines/binary>>, NewPtrToDelete);
generate_fun_with_array_size_erl(FunName, TupleParams, Descr, [{Name,Index,IndexPtr}|LengthParams], Result, PtrToDelete) ->
  Lines = <<"\t{{ParamPtr}} = list_to_{{Type}}_array({{List}}),\n"
            "\t{{ParamSize}} = length({{List}}),\n">>,
  PtrName = element(1, lists:nth(IndexPtr,TupleParams)),
  PtrType = element(3, lists:nth(IndexPtr,TupleParams)),
  ListName = "List"++IndexPtr,
  Map = #{"Type" => PtrType,
          "ParamPtr" => PtrName,
          "ParamSize" => Name,
          "List" => ListName},
  NewLines = bbmustache:render(Lines, Map),
  case lists:member(free_after, element(4, lists:nth(Index, TupleParams))) of
    true -> NewPtrToDelete = [{PtrName,PtrType}|PtrToDelete];
    false -> NewPtrToDelete = PtrToDelete
  end,
  generate_fun_with_array_size_erl(FunName, TupleParams, Descr, LengthParams, <<Result/binary, NewLines/binary>>, NewPtrToDelete).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero C (.c) el código de la cabecera con la importación de librerías
%  y la definición de constantes, macros y algunas funciones de uso general.
generate_init_c(CLib) ->
  Content = read_file("resources/init_c_code.c"),
  NewContent = bbmustache:render(Content, #{"CLib"=>CLib}),
  write_file(get_c_filename(), NewContent, [append]).

generate_native_types_parser_c() ->
  {ok, File} = file:read_file("resources/native_types.c"),
  Content = unicode:characters_to_list(File),
  write_file(get_c_filename(), Content++?SEPARATOR_C, [append]),
  add_type_name(int, "int"),
  add_type_name(float, "float"),
  add_type_name(double, "double"),
  add_type_name(string, "string"),
  add_type_name(pointer, "void*"),
  case file:open("resources/native_types_c_handlers_list.txt", [read]) of
    {ok, InputDevice} ->
      {ok, L} = io:read(InputDevice, ""),
      file:close(InputDevice);
    {error, Error} ->
      L = [],
      io:format("Error reading file: ~p~n", [Error])
  end,
  [add_fun_code(F) || F<-L],
  ok.

%% -----------------------------------------------------------------------------
%  Escribe en el fichero C (.c) el código generado para el manejo de tipos
%  definidos en el fichero de especificación.
generate_types_parser_c([]) ->
  write_file(get_c_filename(), ?SEPARATOR_C, [append]);
generate_types_parser_c([TypeSpec|TypeList]) ->
  %io:format("Type spec: ~p~n", [Type]),
  #type_spec{erlang_name= ErlName, c_name=CName, type_descr=TypeDescr} = TypeSpec,
  case is_tuple(TypeDescr) of
    true ->
      case tuple_size(TypeDescr) of
        2 ->
          {Type, Desc} = TypeDescr;
        3 ->
          case is_tuple(element(2,TypeDescr)) of
            true -> {_, {Type, _}, Desc} = TypeDescr;
            false -> {_, Type, Desc} = TypeDescr
          end
      end;
    false ->
      Type = TypeDescr,
      Desc = undefined
  end,
  ContentMap = #{"ErlName"=>ErlName,
                  "CName"=>CName,
                  "Type"=>Type,
                  "Desc"=>Desc,
                  "DescC"=>get_type_name(Desc)},
  add_type_name(ErlName, CName),

  case TypeDescr of
    {T, opaque} when (T==struct) or (T==union) ->
      FinalMap = ContentMap,
      case file:read_file("resources/opaque_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {struct, MemberList} ->
      FinalMap = ContentMap,
      ErlNameStr = atom_to_list(ErlName),
      ReadStruct = generate_read_struct_c(MemberList),
      WriteStruct = generate_write_struct_c(MemberList),
      case file:read_file("resources/struct_fun_templates.c") of
        {ok, Handlers} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Handlers = <<"">>
      end,
      add_fun_code("pointer_deref_"++ErlNameStr++"_Handler"),
      add_fun_code("pointer_deref_"++ErlNameStr++"_array_Handler"),
      add_fun_code("pointer_deref_"++ErlNameStr++"_assign_Handler"),
      add_fun_code("pointer_deref_"++ErlNameStr++"_array_assign_Handler"),
      add_fun_code("new_"++ErlNameStr++"_Handler"),
      add_fun_code("new_"++ErlNameStr++"_array_Handler"),
      add_fun_code("delete_"++ErlNameStr++"_Handler"),
      GettersSetters = generate_getters_setters_c(MemberList, ErlNameStr, CName),
      Content = <<ReadStruct/binary, WriteStruct/binary, Handlers/binary, GettersSetters/binary>>;
    {union, MemberList} ->
      FinalMap = ContentMap,
      ErlNameStr = atom_to_list(ErlName),
      case file:read_file("resources/union_fun_templates.c") of
        {ok, UnionFuncs} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          UnionFuncs = <<"">>
      end,
      add_fun_code("new_"++ErlNameStr++"_Handler"),
      add_fun_code("new_"++ErlNameStr++"_array_Handler"),
      add_fun_code("delete_"++ErlNameStr++"_Handler"),
      GettersSetters = generate_getters_setters_c(MemberList, ErlNameStr, CName),
      Content = <<UnionFuncs/binary, GettersSetters/binary>>;
    {enum, _ElemList} ->
      FinalMap = ContentMap,
      case file:read_file("resources/enum_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {fixed_array, _TD, _Num} ->
      FinalMap = ContentMap,
      case file:read_file("resources/fixed_array_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {pointer, _TD} ->
      FinalMap = ContentMap,
      case file:read_file("resources/pointertype_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    {T, NBits} when (T==int) or (T==float) ->
      FinalMap = maps:put("Casting", atom_to_list(T)++integer_to_list(NBits)++"_t *", ContentMap),
      case file:read_file("resources/type_nbits_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end;
    _ ->
      FinalMap = ContentMap,
      case file:read_file("resources/generic_type_fun_templates.c") of
        {ok, Content} -> ok;
        {error, Error} ->
          io:format("Error reading file: ~p~n", [Error]),
          Content = <<"">>
      end
  end,
  write_file(get_c_filename(), binary_to_list(bbmustache:render(Content,FinalMap)), [append]),
  generate_types_parser_c(TypeList).

%% -----------------------------------------------------------------------------
%  Genera el código C para la función read_struct que deserializa un tipo de dato
%  struct. Devuelve el código generado en forma de bitstring.
generate_read_struct_c(MemberList) ->
  InitFun = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
              "\tbyte *current_in = in;\n\n">>,
  generate_read_struct_c(MemberList, InitFun).
generate_read_struct_c([], Result) ->
  EndFun = <<"\n\treturn current_in;\n}\n\n">>,
  <<Result/binary, EndFun/binary>>;
generate_read_struct_c([M|MemberList], Result) ->
  #struct_member{c_name=CName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  case Type of
    fixed_array ->
      Line = <<"\tcurrent_in = read_{{ParamType}}_array(current_in, result->{{CName}}, {{Size}});\n">>,
      Map = #{"ParamType"=>element(2,TypeDescr), "CName"=>CName, "Size"=>element(3,TypeDescr)};
    string ->
      Line = <<"\tcurrent_in = read_{{ParamType}}(current_in, (string *) result->{{CName}});\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName};
    pointer ->
      Line = <<"\tcurrent_in = read_{{ParamType}}(current_in, (void **) &(result->{{CName}}));\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName};
    _ ->
      Line = <<"\tcurrent_in = read_{{ParamType}}(current_in, &(result->{{CName}}));\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName}
  end,
  NewLine = bbmustache:render(Line, Map),
  generate_read_struct_c(MemberList, <<Result/binary, NewLine/binary>>).

%% -----------------------------------------------------------------------------
%  Genera el código C para la función write_struct que serializa un tipo de dato
%  struct. Devuelve el código generado en forma de bitstring.
generate_write_struct_c(MemberList) ->
  InitFun = <<"byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
              "\tbyte *current_out = out;\n\n">>,
  generate_write_struct_c(MemberList, InitFun).
generate_write_struct_c([], Result) ->
  EndFun = <<"\n\treturn current_out;\n}\n\n">>,
  <<Result/binary, EndFun/binary>>;
generate_write_struct_c([M|MemberList], Result) ->
  #struct_member{c_name=CName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  case Type of
    fixed_array ->
      Line = <<"\tcurrent_out = write_{{ParamType}}_array(value->{{CName}}, current_out, len, {{Size}});\n">>,
      Map = #{"ParamType"=>element(2,TypeDescr), "CName"=>CName, "Size"=>element(3,TypeDescr)};
    string ->
      Line = <<"\tcurrent_out = write_{{ParamType}}((string *) value->{{CName}}, current_out, len);\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName};
    pointer ->
      Line = <<"\tcurrent_out = write_{{ParamType}}((void **) &(value->{{CName}}), current_out, len);\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName};
    _ ->
      Line = <<"\tcurrent_out = write_{{ParamType}}(&(value->{{CName}}), current_out, len);\n">>,
      Map = #{"ParamType"=>Type, "CName"=>CName}
  end,
  NewLine = bbmustache:render(Line, Map),
  generate_write_struct_c(MemberList, <<Result/binary, NewLine/binary>>).

%% -----------------------------------------------------------------------------
%  Genera el código C para las funciones get y set de cada elemento de un tipo
%  struct. Devuelve el código generado en forma de bitstring.
generate_getters_setters_c(MemberList, StructErlName, StructCName) ->
  generate_getters_setters_c(MemberList, StructErlName, StructCName, <<"">>).
generate_getters_setters_c([], _StructErlName, _StructCName, Result) -> Result;
generate_getters_setters_c([M|MemberList], StructErlName, StructCName, Result) ->
  #struct_member{erlang_name=AttribErl, c_name=AttribC, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> Type = element(1, TypeDescr);
    false -> Type = TypeDescr
  end,
  % El código de las funciones get y set varía en función de si el tipo de dato
  % es de tipo fixed_array, string, puntero u otra cosa.
  case Type of
    fixed_array ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
              "\tcurrent_out = write_{{TypeErl}}_array(ptr->{{AttribC}}, current_out, len_out, {{Size}});\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_in = read_{{TypeErl}}_array(current_in, ptr->{{AttribC}}, {{Size}});\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n}\n\n">>,
      TypeErl = element(2, TypeDescr),
      Size = element(3, TypeDescr);
    string ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
              "\tcurrent_out = write_{{TypeErl}}((string *) ptr->{{AttribC}}, current_out, len_out);\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_in = read_{{TypeErl}}(current_in, (string *) ptr->{{AttribC}});\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n}\n\n">>,
      TypeErl = Type,
      Size = undefined;
    pointer ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
              "\tcurrent_out = write_{{TypeErl}}((void **) &(ptr->{{AttribC}}), current_out, len_out);\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_in = read_{{TypeErl}}(current_in, (void **) &(ptr->{{AttribC}}));\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n}\n\n">>,
      TypeErl = Type,
      Size = undefined;
    _ ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
              "\tcurrent_out = write_{{TypeErl}}(&(ptr->{{AttribC}}), current_out, len_out);\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\tcurrent_in = read_{{TypeErl}}(current_in, &(ptr->{{AttribC}}));\n"
              "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n}\n\n">>,
      TypeErl = Type,
      Size = undefined
  end,

  add_fun_code(StructErlName++"_get_"++atom_to_list(AttribErl)++"_Handler"),
  add_fun_code(StructErlName++"_set_"++atom_to_list(AttribErl)++"_Handler"),
  Map = #{"StructErl" => StructErlName,
          "StructC" => StructCName,
          "AttribErl" => AttribErl,
          "AttribC" => AttribC,
          "TypeErl" => TypeErl,
          "Size" => Size},
  NewLines = bbmustache:render(<<Get/binary, Set/binary>>, Map),
  generate_getters_setters_c(MemberList, StructErlName, StructCName, <<Result/binary, NewLines/binary>>).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero C (.c) el código generado para las funciones definidas
%  en el fichero de especificación.
generate_functions_c([]) ->
  Array = generate_fun_array(get_fun_list()),
  write_file(get_c_filename(), binary_to_list(Array)++?SEPARATOR_C, [append]),
  ok;
generate_functions_c([F|FunList]) ->
  #fun_spec{erlang_name=ErlName, c_name=CName, params=Params, type_descr=Descr} = F,
  HandlerName = atom_to_list(ErlName)++"_Handler",
  InitLines = <<"void {{HandlerName}}(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n">>,
  {ReadLines, Vars, RetVars, FunVars} = generate_fun_params_c(Params, atom_to_list(ErlName)),
  case Descr of
    void ->
      RetType = nil,
      FunLine = <<"\t{{CName}}({{FunVars}});\n"
                  "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n">>;
    string ->
      RetType = string,
      FunLine = <<"\t{{RetCType}} retvar;\n"
                  "\tstrcpy((char *)retvar, {{CName}}({{FunVars}}));\n"
                  "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
                  "\tcurrent_out = write_{{RetType}}(&retvar, current_out, len_out);\n">>;
    {pointer, Type} ->
      RetType = Type,
      FunLine = <<"\t{{RetCType}} *retvar = {{CName}}({{FunVars}});\n"
                  "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
                  "\tcurrent_out = write_pointer((void **) &retvar, current_out, len_out);\n">>;
    Type ->
      RetType = Type,
      FunLine = <<"\t{{RetCType}} retvar = {{CName}}({{FunVars}});\n"
                  "\tcurrent_out = write_byte(RET_CODE, current_out, len_out);\n"
                  "\tcurrent_out = write_{{RetType}}(&retvar, current_out, len_out);\n">>
  end,
  ReturnLines = generate_fun_return_params_c(RetVars),
  FinalLine = <<"}\n\n">>,
  Map = #{"CName" => CName,
          "HandlerName" => HandlerName,
          "FunVars" => string:join(Vars, ", "),
          "RetCType" => get_type_name(RetType),
          "RetType" => atom_to_list(RetType)},
  Content = <<InitLines/binary, ReadLines/binary, FunLine/binary, ReturnLines/binary, FinalLine/binary>>,
  generate_fun_wrappers_c(FunVars),
  write_file(get_c_filename(), binary_to_list(bbmustache:render(Content, Map)), [append]),
  add_fun_code(HandlerName),
  generate_functions_c(FunList).

%% -----------------------------------------------------------------------------
%  Genera a partir de la lista de parámetros de una función una tupla que contiene:
%    -La parte del código de la función correspondiente a la deserialización de
%     parámetros en forma de bitstring.
%    -Una lista con los nombres de variable generados para cada parámetro.
%    -Una lista de tuplas con los nombres de los parámetros de salida y su tipo.
%    -Una lista de tuplas con infomación de los parámetros funcionales (si los hay):
%       -Nombre del parámetros funcional.
%       -Número de parámetro en fucnión de su posición en la cabecera de la función.
%       -Tupla con los tipos que recibe la función pasada como parámetro.
%       -Tipo devuelto por la funcion pasada como parámetro
%  Toda esta información es utilizada en la generación del código de las funciones de C.
generate_fun_params_c([], _FunName) ->
  {<<"">>, [], [], []};
generate_fun_params_c(Params, FunName) ->
  generate_fun_params_c(Params, FunName, <<"">>, [], [], [], 1).
generate_fun_params_c([], _FunName, ReadLines, Vars, RetVars, FunVars, _Cnt) ->
  Line = <<"\n">>,
  {<<ReadLines/binary, Line/binary>>, lists:reverse(Vars), lists:reverse(RetVars), lists:reverse(FunVars)};
generate_fun_params_c([{_,T,O}|Params], FunName, ReadLines, Vars, RetVars, FunVars, Cnt) ->
  Return = lists:member(return, O),
  case T of
    {function, PTypes, RType} ->
      FunId = length(FunVars)+1,
      VarName = FunName++"_wrapper_"++integer_to_list(FunId),
      Lines = <<"">>,
      Map = maps:new(),
      NewRetVars = RetVars,
      NewFunVars = [{VarName, FunId, PTypes, RType}|FunVars];
    {pointer, ErlType} when Return ->
      VarName = "var"++integer_to_list(Cnt),
      Lines = <<"\t{{CType}} *{{VarName}} = malloc(sizeof({{CType}}));\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName},
      NewRetVars = [{VarName, ErlType}|RetVars],
      NewFunVars = FunVars;
    {pointer, ErlType} ->
      VarName = "var"++integer_to_list(Cnt),
      Lines = <<"\t{{CType}} *{{VarName}};\n"
                "\tcurrent_in = read_pointer(current_in, (void **) &{{VarName}});\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName},
      NewRetVars = RetVars,
      NewFunVars = FunVars;
    ErlType ->
      VarName = "var"++integer_to_list(Cnt),
      Lines = <<"\t{{CType}} {{VarName}};\n"
                "\tcurrent_in = read_{{ReadType}}(current_in, &{{VarName}});\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName, "ReadType"=>ErlType},
      NewRetVars = RetVars,
      NewFunVars = FunVars
  end,
  NewLines = bbmustache:render(Lines, Map),
  generate_fun_params_c(Params, FunName, <<ReadLines/binary, NewLines/binary>>, [VarName|Vars], NewRetVars, NewFunVars, Cnt+1).

%% -----------------------------------------------------------------------------
%  Genera la parte del código de la funcion de C correspondiente a la serialización
%  de parámetros de salida. Devuelve el código generado en forma de bitstring.
generate_fun_return_params_c(RetVars) ->
  generate_fun_return_params_c(RetVars, <<"">>).
generate_fun_return_params_c([], Result) -> Result;
generate_fun_return_params_c([{VN, VT}|RetVars], Result) ->
  case VT of
    string ->
      Line = <<"\tcurrent_out = write_{{Type}}(&{{Name}}, current_out, len_out);\n">>;
    _ ->
      Line = <<"\tcurrent_out = write_{{Type}}({{Name}}, current_out, len_out);\n"
              "\tfree({{Name}});\n">>
  end,
  Map = #{"Type"=>atom_to_list(VT), "Name"=>VN},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_return_params_c(RetVars, <<Result/binary, NewLine/binary>>).

%% -----------------------------------------------------------------------------
%  Genera la parte del código C correspondiente a los wrappers necesarios para la
%  ejecución de fucniones de orden superior. Devuelve el código de las funciones
%  wrapper para cada parámetro funcional en formato bitstring.
generate_fun_wrappers_c(FunVars) ->
  generate_fun_wrappers_c(FunVars, <<"">>).
generate_fun_wrappers_c([], Result) ->
  write_file(get_c_filename(), Result, [append]);
generate_fun_wrappers_c([{FunName, FunId, PTypes, RType}|FunVars], Result) ->
  FunCode = <<"{{RetType}} {{FunName}} ({{Args}}) {\n"
              "\tstatic byte input_buffer[BUF_SIZE];\n"
              "\tstatic byte output_buffer[BUF_SIZE];\n"
              "\tbyte *current_out = output_buffer;\n"
              "\tsize_t len_out = 0;\n\n"
              "\tcurrent_out = write_byte(CALL_CODE, current_out, &len_out);\n"
              "\tcurrent_out = write_byte({{FunId}}, current_out, &len_out);\n"
              "{{{WriteArgs}}}\n"
              "\twrite_command(output_buffer, len_out);\n\n"
              "\twait_for_input(input_buffer, output_buffer);\n\n"
              "\tbyte *current_in = input_buffer;\n"
              "\tcurrent_in++;\n"
              "\t{{RetType}} result;\n"
              "\tcurrent_in = read_{{ErlRetType}}(current_in, &result);\n\n"
              "\treturn result;\n}\n\n">>,
  Args = [get_type_name(lists:nth(Id, PTypes))++" param"++integer_to_list(Id) || Id<-lists:seq(1, length(PTypes))],
  WriteArgs = ["\tcurrent_out = write_"++atom_to_list(lists:nth(Id, PTypes))++"(&param"++integer_to_list(Id)++", current_out, &len_out);\n"
                || Id<-lists:seq(1, length(PTypes))],
  Map = #{"RetType" => get_type_name(RType),
          "FunName" => FunName,
          "Args" => string:join(Args, ", "),
          "FunId" => integer_to_list(FunId),
          "ErlRetType" => RType,
          "WriteArgs" => string:join(WriteArgs, "")},
  NewFunCode = bbmustache:render(FunCode, Map),
  generate_fun_wrappers_c(FunVars, <<Result/binary, NewFunCode/binary>>).

%% -----------------------------------------------------------------------------
%  Genera la parte del código C correspondiente al array que contiene las referencias
%  a las funciones handler generadas. Devuelve el código generado en forma de bitstring.
generate_fun_array(FunList) ->
  Line = <<"handler handlers[] = {\n\t0,\n">>,
  generate_fun_array(FunList, Line).
generate_fun_array([{Code,Name}|[]], Result) ->
  Line = <<"\t{{Name}}\t\t//{{Code}}\n};\n\n">>,
  Map = #{"Name"=>Name, "Code"=>integer_to_list(Code)},
  NewLine = bbmustache:render(Line, Map),
  <<Result/binary, NewLine/binary>>;
generate_fun_array([{Code,Name}|FunList], Result) ->
  Line = <<"\t{{Name}},\t\t//{{Code}}\n">>,
  Map = #{"Name"=>Name, "Code"=>integer_to_list(Code)},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_array(FunList, <<Result/binary, NewLine/binary>>).

%% -----------------------------------------------------------------------------
%  Escribe en el fichero C (.c) el código de la función main y de las funciones
%  para la gestión de comunicaciones Erlang-C.
generate_main_c() ->
  {ok, File} = file:read_file("resources/main_funcs.c"),
  Content = unicode:characters_to_list(File),
  write_file(get_c_filename(), Content, [append]).

%% -----------------------------------------------------------------------------
%  Obtiene la especificación de un tipo de dato a partir de su nombre en Erlang.
find_type_spec_by_erlname(_Type, []) -> undefined;
find_type_spec_by_erlname(Type, [TS|TypeSpecList]) ->
  {_,Name,_,_,_} = TS,
  case Type==Name of
    true -> TS;
    false -> find_type_spec_by_erlname(Type, TypeSpecList)
  end.

%% -----------------------------------------------------------------------------
%  Obtiene la especificación de un tipo de dato a partir de su nombre en C.
% find_type_spec_by_cname(_Type, []) -> undefined;
% find_type_spec_by_cname(Type, [TS|TypeSpecList]) ->
%   {_,_,Name,_,_} = TS,
%   case Type==Name of
%     true -> TS;
%     false -> find_type_spec_by_cname(Type, TypeSpecList)
%   end.

%% -----------------------------------------------------------------------------
%  Escribe en un fichero "File" el contenido "Content" con las opciones "Modes".
write_file(File, Content, Modes) ->
  case file:write_file(File, Content, Modes) of
    ok -> ok;
    {error, Reason} -> io:format("Write error: ~p~n",[Reason])
  end.

%% -----------------------------------------------------------------------------
%  Lee el contenido de un fichero "File".
read_file(File) ->
  case file:read_file(File) of
    {ok, Content} -> Content;
    {error, Error} ->
      io:format("Error reading file: ~p~n", [Error]),
      <<"">>
  end.

%% -----------------------------------------------------------------------------
%  Bucle del proceso auxiliar que se encarga de mantener la siguiente información
%  global para realizar consultas:
%    -La información de la tupla "generator_info" del fichero de especificación.
%    -Código de operación para funciones que se va incrementando por cada
%     función generada.
%    -Lista de nombres de funciones con sus códigos de operación.
%    -Map con la correspondencia entre nombre de funcion en Erlang y C.
generator_helper(InitCode, Info) ->
  generator_helper(InitCode, Info, [], maps:new()).
generator_helper(Code, Info, FunList, TypeMap) ->
  receive
    {Pid, {fun_code, NameFun}} ->
      Pid ! {fun_code, Code},
      generator_helper(Code+1, Info, FunList++[{Code, NameFun}], TypeMap);
    {_Pid, {fun_add, NameFun}} ->
      generator_helper(Code+1, Info, FunList++[{Code, NameFun}], TypeMap);
    {Pid, fun_list} ->
      Pid ! {fun_list, FunList},
      generator_helper(Code, Info, FunList, TypeMap);
    {_Pid, {reset, InitCode}} ->
      generator_helper(InitCode, Info, [], maps:new());
    {_Pid, {new_type, ErlName, CName}} ->
      generator_helper(Code, Info, FunList, maps:put(ErlName, CName, TypeMap));
    {Pid, {get_type, ErlName}} ->
      Pid ! {type_name, maps:get(ErlName, TypeMap, "")},
      generator_helper(Code, Info, FunList, TypeMap);
    {Pid, erl_filename} ->
      Pid ! {erl_filename, Info#generator_info.erl_file_gen},
      generator_helper(Code, Info, FunList, TypeMap);
    {Pid, hrl_filename} ->
      Pid ! {hrl_filename, Info#generator_info.hrl_file_gen},
      generator_helper(Code, Info, FunList, TypeMap);
    {Pid, c_filename} ->
      Pid ! {c_filename, Info#generator_info.c_file_gen},
      generator_helper(Code, Info, FunList, TypeMap);
    {_Pid, exit} ->
      ok;
    _ ->
      generator_helper(Code, Info, FunList, TypeMap)
  end.

%% -----------------------------------------------------------------------------
%  Función para solicitar al proceso generator_helper la asignación de un código de
%  operación para la función "NameFun" y obtener el código asignado.
get_fun_code(NameFun) ->
  generator_helper ! {self(), {fun_code, NameFun}},
  receive
    {fun_code, Code} -> Code
  end.

%% -----------------------------------------------------------------------------
%  Función para solicitar al proceso generator_helper la asignación de un código de
%  operación para la función "NameFun".
add_fun_code(NameFun) ->
  generator_helper ! {self(), {fun_add, NameFun}}.

%% -----------------------------------------------------------------------------
%  Función para obtener del proceso generator_helper la lista de funciones con sus
%  códigos de operación.
get_fun_list() ->
  generator_helper ! {self(), fun_list},
  receive
    {fun_list, FunList} -> FunList
  end.

%% -----------------------------------------------------------------------------
%  Función para solicitar al proceso generator_helper reiniciar la secuencia de
%  códigos de operación comenzando por el número "InitCode".
reset_fun_code(InitCode) ->
  generator_helper ! {self(), {reset, InitCode}}.

%% -----------------------------------------------------------------------------
%  Función para indicar al proceso generator_helper la correspondencia entre los
%  nombre de una una función en Erlang y C.
add_type_name(ErlName, CName) ->
  generator_helper ! {self(), {new_type, ErlName, CName}}.

%% -----------------------------------------------------------------------------
%  Función para obtener del proceso generator_helper el nombre de una función en C
%  dado su nombre en Erlang.
get_type_name(ErlName) ->
  generator_helper ! {self(), {get_type, ErlName}},
  receive
    {type_name, CName} -> CName
  end.

%% -----------------------------------------------------------------------------
%  Función para obtener del proceso generator_helper el nombre del fichero fuente
%  de Erlang (.erl) generado.
get_erl_filename() ->
  generator_helper ! {self(), erl_filename},
  receive
    {erl_filename, ErlFileName} -> ErlFileName
  end.

%% -----------------------------------------------------------------------------
%  Función para obtener del proceso generator_helper el nombre del fichero de cabecera
%  de Erlang (.hrl) generado.
get_hrl_filename() ->
  generator_helper ! {self(), hrl_filename},
  receive
    {hrl_filename, HrlFileName} -> HrlFileName
  end.

%% -----------------------------------------------------------------------------
%  Función para obtener del proceso generator_helper el nombre del fichero fuente
%  de C (.c) generado.
get_c_filename() ->
  generator_helper ! {self(), c_filename},
  receive
    {c_filename, CFileName} -> CFileName
  end.

%% -----------------------------------------------------------------------------
%  Función para terminar el proceso generator_helper.
end_helper() ->
  generator_helper ! {self(), exit}.
