-module(sdl_generator).

-export([generate_code/0, generate_code/1]).

-import(bbmustache, [render/2]).

-record(macro_spec, {name, value}).
-record(type_spec, {erlang_name, c_name, type_descr, option}).
-record(fun_spec, {erlang_name, c_name, params, type_descr, option}).
-record(struct_member, {erlang_name, c_name, type_descr, option}).
-record(param_spec, {type_descr, option}).

-define(ERL_FILENAME, "sdl_ports_gen.erl").
-define(C_FILENAME, "sdl_ports_gen.c").
-define(C_HANDLER, "sdl_handler").
-define(MODULE_NAME, "sdl_ports_gen").
-define(PORT_NAME, sdl_port).   % atom
-define(SEPARATOR, "%--------------------------------------------------------\n\n").

generate_code() ->
  generate_code("sdl_spec_test.txt").

generate_code(Filename) ->
  case file:open(Filename, [read]) of
    {ok, InputDevice} ->
      {ok, {spec, MacroList, TypeList, FunList}} = io:read(InputDevice, ""),
      file:close(InputDevice),
      file:delete(?ERL_FILENAME),
      write_file(?ERL_FILENAME, "-module("++?MODULE_NAME++").\n", [append]),
      write_file(?ERL_FILENAME, "-compile(export_all).\n\n", [append]),
      generate_export(FunList),
      generate_macros(MacroList),
      generate_init_port(?PORT_NAME),
      generate_native_types_parser_erl(),
      generate_types_parser_erl(TypeList),
      generate_functions_erl(FunList);
      %io:format("Macros: ~p~n", [MacroList]),
      %io:format("Types: ~p~n", [TypeList]),
      %io:format("Funs: ~p~n", [FunList]);
    {error, Error} ->
      io:format("Error reading file: ~p~n", [Error])
  end,
  ok.

generate_export(FunList) ->
  generate_export(FunList, []).
generate_export([], Result) ->
  Line = "-export(["++string:join(lists:reverse(Result), ", ")++"]).\n\n",
  write_file(?ERL_FILENAME, Line, [append]);
generate_export([{_,Name,_,Params,_,_}|FList], Result) ->
  P = [ T || {_,T,O} <- Params, not lists:member(return, O) ],
  Elem = atom_to_list(Name)++"/"++integer_to_list(length(P)),
  generate_export(FList, [Elem|Result]).

generate_macros([]) ->
  case file:write_file(?ERL_FILENAME, "\n\n", [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end;
generate_macros([Macro|MacroList]) ->
  %io:format("Macro spec: ~p~n", [Macro]),
  Line = "-define(" ++ Macro#macro_spec.name ++ ", " ++ Macro#macro_spec.value ++ ").\n",
  case file:write_file(?ERL_FILENAME, Line, [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end,
  generate_macros(MacroList).

generate_init_port(PortName) ->
  % Init = "init_port() ->\n"
  %         "\tPort = open_port({spawn, \"./" ++ ?C_HANDLER ++ "\"}, [{packet, 2}]),\n"
  %         "\tregister("++atom_to_list(PortName)++", Port), Port.\n\n",
  Init = bbmustache:render(<<"init_port() ->\n"
          "\tPort = open_port({spawn, \"./{{H}}\"}, [{packet, 2}]),\n"
          "\tregister({{P}}, Port), Port.\n\n">>, #{"H" => ?C_HANDLER, "P" => atom_to_list(PortName)}),
  case file:write_file(?ERL_FILENAME, binary_to_list(Init)++?SEPARATOR, [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end.

generate_native_types_parser_erl() ->
  % int
  IntType = "int_to_bytelist(Value) ->\n"
              "\tint_to_bytelist(Value, 32).\n\n"
              "int_to_bytelist(Value, NBits) ->\n"
              "\tbinary:bin_to_list(<< Value:NBits >>).\n\n"
              "bytelist_to_int(Bytelist) ->\n"
              "\tbytelist_to_int(Bytelist, 32).\n\n"
              "bytelist_to_int(Bytelist, NBits) ->\n"
              "\t<< Value : NBits >> = binary:list_to_bin(Bytelist), Value.\n\n"
              "parse_int(Bytelist) ->\n"
              "\tparse_int(Bytelist, 32).\n\n"
              "parse_int(Bytelist, NBytes) ->\n"
              "\tparse_int(Bytelist, NBytes, NBytes, []).\n"
              "parse_int([B|Rest], NBytes, Cnt, Result) when Cnt>0 ->\n"
              "\tparse_int(Rest, NBytes, Cnt-8, [B|Result]);\n"
              "parse_int(Bytelist, NBytes, _Cnt, Result) ->\n"
              "\t{bytelist_to_int(lists:reverse(Result), NBytes), Bytelist}.\n\n",
  % float
  FloatType = "float_to_bytelist(Value) ->\n"
              "\tfloat_to_bytelist(Value, 32).\n\n"
              "float_to_bytelist(Value, NBits) ->\n"
              "\tbinary:bin_to_list(<< Value:NBits/float >>).\n\n"
              "bytelist_to_float(Bytelist) ->\n"
              "\tbytelist_to_float(Bytelist, 32).\n\n"
              "bytelist_to_float(Bytelist, NBits) ->\n"
              "\t<< Value : NBits/float >> = binary:list_to_bin(Bytelist), Value.\n\n"
              "parse_float(Bytelist) ->\n"
              "\tparse_float(Bytelist, 32).\n\n"
              "parse_float(Bytelist, NBytes) ->\n"
              "\tparse_float(Bytelist, NBytes, NBytes, []).\n"
              "parse_float([B|Rest], NBytes, Cnt, Result) when Cnt>0 ->\n"
              "\tparse_float(Rest, NBytes, Cnt-8, [B|Result]);\n"
              "parse_float(Bytelist, NBytes, _Cnt, Result) ->\n"
              "\t{bytelist_to_float(lists:reverse(Result), NBytes), Bytelist}.\n\n",
  % double
  DoubleType = "double_to_bytelist(Value) ->\n"
              "\tfloat_to_bytelist(Value, 64).\n\n"
              "bytelist_to_double(Bytelist) ->\n"
              "\tbytelist_to_float(Bytelist, 64).\n\n"
              "parse_double(Bytelist) ->\n"
              "\tparse_float(Bytelist, 64).\n\n",
  % string
  StringType = "string_to_bytelist(Value, Encoding) ->\n"
              "\tbinary:bin_to_list(unicode:characters_to_binary(Value, Encoding))++[$\\0].\n\n"
              "bytelist_to_string(Bytelist, Encoding) ->\n"
              "\tunicode:characters_to_list(binary:list_to_bin(Bytelist), Encoding).\n\n"
              "string_to_bytelist(Value) ->\n"
              "\tstring_to_bytelist(Value, utf8).\n\n"
              "bytelist_to_string(Bytelist) ->\n"
              "\tbytelist_to_string(Bytelist, utf8).\n\n"
              "parse_string(Bytelist) ->\n"
              "\tparse_string(Bytelist, []).\n"
              "parse_string([$\\0|Rest], Result) ->\n"
              "\t{bytelist_to_string(lists:reverse(Result)), Rest};\n"
              "parse_string([B|Rest], Result) ->\n"
              "\tparse_string(Rest, [B|Result]).\n\n",
  % pointer
  PointerType = "pointer_to_bytelist(Value) ->\n"
              "\tint_to_bytelist(Value, 64).\n\n"
              "bytelist_to_pointer(Bytelist) ->\n"
              "\tbytelist_to_int(Bytelist, 64).\n\n"
              "parse_pointer(Bytelist) ->\n"
              "\tparse_int(Bytelist, 64).\n\n",

  write_file(?ERL_FILENAME, IntType++FloatType++DoubleType++StringType++PointerType++?SEPARATOR, [append]).

generate_types_parser_erl([]) ->
  write_file("sdl_ports_gen.erl", ?SEPARATOR, [append]);
generate_types_parser_erl([TypeSpec|TypeList]) ->
  %io:format("Type spec: ~p~n", [Type]),
  #type_spec{erlang_name=ErlName, c_name=CName, type_descr=TypeDescr, option=Opt} = TypeSpec,
  if
    is_tuple(TypeDescr) -> {Type, Desc} = TypeDescr;
    true -> Type = TypeDescr, Desc = nil
  end,
  ContentMap = #{"ErlName"=>ErlName, "Type"=>Type, "Desc"=>Desc},

  case TypeDescr of
    {struct, opaque} ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\tpointer_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_pointer(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_pointer(Bytelist).\n\n">>;
    {struct, MemberList} ->
      Args = string:join([atom_to_list(A) || {_,A,_,_,_} <- Desc], ", "),
      FinalMap = maps:put("Args", Args, ContentMap),
      ContentPart1 = <<"-record({{ErlName}}, {{{{Args}}}}).\n\n">>,
      ContentPart2 = generate_struct_to_bytelist(MemberList, ErlName),
      ContentPart3 = generate_bytelist_to_struct(MemberList, ErlName),
      ContentPart4 = generate_parse_struct(MemberList, ErlName),
      % TODO falta el metodo parse_struct(Bytelist) y el pointer_deref_XXX
      Content = <<ContentPart1/binary, ContentPart2/binary, ContentPart3/binary, ContentPart4/binary>>;
    {enum, _ElemList} ->
      FinalMap = ContentMap,
      Content = <<"">>; % De momento no
    {array, _TD} ->
      FinalMap = ContentMap,
      Content = <<"">>; % De momento no
    {string, _Cod} ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\tstring_to_bytelist(Value, {{Desc}}).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_string(Bytelist, {{Desc}}).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_string(Bytelist).\n\n">>;
    {pointer, _TD} ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\tpointer_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_pointer(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_pointer(Bytelist).\n\n"
                "pointer_deref_{{ErlName}}(Pointer) ->\n"
                "\tPList = pointer_to_bytelist(Pointer)\n"
                "\tsdl_port ! {self(), {command, [1|PList]}},\n"
                "\treceive\n"
                "\t\t{ _, { data, DataList }} ->\n"
                "\t\t\tbytelist_to_{{Desc}}(DataList);\n"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    {T, _NBits} when (T==int) or (T==float) ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\t{{Type}}_to_bytelist(Value, {{Desc}}).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_{{Type}}(Bytelist, {{Desc}}).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_{{Type}}(Bytelist, {{Desc}}).\n\n">>;
    T when (T==int) or (T==float) or (T==double) or (T==string) or (T==pointer) ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\t{{Type}}_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_{{Type}}(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_{{Type}}(Bytelist).\n\n">>;
    ErlangName ->
      FinalMap = ContentMap,
      Content = <<"">>
  end,
  write_file("sdl_ports_gen.erl", binary_to_list(bbmustache:render(Content,FinalMap)), [append]),
  generate_types_parser_erl(TypeList).

generate_struct_to_bytelist(Members, StructName) ->
  Init = <<"{{ErlName}}_to_bytelist(Value) ->\n"
          "\tReturn = [">>,
  generate_struct_to_bytelist(Members, StructName, Init).
generate_struct_to_bytelist([M|[]], StructName, Result) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  Final = bbmustache:render(<<"{{TD}}_to_bytelist(Value#{{SN}}.{{EN}})],\n"
                              "\tlists:flatten(Return).\n\n">>,
                            #{"TD"=>Type, "SN"=>StructName, "EN"=>ErlName}),
  <<Result/binary, Final/binary>>;
generate_struct_to_bytelist([M|Members], StructName, Result) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  Line = bbmustache:render(<<"{{TD}}_to_bytelist(Value#{{SN}}.{{EN}}),\n\t">>, #{"TD"=>Type, "SN"=>StructName, "EN"=>ErlName}),
  generate_struct_to_bytelist(Members, StructName, <<Result/binary, Line/binary>>).

generate_bytelist_to_struct(Members, StructName) ->
  InitLines = <<"bytelist_to_{{ErlName}}(Bytelist) ->\n"
          "\tR0 = Bytelist,\n">>,
  generate_bytelist_to_struct(Members, [], StructName, InitLines, 1).
generate_bytelist_to_struct([], MemberNames, StructName, Result, Cnt) ->
  MNames = string:join(lists:reverse(MemberNames), ", "),
  Line = bbmustache:render(<<"\t#{{SN}}{{{{M}}}}.\n\n">>, #{"SN"=>StructName, "M"=>MNames}),
  <<Result/binary, Line/binary>>;
generate_bytelist_to_struct([M|Members], MemberNames, StructName, Result, Cnt) ->
  #struct_member{erlang_name=ErlName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  MName = atom_to_list(ErlName)++"="++string:titlecase(atom_to_list(ErlName)),
  LineMap = #{"TD"=>Type, "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1},
  Line = bbmustache:render(<<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}(R{{CntPrev}}),\n">>, LineMap),
  generate_bytelist_to_struct(Members, [MName|MemberNames], StructName, <<Result/binary, Line/binary>>, Cnt+1).

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
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  MName = atom_to_list(ErlName)++"="++string:titlecase(atom_to_list(ErlName)),
  LineMap = #{"TD"=>Type, "EN"=>string:titlecase(atom_to_list(ErlName)), "Cnt"=>Cnt, "CntPrev"=>Cnt-1},
  Line = bbmustache:render(<<"\t{{{EN}}, R{{Cnt}}} = parse_{{TD}}(R{{CntPrev}}),\n">>, LineMap),
  generate_parse_struct(Members, [MName|MemberNames], StructName, <<Result/binary, Line/binary>>, Cnt+1).

generate_functions_erl(FunList) ->
  generate_functions_erl(FunList, 1).
generate_functions_erl([], _Cnt) -> ok;
generate_functions_erl([Fun|FunList], Cnt) ->
  #fun_spec{erlang_name=ErlName, c_name=CName, params=Params, type_descr=Descr, option=Opt} = Fun,
  StrParams = fun_params_to_string(Params),
  VarList = case ["Param"++integer_to_list(I) || I <- lists:seq(1,length(StrParams))] of
              [] -> ["[]"];
              List -> List
            end,
  Header = <<"{{ErlName}}({{HParams}}) ->\n">>,
  Body1 = <<"{{BodyParams}}"
            "\t{{Port}} ! {self(), {command, [{{Code}}, {{Vars}}]}},\n">>,
        % TODO Cambiar el envio de parametros encadenados con ++ por listas anidadas
  case Descr of
    {pointer, Type} -> RetType = pointer;
    _ -> RetType = Descr
  end,
  case RetType of
    void ->
      Body2 = <<"\treceive\n"
                "\t\t{_, {data, []}} ->\n"
                "\t\t\tok;\n"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    _ ->
      Body2 = <<"\treceive\n"
                "\t\t{_, {data, DataList}} ->\n"
                "\t\t\tbytelist_to_{{RetType}}(DataList);\n"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>
        % TODO Modificar el return para funciones con parametros por referencia
  end,

  Map = #{"ErlName"=>atom_to_list(ErlName),
          "HParams"=>string:join(StrParams, ", "),
          "BodyParams"=>generate_body_parameters_to_send(Params, StrParams),
          "Port"=>atom_to_list(?PORT_NAME),
          "Code"=>integer_to_list(Cnt),
          "Vars"=>string:join(VarList,", "),
          "RetType"=>atom_to_list(RetType)},
  Content = <<Header/binary, Body1/binary, Body2/binary>>,
  write_file(?ERL_FILENAME, binary_to_list(bbmustache:render(Content, Map)), [append]),
  generate_functions_erl(FunList, Cnt+1).

fun_params_to_string(Params) ->
  fun_params_to_string(Params, "", 1).
fun_params_to_string([], Result, _Cnt) -> lists:reverse(Result);
fun_params_to_string([{_,P,O}|Params], Result, Cnt) ->
  case lists:member(return, O) of
    true ->
      NewResult = Result;
    _ ->
      case P of
        {pointer, Type} ->
          Elem = "P_"++string:titlecase(atom_to_list(Type)) ++ "_" ++ integer_to_list(Cnt),
          NewResult = [Elem|Result];
        _ ->
          Elem = string:titlecase(atom_to_list(P)) ++ "_" ++ integer_to_list(Cnt),
          NewResult = [Elem|Result]
      end
  end,

  % fun_params_to_string(Params, Result++[Elem], Cnt+1).
  fun_params_to_string(Params, NewResult, Cnt+1).

generate_body_parameters_to_send(Params, StrParams) ->
  generate_body_parameters_to_send(Params, StrParams, "", 1).
generate_body_parameters_to_send(_Params, [], Result, Cnt) -> Result;
generate_body_parameters_to_send([{_,T,O}|Params], [S|StrParams], Result, Cnt) ->
  case lists:member(return,O) of
    true ->
      Line = "";
    _ ->
      case T of
        {pointer, Type} ->
          Line = "\tParam" ++ integer_to_list(Cnt) ++ " = pointer_to_bytelist("++S++"),\n";
        _ ->
          Line = "\tParam" ++ integer_to_list(Cnt) ++ " = " ++ atom_to_list(T) ++ "_to_bytelist("++S++"),\n"
      end
  end,
  generate_body_parameters_to_send(Params, StrParams, Result++Line, Cnt+1).

find_type_spec_by_erlname(_Type, []) -> nil;
find_type_spec_by_erlname(Type, [TS|TypeSpecList]) ->
  {_,Name,_,_,_} = TS,
  case Type==Name of
    true -> TS;
    false -> find_type_spec_by_erlname(Type, TypeSpecList)
  end.

find_type_spec_by_cname(_Type, []) -> nil;
find_type_spec_by_cname(Type, [TS|TypeSpecList]) ->
  {_,_,Name,_,_} = TS,
  case Type==Name of
    true -> TS;
    false -> find_type_spec_by_cname(Type, TypeSpecList)
  end.

write_file(File, Content, Modes) ->
  case file:write_file(File, Content, Modes) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end.
