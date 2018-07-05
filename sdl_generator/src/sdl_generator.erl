-module(sdl_generator).

-export([main/1, generate_code/0, generate_code/1, sdl_helper/0]).

-import(bbmustache, [render/2]).

-record(macro_spec, {name, value}).
-record(type_spec, {erlang_name, c_name, type_descr, option}).
-record(fun_spec, {erlang_name, c_name, params, type_descr, option}).
-record(struct_member, {erlang_name, c_name, type_descr, option}).
%-record(param_spec, {type_descr, option}).

-define(ERL_FILENAME, "sdl_ports_gen.erl").
-define(HRL_FILENAME, "sdl_ports_gen.hrl").
-define(C_FILENAME, "sdl_ports_gen.c").
-define(C_HANDLER, "_checkouts/sdl_generator/sdl_ports_gen").
-define(MODULE_NAME, "sdl_ports_gen").
-define(PORT_NAME, sdl_port).   % atom
-define(C_LIB_IMPORT, "SDL2/SDL.h").
-define(SEPARATOR_ERL, "%--------------------------------------------------------\n\n").
-define(SEPARATOR_C, "//--------------------------------------------------------\n\n").

%% escript Entry point
main(_Args) ->
    generate_code(),
    erlang:halt(0).

generate_code() ->
  generate_code("resources/sdl_spec_test.txt").
generate_code(Filename) ->
  register(sdl_helper, spawn(?MODULE, sdl_helper, [])),
  case file:open(Filename, [read]) of
    {ok, InputDevice} ->
      {ok, {spec, MacroList, TypeList, FunList}} = io:read(InputDevice, ""),
      file:close(InputDevice),
      file:delete(?ERL_FILENAME),
      file:delete(?HRL_FILENAME),
      file:delete(?C_FILENAME),

      % Erlang code
      InitLines = "-module("++?MODULE_NAME++").\n"
                  "-include(\"sdl_ports_gen.hrl\").\n"
                  "-compile(export_all).\n\n",
      write_file(?ERL_FILENAME, InitLines, [append]),
      generate_export(TypeList, FunList),
      generate_macros(MacroList),
      generate_init_port(),
      generate_native_types_parser_erl(),
      generate_types_parser_erl(TypeList),
      generate_functions_erl(FunList),
      reset_fun_code(),

      % C code
      generate_init_c(?C_LIB_IMPORT),
      generate_native_types_parser_c(),
      generate_types_parser_c(TypeList),
      generate_functions_c(FunList),
      generate_main_c();
      %io:format("Macros: ~p~n", [MacroList]),
      %io:format("Types: ~p~n", [TypeList]),
      %io:format("Funs: ~p~n", [FunList]);
    {error, Error} ->
      io:format("Error reading file: ~p~n", [Error])
  end,
  end_helper(),
  ok.

% TODO añadir los los new, delete y deref para tipos básicos
generate_export(TypeList, FunList) ->
  generate_export(TypeList, FunList, ["init_port/0"]).
generate_export([], [], Result) ->
  Line = "-export([\n\t"++string:join(lists:reverse(Result), ",\n\t")++"]).\n\n",
  write_file(?ERL_FILENAME, Line, [append]);
generate_export([], [{_,Name,_,Params,_,_}|FunList], Result) ->
  P = [ T || {_,T,O} <- Params, not lists:member(return, O) ],
  Elem = atom_to_list(Name)++"/"++integer_to_list(length(P)),
  generate_export([], FunList, [Elem|Result]);
generate_export([T|TypeList], FunList, Result) ->
  #type_spec{erlang_name=ErlName, type_descr=TypeDescr} = T,
  case is_tuple(TypeDescr) of
    true when (element(1,TypeDescr)==pointer) ->
      Elem = "pointer_deref_"++atom_to_list(ErlName)++"/1",
      generate_export(TypeList, FunList, [Elem|Result]);
    true when (element(1,TypeDescr)==struct) and (element(2,TypeDescr)/=opaque) ->
      Deref = "pointer_deref_"++atom_to_list(ErlName)++"/1",
      New = "new_"++atom_to_list(ErlName)++"/0",
      Delete = "delete_"++atom_to_list(ErlName)++"/1",
      NewResult = generate_export_setters_getters(element(2,TypeDescr), atom_to_list(ErlName), [Delete|[New|[Deref|Result]]]),
      generate_export(TypeList, FunList, NewResult);
    true when (element(1,TypeDescr)==union) and (element(2,TypeDescr)/=opaque) ->
      NewResult = generate_export_setters_getters(element(2,TypeDescr), atom_to_list(ErlName), Result),
      generate_export(TypeList, FunList, NewResult);
    true ->
      generate_export(TypeList, FunList, Result);
    false ->
      generate_export(TypeList, FunList, Result)
  end.

generate_export_setters_getters([], _StructName, Result) -> Result;
generate_export_setters_getters([M|Members], StructName, Result) ->
  #struct_member{erlang_name=AttribName} = M,
  Get = StructName++"_get_"++atom_to_list(AttribName)++"/1",
  Set = StructName++"_set_"++atom_to_list(AttribName)++"/2",
  generate_export_setters_getters(Members, StructName, [Set|[Get|Result]]).

generate_macros([]) ->
  write_file(?HRL_FILENAME, "\n", [append]);
generate_macros([Macro|MacroList]) ->
  Line = "-define(" ++ Macro#macro_spec.name ++ ", " ++ Macro#macro_spec.value ++ ").\n",
  case write_file(?HRL_FILENAME, Line, [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end,
  generate_macros(MacroList).

generate_init_port() ->
  Init = bbmustache:render(<<"init_port() ->\n"
          "\tPort = open_port({spawn, \"{{H}}\"}, [{packet, 2}]),\n"
          "\tregister({{P}}, Port), Port.\n\n">>, #{"H" => ?C_HANDLER, "P" => atom_to_list(?PORT_NAME)}),
  case file:write_file(?ERL_FILENAME, binary_to_list(Init)++?SEPARATOR_ERL, [append]) of
    ok -> ok;
    {error, Reason} -> io:format("Write macro error: ~p~n",[Reason])
  end.

% TODO pasar todo esto a plantillas
% TODO funciones new y delete para tipos básicos (int, float) y funciones deref_XXX para cada tipo.
% TODO hay que añadir al export todas estas funciones
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

  write_file(?ERL_FILENAME, IntType++FloatType++DoubleType++StringType++PointerType++?SEPARATOR_ERL, [append]).

generate_types_parser_erl([]) ->
  write_file("sdl_ports_gen.erl", ?SEPARATOR_ERL, [append]);
generate_types_parser_erl([TypeSpec|TypeList]) ->
  %io:format("Type spec: ~p~n", [Type]),
  #type_spec{erlang_name=ErlName, type_descr=TypeDescr} = TypeSpec,
  if
    is_tuple(TypeDescr) -> {Type, Desc} = TypeDescr;
    true -> Type = TypeDescr, Desc = nil
  end,
  ContentMap = #{"ErlName"=>ErlName, "Type"=>Type, "Desc"=>Desc, "Port"=>atom_to_list(?PORT_NAME)},

  case TypeDescr of
    {T, opaque} when (T==struct) or (T==union) ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\tpointer_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_pointer(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_pointer(Bytelist).\n\n">>;
    {struct, MemberList} ->
      Args = string:join([atom_to_list(A) || {_,A,_,_,_} <- MemberList], ", "),
      DerefName = "pointer_deref_"++atom_to_list(ErlName),
      NewName = "new_"++atom_to_list(ErlName),
      DeleteName = "delete_"++atom_to_list(ErlName),
      NewContentMap = #{"Args" => Args,
                        "Code" => integer_to_list(get_fun_code(DerefName)),
                        "CodeNew" => integer_to_list(get_fun_code(NewName)),
                        "CodeDelete" => integer_to_list(get_fun_code(DeleteName))},
      FinalMap = maps:merge(ContentMap, NewContentMap),
      RecordLine = bbmustache:render(<<"-record({{ErlName}}, {{{{Args}}}}).\n">>, FinalMap),
      write_file(?HRL_FILENAME, RecordLine, [append]),
      ContentPart1 = generate_struct_to_bytelist(MemberList, ErlName),
      ContentPart2 = generate_bytelist_to_struct(MemberList, ErlName),
      ContentPart3 = generate_parse_struct(MemberList, ErlName),
      ContentPart4 = <<"pointer_deref_{{ErlName}}(Pointer) ->\n"
                    "\tCode = int_to_bytelist({{Code}}),\n"
                    "\tPList = pointer_to_bytelist(Pointer),\n"
                    "\t{{Port}} ! {self(), {command, [Code, PList]}},\n"
                    "\treceive\n"
                    "\t\t{_, { data, DataList}} ->\n"
                    "\t\t\tbytelist_to_{{ErlName}}(DataList);\n"
                    "\t\tMsg ->\n"
                    "\t\t\t{error, Msg}\n"
                    "\tend.\n\n"
                    "new_{{ErlName}}() ->\n"
                    "\tCode = int_to_bytelist({{CodeNew}}),\n"
                    "\t{{Port}} ! {self(), {command, [Code]}},\n"
                    "\treceive\n"
                    "\t\t{_, { data, DataList}} ->\n"
                    "\t\t\tbytelist_to_pointer(DataList);\n"
                    "\t\tMsg ->\n"
                    "\t\t\t{error, Msg}\n"
                    "\tend.\n\n"
                    "delete_{{ErlName}}(Pointer) ->\n"
                    "\tCode = int_to_bytelist({{CodeDelete}}),\n"
                    "\tPList = pointer_to_bytelist(Pointer),\n"
                    "\t{{Port}} ! {self(), {command, [Code, PList]}},\n"
                    "\treceive\n"
                    "\t\t{_, { data, _DataList}} ->\n"
                    "\t\t\tok;\n"
                    "\t\tMsg ->\n"
                    "\t\t\t{error, Msg}\n"
                    "\tend.\n\n">>,
      ContentPart5 = generate_getters_setters_erl(MemberList, ErlName),
      Content = <<ContentPart1/binary, ContentPart2/binary, ContentPart3/binary,
                  ContentPart4/binary, ContentPart5/binary>>;
    {union, MemberList} ->
      FinalMap = ContentMap,
      ContentPart1 = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                      "\tpointer_to_bytelist(Value).\n\n"
                      "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                      "\tbytelist_to_pointer(Bytelist).\n\n"
                      "parse_{{ErlName}}(Bytelist) ->\n"
                      "\tparse_pointer(Bytelist).\n\n">>,
      ContentPart2 = generate_getters_setters_erl(MemberList, ErlName),
      Content = <<ContentPart1/binary, ContentPart2/binary>>;
    {enum, ElemList} ->
      FinalMap = ContentMap,
      GetInt = generate_enum_get_int(ElemList),
      GetAtom = generate_enum_get_atom(ElemList),
      SerializeEnum = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                      "\tInt = {{ErlName}}_get_int(Value),\n"
                      "\tint_to_bytelist(Int).\n\n",
                      "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                      "\tInt = bytelist_to_int(Bytelist),\n"
                      "\t{{ErlName}}_get_atom(Int).\n\n"
                      "parse_{{ErlName}}(Bytelist) ->\n"
                      "\t{Int, RList} = parse_int(Bytelist),\n"
                      "\t{{{ErlName}}_get_atom(Int), RList}.\n\n">>,
      Content = <<GetInt/binary, GetAtom/binary, SerializeEnum/binary>>;
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
      DerefName = "pointer_deref_"++atom_to_list(ErlName),
      FinalMap = maps:put("Code", integer_to_list(get_fun_code(DerefName)), ContentMap),
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\tpointer_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_pointer(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_pointer(Bytelist).\n\n"
                "pointer_deref_{{ErlName}}(Pointer) ->\n"
                "\tPList = pointer_to_bytelist(Pointer)\n"
                "\tCode = int_to_bytelist({{Code}}),\n"
                "\t{{Port}} ! {self(), {command, [Code, PList]}},\n"
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
    _ ->
      FinalMap = ContentMap,
      Content = <<"{{ErlName}}_to_bytelist(Value) ->\n"
                "\t{{Type}}_to_bytelist(Value).\n\n"
                "bytelist_to_{{ErlName}}(Bytelist) ->\n"
                "\tbytelist_to_{{Type}}(Bytelist).\n\n"
                "parse_{{ErlName}}(Bytelist) ->\n"
                "\tparse_{{Type}}(Bytelist).\n\n">>
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
generate_bytelist_to_struct([], MemberNames, StructName, Result, _Cnt) ->
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

generate_getters_setters_erl(MemberList, StructName) ->
  generate_getters_setters_erl(MemberList, StructName, <<"">>).
generate_getters_setters_erl([], _StructName, Result) -> Result;
generate_getters_setters_erl([M|MemberList], StructName, Result) ->
  #struct_member{erlang_name=AttribName, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  Get = <<"{{StructName}}_get_{{Attrib}}(Pointer) ->\n"
          "\tCode = int_to_bytelist({{CodeGet}}),\n"
          "\tPList = pointer_to_bytelist(Pointer),\n"
          "\t{{Port}} ! {self(), {command, [Code, PList]}},\n"
          "\treceive\n"
          "\t\t{ _, { data, DataList }} ->\n"
          "\t\t\tbytelist_to_{{Type}}(DataList);\n"
          "\t\tMsg ->\n"
          "\t\t\t{error, Msg}\n"
          "\tend.\n\n">>,
  Set = <<"{{StructName}}_set_{{Attrib}}(Pointer, Attrib) ->\n"
          "\tCode = int_to_bytelist({{CodeSet}}),\n"
          "\tPList = pointer_to_bytelist(Pointer),\n"
          "\tAList = {{Type}}_to_bytelist(Attrib),\n"
          "\t{{Port}} ! {self(), {command, [Code, PList, AList]}},\n"
          "\treceive\n"
          "\t\t{ _, { data, _DataList }} ->\n"
          "\t\t\tok;\n"
          "\t\tMsg ->\n"
          "\t\t\t{error, Msg}\n"
          "\tend.\n\n">>,
  Map = #{"StructName" => atom_to_list(StructName),
          "Attrib" => atom_to_list(AttribName),
          "Port" => atom_to_list(?PORT_NAME),
          "CodeGet" => integer_to_list(get_fun_code(atom_to_list(StructName)++"_get_"++atom_to_list(AttribName))),
          "CodeSet" => integer_to_list(get_fun_code(atom_to_list(StructName)++"_set_"++atom_to_list(AttribName))),
          "Type" => atom_to_list(Type)},
  NewLines = bbmustache:render(<<Get/binary, Set/binary>>, Map),
  generate_getters_setters_erl(MemberList, StructName, <<Result/binary, NewLines/binary>>).

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

generate_functions_erl([]) -> ok;
generate_functions_erl([Fun|FunList]) ->
  #fun_spec{erlang_name=ErlName, params=Params, type_descr=Descr} = Fun,
  StrParams = fun_params_to_string(Params),
  VarList = case ["Param"++integer_to_list(I) || I <- lists:seq(1,length(StrParams))] of
              [] -> ["[]"];
              List -> List
            end,
  Header = <<"{{ErlName}}({{HParams}}) ->\n">>,
  Body1 = <<"\tCode = int_to_bytelist({{Code}}),\n"
            "{{BodyParams}}"
            "\t{{Port}} ! {self(), {command, [Code, {{Vars}}]}},\n">>,
  case Descr of
    {pointer, _Type} ->
      RetType = pointer;
    _ ->
      RetType = Descr
  end,
  RPTypes = [T || {_,T,O} <- Params, lists:member(return, O)],
  case length(RPTypes) of
    0 -> RetParams = "\t\t\tRetParam1;\n";
    _ -> RetParams = generate_fun_return_params_erl([RetType|RPTypes])
  end,
  if
    (RetType==void) and (length(RPTypes)==0) ->
      Body2 = <<"\treceive\n"
                "\t\t{_, {data, _DataList}} ->\n"
                "\t\t\tok;\n"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    (RetType==void) and (length(RPTypes)>0) ->
      Body2 = <<"\treceive\n"
                "\t\t{_, {data, DataList}} ->\n"
                "\t\t\tR0 = DataList,\n"
                "{{RetParams}}"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>;
    true ->
      Body2 = <<"\treceive\n"
                "\t\t{_, {data, DataList}} ->\n"
                "\t\t\t{RetParam1, R1} = parse_{{RetType}}(DataList),\n"
                "{{RetParams}}"
                "\t\tMsg ->\n"
                "\t\t\t{error, Msg}\n"
                "\tend.\n\n">>
  end,

  Map = #{"ErlName"=>atom_to_list(ErlName),
          "HParams"=>string:join(StrParams, ", "),
          "BodyParams"=>generate_body_parameters_to_send(Params, StrParams),
          "Port"=>atom_to_list(?PORT_NAME),
          "Code"=>integer_to_list(get_fun_code(atom_to_list(ErlName))),
          "Vars"=>string:join(VarList,", "),
          "RetType"=>atom_to_list(RetType),
          "RetParams"=>RetParams},
  Content = <<Header/binary, Body1/binary, Body2/binary>>,
  write_file(?ERL_FILENAME, binary_to_list(bbmustache:render(Content, Map)), [append]),
  generate_functions_erl(FunList).

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
  fun_params_to_string(Params, NewResult, Cnt+1).

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
        _ ->
          Line = "\tParam" ++ integer_to_list(Cnt) ++ " = " ++ atom_to_list(T) ++ "_to_bytelist("++S++"),\n"
      end
  end,
  generate_body_parameters_to_send(Params, StrParams, Result++Line, Cnt+1).

generate_fun_return_params_erl([void|ParamList]) ->
  generate_fun_return_params_erl(ParamList, <<"">>, [], 1);
generate_fun_return_params_erl([_|ParamList]) ->
  generate_fun_return_params_erl(ParamList, <<"">>, ["RetParam1"], 2).
generate_fun_return_params_erl([], Result, ParamNames, _Cnt) ->
  PN = string:join(lists:reverse(ParamNames), ", "),
  Line = bbmustache:render(<<"\t\t\t{{{{PNames}}}};\n">>, #{"PNames"=>PN}),
  binary_to_list(<<Result/binary, Line/binary>>);
generate_fun_return_params_erl([P|ParamList], Result, ParamNames, Cnt) ->
  % Para cada parámetro de salida se pasa a Erlang el contenido del puntero
  Line = <<"\t\t\t{RetParam{{Cnt}}, R{{Cnt}}} = parse_{{Type}}(R{{PrevCnt}}),\n">>,
  NewPName = "RetParam"++integer_to_list(Cnt),
  case P of
    {pointer, T} -> Type = T;
    T -> Type = T
  end,
  Map = #{"Cnt"=>integer_to_list(Cnt), "Type"=>Type, "PrevCnt"=>integer_to_list(Cnt-1)},
  NewLine = bbmustache:render(Line, Map),
  generate_fun_return_params_erl(ParamList, <<Result/binary, NewLine/binary>>, [NewPName|ParamNames], Cnt+1).

generate_init_c(CLib) ->
  {ok, File} = file:read_file("resources/init_c_code.c"),
  StringLines = binary_to_list(bbmustache:render(File, #{"CLib"=>CLib})),
  write_file(?C_FILENAME, StringLines++?SEPARATOR_C, [append]).

generate_native_types_parser_c() ->
  {ok, File} = file:read_file("resources/native_types.c"),
  Content = unicode:characters_to_list(File),
  write_file(?C_FILENAME, Content++?SEPARATOR_C, [append]),
  add_type_name(int, "int"),
  add_type_name(float, "float"),
  add_type_name(double, "double"),
  add_type_name(string, "string"),
  add_type_name(pointer, "void*").

generate_types_parser_c([]) ->
  write_file("sdl_ports_gen.c", ?SEPARATOR_C, [append]);
generate_types_parser_c([TypeSpec|TypeList]) ->
  %io:format("Type spec: ~p~n", [Type]),
  #type_spec{erlang_name= ErlName, c_name=CName, type_descr=TypeDescr} = TypeSpec,
  if
    is_tuple(TypeDescr) -> {Type, Desc} = TypeDescr;
    true -> Type = TypeDescr, Desc = nil
  end,
  ContentMap = #{"ErlName"=>ErlName, "CName"=>CName, "Type"=>Type, "Desc"=>Desc, "Port"=>atom_to_list(?PORT_NAME)},
  add_type_name(ErlName, CName),

  case TypeDescr of
    {T, opaque} when (T==struct) or (T==union) ->
      Content = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_pointer(current_in, (void **) &result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_pointer(&value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n">>;
    {struct, MemberList} ->
      ErlNameStr = atom_to_list(ErlName),
      ReadStruct = generate_read_struct_c(MemberList),
      WriteStruct = generate_write_struct_c(MemberList),
      DerefStruct = <<"void pointer_deref_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
                      "\tbyte *current_in = in, *current_out = out;\n"
                      "\t*len_out = 0; current_in+=4;\n\n"
                      "\t{{CName}} *pointer;\n"
                      "\tcurrent_in = read_pointer(current_in, (void **) &pointer);\n"
                      "\tcurrent_out = write_{{ErlName}}(pointer, current_out, len_out);\n}\n\n">>,
      NewDeleteStruct = <<"void new_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
                          "\tbyte *current_in = in, *current_out = out;\n"
                          "\t*len_out = 0; current_in+=4;\n\n"
                          "\t{{CName}} *ptr = malloc(sizeof({{CName}}));\n"
                          "\tcurrent_out = write_pointer(&ptr, current_out, len_out);\n}\n\n"
                          "void delete_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
                          "\tbyte *current_in = in, *current_out = out;\n"
                          "\t*len_out = 0; current_in+=4;\n\n"
                          "\t{{CName}} *ptr;\n"
                          "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
                          "\tfree(ptr);\n}\n\n">>,
      add_fun_code("pointer_deref_"++ErlNameStr++"_Handler"),
      add_fun_code("new_"++ErlNameStr++"_Handler"),
      add_fun_code("delete_"++ErlNameStr++"_Handler"),
      GettersSetters = generate_getters_setters_c(MemberList, ErlNameStr, CName),
      Content = <<ReadStruct/binary, WriteStruct/binary, DerefStruct/binary, NewDeleteStruct/binary, GettersSetters/binary>>;
    {union, MemberList} ->
      ReadWrite = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_pointer(current_in, (void **) &result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_pointer(&value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n">>,
      GettersSetters = generate_getters_setters_c(MemberList, atom_to_list(ErlName), CName),
      Content = <<ReadWrite/binary, GettersSetters/binary>>;
    {enum, _ElemList} ->
      Content = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_int(current_in, result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_int(value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n">>;
    {array, _TD} ->
      Content = <<"">>; % De momento no
    {pointer, _TD} ->
      add_fun_code("pointer_deref_"++atom_to_list(ErlName)++"_Handler"),
      Content = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_pointer(current_in, result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_pointer(value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n"
                  "void pointer_deref_{{ErlName}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
                  "\tbyte *current_in = in, *current_out = out;\n"
                  "\t*len_out = 0; current_in+=4;\n\n"
                  "\t{{CName}} *pointer;\n"
                  "\tcurrent_in = read_{{ErlName}}(current_in, pointer);\n"
                  "\tcurrent_out = write_{{Desc}}(pointer, current_out, len_out);\n}\n\n">>;
    {T, _NBits} when (T==int) or (T==float) ->
      Content = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_{{Type}}{{Desc}}(current_in, result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_{{Type}}{{Desc}}(value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n">>;
    _ ->
      Content = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
                  "\tbyte *current_in = in;\n\n"
                  "\tcurrent_in = read_{{Type}}(current_in, result);\n\n"
                  "\treturn current_in;\n}\n\n"
                  "byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
                  "\tbyte *current_out = out;\n\n"
                  "\tcurrent_out = write_{{Type}}(value, current_out, len);\n\n"
                  "\treturn current_out;\n}\n\n">>
  end,
  write_file("sdl_ports_gen.c", binary_to_list(bbmustache:render(Content,ContentMap)), [append]),
  generate_types_parser_c(TypeList).

generate_read_struct_c(MemberList) ->
  InitFun = <<"byte * read_{{ErlName}}(byte *in, {{CName}} *result) {\n"
              "\tbyte *current_in = in;\n\n">>,
  generate_read_struct_c(MemberList, InitFun).
generate_read_struct_c([], Result) ->
  EndFun = <<"\n\treturn current_in;\n}\n\n">>,
  <<Result/binary, EndFun/binary>>;
generate_read_struct_c([M|MemberList], Result) ->
  #struct_member{c_name=CName, type_descr=TypeDescr} = M,
  Line = <<"\tcurrent_in = read_{{ParamType}}(current_in, &(result->{{CName}}));\n">>,
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  Map = #{"ParamType"=>Type, "CName"=>CName},
  NewLine = bbmustache:render(Line, Map),
  generate_read_struct_c(MemberList, <<Result/binary, NewLine/binary>>).

generate_write_struct_c(MemberList) ->
  InitFun = <<"byte * write_{{ErlName}}({{CName}} *value, byte *out, size_t *len) {\n"
              "\tbyte *current_out = out;\n\n">>,
  generate_write_struct_c(MemberList, InitFun).
generate_write_struct_c([], Result) ->
  EndFun = <<"\n\treturn current_out;\n}\n\n">>,
  <<Result/binary, EndFun/binary>>;
generate_write_struct_c([M|MemberList], Result) ->
  #struct_member{c_name=CName, type_descr=TypeDescr} = M,
  Line = <<"\tcurrent_out = write_{{ParamType}}(&(value->{{CName}}), current_out, len);\n">>,
  case is_tuple(TypeDescr)==true of
    true -> {Type,_} = TypeDescr;
    false -> Type = TypeDescr
  end,
  Map = #{"ParamType"=>Type, "CName"=>CName},
  NewLine = bbmustache:render(Line, Map),
  generate_write_struct_c(MemberList, <<Result/binary, NewLine/binary>>).

generate_getters_setters_c(MemberList, StructErlName, StructCName) ->
  generate_getters_setters_c(MemberList, StructErlName, StructCName, <<"">>).
generate_getters_setters_c([], _StructErlName, _StructCName, Result) -> Result;
generate_getters_setters_c([M|MemberList], StructErlName, StructCName, Result) ->
  #struct_member{erlang_name=AttribErl, c_name=AttribC, type_descr=TypeDescr} = M,
  case is_tuple(TypeDescr) of
    true ->
      TypeErl = atom_to_list(element(1,TypeDescr)),
      TypeC = get_type_name(element(2,TypeDescr))++"*";
    false ->
      TypeErl = atom_to_list(TypeDescr),
      TypeC = get_type_name(TypeDescr)
  end,
  case TypeC of
    "string" ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\t{{TypeC}} value;\n"
              "\tstrcpy(value, ptr->{{AttribC}});\n"
              "\tcurrent_out = write_{{TypeErl}}(&value, current_out, len_out);\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\t{{TypeC}} value;\n"
              "\tcurrent_in = read_{{TypeErl}}(current_in, &value);\n"
              "\tstrcpy(ptr->{{AttribC}}, value);\n}\n\n">>;
    _ ->
      Get = <<"void {{StructErl}}_get_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\t{{TypeC}} value;\n"
              "\tvalue = ptr->{{AttribC}};\n"
              "\tcurrent_out = write_{{TypeErl}}(&value, current_out, len_out);\n}\n\n">>,
      Set = <<"void {{StructErl}}_set_{{AttribErl}}_Handler(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n"
              "\t{{StructC}} *ptr;\n"
              "\tcurrent_in = read_pointer(current_in, (void **) &ptr);\n"
              "\t{{TypeC}} value;\n"
              "\tcurrent_in = read_{{TypeErl}}(current_in, &value);\n"
              "\tptr->{{AttribC}} = value;\n}\n\n">>
  end,
  add_fun_code(StructErlName++"_get_"++atom_to_list(AttribErl)++"_Handler"),
  add_fun_code(StructErlName++"_set_"++atom_to_list(AttribErl)++"_Handler"),
  Map = #{"StructErl" => StructErlName,
          "StructC" => StructCName,
          "AttribErl" => atom_to_list(AttribErl),
          "AttribC" => AttribC,
          "TypeErl" => TypeErl,
          "TypeC" => TypeC},
  NewLines = bbmustache:render(<<Get/binary, Set/binary>>, Map),
  generate_getters_setters_c(MemberList, StructErlName, StructCName, <<Result/binary, NewLines/binary>>).

generate_functions_c([]) ->
  Array = generate_fun_array(get_fun_list()),
  write_file(?C_FILENAME, binary_to_list(Array)++?SEPARATOR_C, [append]),
  ok;
generate_functions_c([F|FunList]) ->
  #fun_spec{c_name=CName, params=Params, type_descr=Descr} = F,
  HandlerName = CName++"_Handler",
  InitLines = <<"void {{HandlerName}}(byte *in, size_t len_in, byte *out, size_t *len_out) {\n"
              "\tbyte *current_in = in, *current_out = out;\n"
              "\t*len_out = 0; current_in+=4;\n\n">>,
  {ReadLines, Vars, RetVars} = generate_fun_params_c(Params),
  case Descr of
    void ->
      RetType = nil,
      FunLine = <<"\t{{CName}}({{FunVars}});\n">>;
    string ->
      RetType = string,
      FunLine = <<"\t{{RetCType}} retvar;\n"
                  "\tstrcpy(retvar, {{CName}}());\n"
                  "\tcurrent_out = write_{{RetType}}(&retvar, current_out, len_out);\n">>;
    {pointer, Type} ->
      RetType = Type,
      FunLine = <<"\t{{RetCType}} *retvar = {{CName}}({{FunVars}});\n"
                  "\tcurrent_out = write_pointer(&retvar, current_out, len_out);\n">>;
    Type ->
      RetType = Type,
      FunLine = <<"\t{{RetCType}} retvar = {{CName}}({{FunVars}});\n"
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
  write_file(?C_FILENAME, binary_to_list(bbmustache:render(Content, Map)), [append]),
  add_fun_code(HandlerName),
  generate_functions_c(FunList).

generate_fun_params_c([]) ->
  {<<"">>, [], []};
generate_fun_params_c(Params) ->
  generate_fun_params_c(Params, <<"">>, [], [], 1).
generate_fun_params_c([], ReadLines, Vars, RetVars, _Cnt) ->
  CR = <<"\n">>,
  {<<ReadLines/binary, CR/binary>>, lists:reverse(Vars), lists:reverse(RetVars)};
generate_fun_params_c([{_,T,O}|Params], ReadLines, Vars, RetVars, Cnt) ->
  VarName = "var"++integer_to_list(Cnt),
  Return = lists:member(return, O),
  case is_tuple(T) of
    true when Return->
      {Type, ErlType} = T,
      Line1 = <<"\t{{CType}} *{{VarName}} = malloc(sizeof({{CType}}));\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName, "ReadType"=>Type};
    true ->
      {Type, ErlType} = T,
      Line1 = <<"\t{{CType}} *{{VarName}};\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName, "ReadType"=>Type};
    false ->
      ErlType = T,
      Line1 = <<"\t{{CType}} {{VarName}};\n">>,
      Map = #{"CType"=>get_type_name(ErlType), "VarName"=>VarName, "ReadType"=>ErlType}
  end,
  case lists:member(return, O) of
    true ->
      Line2 = <<"">>,
      NewRetVars = [{VarName, ErlType}|RetVars];
    false ->
      Line2 = <<"\tcurrent_in = read_{{ReadType}}(current_in, &{{VarName}});\n">>,
      NewRetVars = RetVars
  end,
  NewLines = bbmustache:render(<<Line1/binary, Line2/binary>>, Map),
  generate_fun_params_c(Params, <<ReadLines/binary, NewLines/binary>>, [VarName|Vars], NewRetVars, Cnt+1).

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

generate_main_c() ->
  {ok, File} = file:read_file("resources/main_funcs.c"),
  Content = unicode:characters_to_list(File),
  write_file(?C_FILENAME, Content, [append]).

% find_type_spec_by_erlname(_Type, []) -> nil;
% find_type_spec_by_erlname(Type, [TS|TypeSpecList]) ->
%   {_,Name,_,_,_} = TS,
%   case Type==Name of
%     true -> TS;
%     false -> find_type_spec_by_erlname(Type, TypeSpecList)
%   end.
%
% find_type_spec_by_cname(_Type, []) -> nil;
% find_type_spec_by_cname(Type, [TS|TypeSpecList]) ->
%   {_,_,Name,_,_} = TS,
%   case Type==Name of
%     true -> TS;
%     false -> find_type_spec_by_cname(Type, TypeSpecList)
%   end.

write_file(File, Content, Modes) ->
  case file:write_file(File, Content, Modes) of
    ok -> ok;
    {error, Reason} -> io:format("Write error: ~p~n",[Reason])
  end.

sdl_helper() ->
  sdl_helper(1, [], maps:new()).
sdl_helper(Code, FunList, TypeMap) ->
  receive
    {Pid, {fun_code, NameFun}} ->
      Pid ! {fun_code, Code},
      sdl_helper(Code+1, FunList++[{Code, NameFun}], TypeMap);
    {_Pid, {fun_add, NameFun}} ->
      sdl_helper(Code+1, FunList++[{Code, NameFun}], TypeMap);
    {Pid, fun_list} ->
      Pid ! {fun_list, FunList},
      sdl_helper(Code, FunList, TypeMap);
    {_Pid, reset} ->
      sdl_helper(1, [], maps:new());
    {_Pid, {new_type, ErlName, CName}} ->
      sdl_helper(Code, FunList, maps:put(ErlName, CName, TypeMap));
    {Pid, {get_type, ErlName}} ->
      Pid ! {type_name, maps:get(ErlName, TypeMap, "")},
      sdl_helper(Code, FunList, TypeMap);
    {_Pid, exit} ->
      ok;
    _ ->
      sdl_helper(Code, FunList, TypeMap)
  end.

get_fun_code(NameFun) ->
  sdl_helper ! {self(), {fun_code, NameFun}},
  receive
    {fun_code, Code} -> Code
  end.

add_fun_code(NameFun) ->
  sdl_helper ! {self(), {fun_add, NameFun}}.

get_fun_list() ->
  sdl_helper ! {self(), fun_list},
  receive
    {fun_list, FunList} -> FunList
  end.

reset_fun_code() ->
  sdl_helper ! {self(), reset}.

add_type_name(ErlName, CName) ->
  sdl_helper ! {self(), {new_type, ErlName, CName}}.

get_type_name(ErlName) ->
  sdl_helper ! {self(), {get_type, ErlName}},
  receive
    {type_name, CName} -> CName
  end.

end_helper() ->
  sdl_helper ! {self(), exit}.
