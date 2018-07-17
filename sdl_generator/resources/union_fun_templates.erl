{{ErlName}}_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_{{ErlName}}(Bytelist) ->
	parse_pointer(Bytelist).

{{ErlName}}_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_{{ErlName}}() ->
	Code = int_to_bytelist({{CodeNew}}),
	{{Port}} ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_{{ErlName}}_array(Size) ->
	Code = int_to_bytelist({{CodeNewArray}}),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_{{ErlName}}(Pointer) ->
	Code = int_to_bytelist({{CodeDelete}}),
	PList = pointer_to_bytelist(Pointer),
	{{Port}} ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_{{ErlName}}_array(List) ->
	list_to_pointer_array(List).

{{ErlName}}_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

