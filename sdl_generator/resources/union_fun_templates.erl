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
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_{{ErlName}}_auto() ->
	Pointer = new_{{ErlName}}(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_{{ErlName}}, P);
		Error -> Error
	end.

new_{{ErlName}}_array(Size) ->
	Code = int_to_bytelist({{CodeNewArray}}),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_{{ErlName}}_array_auto(Size) ->
	Pointer = new_{{ErlName}}_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_{{ErlName}}, P);
		Error -> Error
	end.

delete_{{ErlName}}(Pointer) ->
	Code = int_to_bytelist({{CodeDelete}}),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_{{ErlName}}_array(List) ->
	list_to_pointer_array(List).

{{ErlName}}_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

