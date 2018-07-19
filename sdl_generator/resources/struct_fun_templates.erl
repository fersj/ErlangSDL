{{ErlName}}_array_to_bytelist(List, Size) when length(List)==Size ->
	[{{ErlName}}_to_bytelist(E) || E<-List].

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_{{ErlName}}_array(Bytelist, Size, []).
bytelist_to_{{ErlName}}_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_{{ErlName}}_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_{{ErlName}}(Bytelist),
	bytelist_to_{{ErlName}}_array(Rest, Size-1, [Elem|Result]).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_{{ErlName}}_array(Bytelist, Size, []).
parse_{{ErlName}}_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_{{ErlName}}_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_{{ErlName}}(Bytelist),
	parse_{{ErlName}}_array(Rest, Size-1, [Elem|Result]).

pointer_deref_{{ErlName}}(Pointer) ->
	Code = int_to_bytelist({{Code}}),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_{{ErlName}}(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	Code = int_to_bytelist({{CodeArray}}),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_{{ErlName}}(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_{{ErlName}}_assign(Pointer, Value) ->
	Code = int_to_bytelist({{CodeAssign}}),
	PList = pointer_to_bytelist(Pointer),
	VList = {{ErlName}}_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist({{CodeArrayAssign}}),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = {{ErlName}}_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

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
	Size = length(List),
	Pointer = new_{{ErlName}}_array(Size),
	list_to_{{ErlName}}_array(List, Pointer, 0).
list_to_{{ErlName}}_array([], Pointer, _Index) -> Pointer;
list_to_{{ErlName}}_array([Value|List], Pointer, Index) ->
	pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value),
	list_to_{{ErlName}}_array(List, Pointer, Index+1).

{{ErlName}}_array_to_list(Pointer, Size) ->
	{{ErlName}}_array_to_list(Pointer, Size-1, []).
{{ErlName}}_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
{{ErlName}}_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_{{ErlName}}_array(Pointer, Size),
	{{ErlName}}_array_to_list(Pointer, Size-1, [Elem|Result]).

