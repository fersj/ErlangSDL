{{ErlName}}_to_bytelist(Value) ->
	Int = {{ErlName}}_get_int(Value),
	int_to_bytelist(Int).

bytelist_to_{{ErlName}}(Bytelist) ->
	Int = bytelist_to_int(Bytelist),
	{{ErlName}}_get_atom(Int).

parse_{{ErlName}}(Bytelist) ->
	{Int, RList} = parse_int(Bytelist),
	{{{ErlName}}_get_atom(Int), RList}.

{{ErlName}}_array_to_bytelist(List, Size) when length(List)==Size ->
	[int_to_bytelist({{ErlName}}_get_int(E)) || E<-List].

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_{{ErlName}}_array(Bytelist, Size, []).
bytelist_to_{{ErlName}}_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_{{ErlName}}_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_int(Bytelist),
	bytelist_to_{{ErlName}}_array(Rest, Size-1, [{{ErlName}}_get_atom(Elem)|Result]).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_{{ErlName}}_array(Bytelist, Size, []).
parse_{{ErlName}}_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_{{ErlName}}_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_int(Bytelist),
	parse_{{ErlName}}_array(Rest, Size-1, [{{ErlName}}_get_atom(Elem)|Result]).

pointer_deref_{{ErlName}}(Pointer) ->
	{{ErlName}}_get_atom(pointer_deref_int(Pointer)).

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	{{ErlName}}_get_atom(pointer_deref_int_array(Pointer, Index)).

pointer_deref_{{ErlName}}_assign(Pointer, Value) ->
	pointer_deref_int_assign(Pointer, {{ErlName}}_get_int(Value)).

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	pointer_deref_int_array_assign(Pointer, Index, {{ErlName}}_get_int(Value)).

new_{{ErlName}}() ->
	new_int().

new_{{ErlName}}_auto() ->
	new_int_auto().

new_{{ErlName}}_array(Size) ->
	new_int_array(Size).

new_{{ErlName}}_array_auto(Size) ->
	new_int_array_auto(Size).

delete_{{ErlName}}(Pointer) ->
	delete_int(Pointer).

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

