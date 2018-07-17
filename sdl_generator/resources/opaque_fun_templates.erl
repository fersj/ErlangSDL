{{ErlName}}_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_{{ErlName}}(Bytelist) ->
	parse_pointer(Bytelist).

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	pointer_deref_pointer_array(Pointer, Index).

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	pointer_deref_pointer_array_assign(Pointer, Index, Value).

{{ErlName}}_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_{{ErlName}}_array(Size) ->
	new_pointer_array(Size).

delete_{{ErlName}}(Pointer) ->
	delete_pointer(Pointer).

list_to_{{ErlName}}_array(List) ->
	list_to_pointer_array(List).

{{ErlName}}_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

