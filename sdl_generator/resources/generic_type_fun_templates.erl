{{ErlName}}_to_bytelist(Value) ->
	{{Type}}_to_bytelist(Value).

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_{{Type}}(Bytelist).

parse_{{ErlName}}(Bytelist) ->
	parse_{{Type}}(Bytelist).

{{ErlName}}_array_to_bytelist(List, Size) ->
	{{Type}}_array_to_bytelist(List, Size).

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_{{Type}}_array(Bytelist, Size).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_{{Type}}_array(Bytelist, Size).

pointer_deref_{{ErlName}}(Pointer) ->
	pointer_deref_{{Type}}(Pointer).

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	pointer_deref_{{Type}}_array(Pointer, Index).

pointer_deref_{{ErlName}}_assign(Pointer, Value) ->
	pointer_deref_{{Type}}_assign(Pointer, Value).

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	pointer_deref_{{Type}}_array_assign(Pointer, Index, Value).

new_{{ErlName}}() ->
	new_{{Type}}().

new_{{ErlName}}_array(Size) ->
	new_{{Type}}_array(Size).

delete_{{ErlName}}(Pointer) ->
	delete_{{Type}}(Pointer).

list_to_{{ErlName}}_array(List) ->
	list_to_{{Type}}_array(List).

{{ErlName}}_array_to_list(Pointer, Size) ->
	{{Type}}_array_to_list(Pointer, Size).

