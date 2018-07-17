{{ErlName}}_to_bytelist(Value) ->
	string_to_bytelist(Value, {{Desc}}).

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_string(Bytelist, {{Desc}}).

parse_{{ErlName}}(Bytelist) ->
	parse_string(Bytelist).

{{ErlName}}_array_to_bytelist(List, Size) ->
	string_array_to_bytelist(List, Size, {{Desc}}).

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_string_array(Bytelist, Size, {{Desc}}).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_string_array(Bytelist, Size, {{Desc}}).

pointer_deref_{{ErlName}}(Pointer, Index) ->
	pointer_deref_string(Pointer, {{Desc}}).

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	pointer_deref_string_array(Pointer, Index, {{Desc}}).

pointer_deref_{{ErlName}}_assign(Pointer, Value) ->
	pointer_deref_string_assign(Pointer, Value, {{Desc}}).

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	pointer_deref_string_array_assign(Pointer, Index, Value, {{Desc}}).

new_{{ErlName}}() ->
	new_string().

new_{{ErlName}}_array(Size) ->
	new_string_array(Size).

delete_{{ErlName}}(Pointer) ->
	delete_string(Pointer).

list_to_{{ErlName}}_array(List) ->
	list_to_array_array(List, {{Desc}}).

{{ErlName}}_array_to_list(Pointer, Size) ->
	string_array_to_list(Pointer, Size, {{Desc}}).