{{ErlName}}_to_bytelist(Value) ->
	{{Type}}_to_bytelist(Value, {{Desc}}).

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_{{Type}}(Bytelist, {{Desc}}).

parse_{{ErlName}}(Bytelist) ->
	parse_{{Type}}(Bytelist, {{Desc}}).

{{ErlName}}_array_to_bytelist(List, Size) ->
	{{Type}}_array_to_bytelist(List, Size, {{Desc}}).

bytelist_to_{{ErlName}}_array(Bytelist, Size) ->
	bytelist_to_{{Type}}_array(Bytelist, Size, {{Desc}}).

parse_{{ErlName}}_array(Bytelist, Size) ->
	parse_{{Type}}_array(Bytelist, Size, {{Desc}}).

pointer_deref_{{ErlName}}(Pointer) ->
	pointer_deref_{{Type}}{{Desc}}(Pointer).

pointer_deref_{{ErlName}}_array(Pointer, Index) ->
	pointer_deref_{{Type}}{{Desc}}_array(Pointer, Index).

pointer_deref_{{ErlName}}_assign(Pointer, Value) ->
	pointer_deref_{{Type}}{{Desc}}_assign(Pointer, Value).

pointer_deref_{{ErlName}}_array_assign(Pointer, Index, Value) ->
	pointer_deref_{{Type}}{{Desc}}_array_assign(Pointer, Index, Value).

new_{{ErlName}}() ->
	new_{{Type}}{{Desc}}().

new_{{ErlName}}_auto() ->
	new_{{Type}}{{Desc}}_auto().

new_{{ErlName}}_array(Size) ->
	new_{{Type}}{{Desc}}_array(Size).

new_{{ErlName}}_array_auto(Size) ->
	new_{{Type}}{{Desc}}_array_auto(Size).

delete_{{ErlName}}(Pointer) ->
	delete_{{Type}}{{Desc}}(Pointer).

list_to_{{ErlName}}_array(List) ->
	list_to_{{Type}}{{Desc}}_array(List).

{{ErlName}}_array_to_list(Pointer, Size) ->
	{{Type}}{{Desc}}_array_to_list(Pointer, Size).

