{{ErlName}}_to_bytelist(List) when length(List)=={{Desc}} ->
	[{{Type}}_to_bytelist || E<-List].

bytelist_to_{{ErlName}}(Bytelist) ->
	bytelist_to_{{ErlName}}(Bytelist, []).
bytelist_to_{{ErlName}}([], Result) ->
	lists:reverse(Result).
bytelist_to_{{ErlName}}(Bytelist, Result) ->
	{Elem, Rest} = parse_{{Type}}(Bytelist),
	bytelist_to_{{ErlName}}(Rest, [Elem|Result]).

parse_{{ErlName}}(Bytelist) ->
	parse_{{ErlName}}(Bytelist, [], 0).
parse_{{ErlName}}(Bytelist, Result, {{Desc}}) ->
	{lists:reverse(Result), Bytelist}.
parse_{{ErlName}}(Bytelist, Result, Cnt) ->
	{Elem, Rest} = parse_{{Type}}(Bytelist),
	parse_{{ErlName}}(Rest, [Elem|Result], Cnt+1).