-ifdef(debug).
-define(DEBUG(X, Y), io:format(X, Y)).
-else.
-define(DEBUG(X, Y), ok).
-endif.

-define(PORT_NAME, {{PortName}}).

%--------------------------------------------------------

start_port_owner(Name) ->
	Port = open_port({spawn, Name}, [{packet, 2}]),
	loop_port_owner(Port).

loop_port_owner(Port) ->
	receive
		{Pid, {call, List}} ->
			Port ! { self(), { command, List }},
			receive 
				{ _, { data, Msg }} -> 
					?DEBUG("Received binary: ~w~n", [Msg]),
					Pid ! {self(), {datalist, Msg}};
				Other ->
					?DEBUG("Unknown response from port: ~w~n", [Other]),
					Pid ! {self(), Other}
			end,
			loop_port_owner(Port)
%    {_, {cast, List}} ->
%      Port ! { self(), { command, List }},
%      loop_port_owner(Port)
	end.

call_port_owner(undefined, _) ->
	io:format("Undefined port owner.~n");
	
call_port_owner(PortOwner, List) when is_atom(PortOwner) ->
	call_port_owner(whereis(PortOwner), List);
	
call_port_owner(PortOwner, List) ->
	PortOwner ! { self(), { call , List }},
	receive
		{PortOwner, X} -> X
	end.

init_port() ->
	Pid = spawn(fun() -> start_port_owner("{{CHandlerPath}}") end),
	%io:format("PID Owner: ~w~n", [Pid]),
	register(?PORT_NAME, Pid),
	%code:ensure_loaded(erlang_gc),
	io:format("{{PortName}} initialized.~n"),
	ok.

