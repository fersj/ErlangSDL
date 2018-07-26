-ifdef(debug).
-define(DEBUG(X, Y), io:format(X, Y)).
-else.
-define(DEBUG(X, Y), ok).
-endif.

-define(PORT_NAME, {{PortName}}).

-define(CALL_CODE, 10).
-define(RET_CODE, 20).

%--------------------------------------------------------

start_port_owner(Name) ->
	Port = open_port({spawn, Name}, [{packet, 2}]),
	loop_port_owner(Port).

loop_port_owner(Port) ->
	receive
		{Pid, {call, List, Funs}} ->
			Port ! { self(), { command, List }},
			loop_receive_from_c(Port, Pid, Funs),
			loop_port_owner(Port)
		% {_, {cast, List}} ->
		% 	Port ! { self(), { command, List }},
		% 	loop_port_owner(Port)
	end.

loop_receive_from_c(Port, Pid, Funs) ->
		receive 
			{ _, { data, [?RET_CODE | Msg] }} -> 
				?DEBUG("Received result binary: ~w~n", [Msg]), 
				Pid ! {self(), {datalist, Msg}};
			{ _, { data, [?CALL_CODE, FunId | Msg] }} -> 
				?DEBUG("Received call request nº ~w: ~w~n", [FunId, Msg]),
				PortOwner = self(),
				PidN = spawn(fun() ->
					Fun = lists:nth(FunId, Funs),
					Result = Fun(Msg),
					?DEBUG("About to send RET with: ~w~n", [Result]),
					PortOwner ! { self(), { result, [?RET_CODE | Result] } }
				end),
				loop_receive_from_erlang(Port, PidN),
				loop_receive_from_c(Port, Pid, Funs);
			Other ->
				?DEBUG("Unknown response from port: ~w~n", [Other]),
				Pid ! {self(), Other}
		end.
	
loop_receive_from_erlang(Port, PidCaller)   ->
	receive
		{ PidCaller, { call, List, Funs } } ->
			Port ! { self(), { command, List } },
			loop_receive_from_c(Port, PidCaller, Funs),
			loop_receive_from_erlang(Port, PidCaller);
		{ PidCaller, { result, Result } } ->
			Port ! { self(), { command, Result }}
	end.

call_port_owner(PortOwner, List) -> 
	call_port_owner(PortOwner, List, []).

call_port_owner(undefined, _, _) ->
	io:format("Undefined port owner.~n");

call_port_owner(PortOwner, List, Funs) when is_atom(PortOwner) ->
	call_port_owner(whereis(PortOwner), List, Funs);

call_port_owner(PortOwner, List, Funs) ->
	PortOwner ! { self(), { call, [?CALL_CODE | List], Funs }},
	receive
		{PortOwner, X} -> X
	end.

init_port() ->
	Pid = spawn(fun() -> start_port_owner("{{CHandlerPath}}") end),
	%io:format("PID Owner: ~w~n", [Pid]),
	register(?PORT_NAME, Pid),
	code:ensure_loaded(erlang_gc),
	io:format("{{PortName}} initialized.~n"),
	ok.

