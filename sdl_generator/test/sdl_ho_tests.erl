-module(sdl_ho_tests).

-import(sdl_ports_gen,[
	init_port/1,
	apply_int/3,
	new_int/0,
	pointer_deref_int_assign/2,
	pointer_deref_int/1,
	]).

-export([test/0]).

-include("_checkouts/sdl_generator/sdl_ports_gen.hrl").

test() ->
	sdl_ports_gen:init_port(),
	io:format("Puerto iniciado~n", []),

	Result = sdl_ports_gen:apply_int(3, fun(X) -> X * 2 end, fun(X) -> X + 1 end),
	io:format("Result apply_int: ~p~n", [Result]),
	
	Ptr = sdl_ports_gen:new_int(),
	io:format("Ptr: ~p~n", [Ptr]),
	sdl_ports_gen:pointer_deref_int_assign(Ptr, 10),
	io:format("Deref int: ~p~n", [sdl_ports_gen:pointer_deref_int(Ptr)]),
	Fun1 = fun(X) ->
				V = sdl_ports_gen:pointer_deref_int(Ptr),
				io:format("Deref int (Fun1):~p~n",[V]),
				V + X
				end,
	Fun2 = fun(X) -> X + 1 end,
	Result2 = sdl_ports_gen:apply_int(3, Fun1, Fun2),
	io:format("Result apply_int ptr: ~p~n", [Result2]).