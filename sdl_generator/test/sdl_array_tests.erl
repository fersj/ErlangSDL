-module(sdl_array_tests).

-import(sdl_ports_gen,[
	init_port/1,
	maxint/1,
	new_arrayA/0,
	pointer_deref_arrayA/1,
	arrayA_set_values/2,
	arrayA_set_id/2,
	new_arrayB/0,
	list_to_string_array/1,
	arrayB_set_values/2,
	arrayB_get_values/1,
	string_array_to_list/2,
	list_to_int_array/1,
	int_array_to_list/2,
	pointer_deref_string_array_assign/3,
	pointer_deref_string_array/2,
	new_arrayC/0,
	arrayC_set_size/2,
	arrayC_set_arraylist_values/2,
	arrayC_get_arraylist_values/1,
	pointer_deref_arrayC/1,
	arrayC_set_values/2,
	new_int_array/1,
	new_string/0,
	pointer_deref_string_assign/2,
	pointer_deref_string/1
	]).

-export([test/0]).

-include("_checkouts/sdl_generator/sdl_ports_gen.hrl").

test() ->
	sdl_ports_gen:init_port(),
	io:format("Puerto iniciado~n", []),

	Value = maxint([2,3,4,5,76,23]),

	io:format("Max: ~p~n", [Value]),
	A = new_arrayA(),
	io:format("ArrayA: ~p~n", [pointer_deref_arrayA(A)]),
	arrayA_set_id(A, 13),
	arrayA_set_values(A, [1,2,3,4,5,6,7,8,9,10]),
	io:format("ArrayA: ~p~n", [pointer_deref_arrayA(A)]),

	B = new_arrayB(),
	ArrayInt = list_to_int_array([4, 3, 2, 1]),
	arrayB_set_values(B, ArrayInt),
	io:format("ArrayB (values): ~p~n", [int_array_to_list(arrayB_get_values(B),4)]),

	C = new_arrayC(),
	ListInt = [4,5,6,7,8,9],
	arrayC_set_size(C, length(ListInt)),
	arrayC_set_values(C, new_int_array(length(ListInt))),
	arrayC_set_arraylist_values(C, ListInt),
	io:format("ArrayC: ~p~n", [pointer_deref_arrayC(C)]),
	io:format("ArrayC (values): ~p~n", [arrayC_get_arraylist_values(C)]),

	String = new_string(),
	pointer_deref_string_assign(String, "prueba"),
	io:format("String: ~s~n", [pointer_deref_string(String)]),
	ArrayStr = list_to_string_array(["hola", "mundo", "y", "adios"]),
	pointer_deref_string_array_assign(ArrayStr, 1, "nuevo"),
	io:format("ArrayStr[1]: ~s~n", [pointer_deref_string_array(ArrayStr, 1)]),
	io:format("ArrayStr: ~s~n", [string_array_to_list(ArrayStr,4)]).