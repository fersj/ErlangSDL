% ---- Int ----

int_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

int_to_bytelist(Value, NBits) ->
	binary:bin_to_list(<< Value:NBits >>).

bytelist_to_int(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

bytelist_to_int(Bytelist, NBits) ->
	<< Value : NBits >> = binary:list_to_bin(Bytelist), Value.

parse_int(Bytelist) ->
	parse_int(Bytelist, 32).

parse_int(Bytelist, NBytes) ->
	parse_int(Bytelist, NBytes, NBytes, []).
parse_int([B|Rest], NBytes, Cnt, Result) when Cnt>0 ->
	parse_int(Rest, NBytes, Cnt-8, [B|Result]);
parse_int(Bytelist, NBytes, _Cnt, Result) ->
	{bytelist_to_int(lists:reverse(Result), NBytes), Bytelist}.

pointer_deref_int(Pointer) ->
	Code = int_to_bytelist(1),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int_assign(Pointer, Value) ->
	Code = int_to_bytelist(2),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int() ->
	Code = int_to_bytelist(3),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_int(Pointer) ->
	Code = int_to_bytelist(4),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

% ---- Float ----

float_to_bytelist(Value) ->
	float_to_bytelist(Value, 32).

float_to_bytelist(Value, NBits) ->
	binary:bin_to_list(<< Value:NBits/float >>).

bytelist_to_float(Bytelist) ->
	bytelist_to_float(Bytelist, 32).

bytelist_to_float(Bytelist, NBits) ->
	<< Value : NBits/float >> = binary:list_to_bin(Bytelist), Value.

parse_float(Bytelist) ->
	parse_float(Bytelist, 32).

parse_float(Bytelist, NBytes) ->
	parse_float(Bytelist, NBytes, NBytes, []).
parse_float([B|Rest], NBytes, Cnt, Result) when Cnt>0 ->
	parse_float(Rest, NBytes, Cnt-8, [B|Result]);
parse_float(Bytelist, NBytes, _Cnt, Result) ->
	{bytelist_to_float(lists:reverse(Result), NBytes), Bytelist}.

pointer_deref_float(Pointer) ->
	Code = int_to_bytelist(5),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_assign(Pointer, Value) ->
	Code = int_to_bytelist(6),
	PList = pointer_to_bytelist(Pointer),
	VList = float_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_float() ->
	Code = int_to_bytelist(7),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_float(Pointer) ->
	Code = int_to_bytelist(8),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

% ---- Double ----

double_to_bytelist(Value) ->
	float_to_bytelist(Value, 64).

bytelist_to_double(Bytelist) ->
	bytelist_to_float(Bytelist, 64).

parse_double(Bytelist) ->
	parse_float(Bytelist, 64).

pointer_deref_double(Pointer) ->
	Code = int_to_bytelist(9),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_double(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_assign(Pointer, Value) ->
	Code = int_to_bytelist(10),
	PList = pointer_to_bytelist(Pointer),
	VList = double_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_double() ->
	Code = int_to_bytelist(11),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_double(Pointer) ->
	Code = int_to_bytelist(12),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

% ---- String ----

string_to_bytelist(Value, Encoding) ->
	binary:bin_to_list(unicode:characters_to_binary(Value, Encoding))++[$\0].

bytelist_to_string(Bytelist, Encoding) ->
	unicode:characters_to_list(binary:list_to_bin(Bytelist), Encoding).

string_to_bytelist(Value) ->
	string_to_bytelist(Value, utf8).

bytelist_to_string(Bytelist) ->
	bytelist_to_string(Bytelist, utf8).

parse_string(Bytelist) ->
	parse_string(Bytelist, []).
parse_string([$\0|Rest], Result) ->
	{bytelist_to_string(lists:reverse(Result)), Rest};
parse_string([B|Rest], Result) ->
	parse_string(Rest, [B|Result]).

pointer_deref_string(Pointer) ->
	Code = int_to_bytelist(13),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_assign(Pointer, Value) ->
	Code = int_to_bytelist(14),
	PList = pointer_to_bytelist(Pointer),
	VList = string_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_string() ->
	Code = int_to_bytelist(15),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_string(Pointer) ->
	Code = int_to_bytelist(16),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

% ---- Pointer ----

pointer_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_pointer(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_pointer(Bytelist) ->
	parse_int(Bytelist, 64).

