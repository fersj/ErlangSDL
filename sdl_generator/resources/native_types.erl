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

int_array_to_bytelist(List, Size) when length(List)==Size ->
	int_array_to_bytelist(List, Size, 32).

bytelist_to_int_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 32).

parse_int_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 32).

int_array_to_bytelist(List, Size, NBits) when length(List)==Size ->
	[int_to_bytelist(E, NBits) || E<-List].

bytelist_to_int_array(Bytelist, Size, NBits) ->
	bytelist_to_int_array(Bytelist, Size, NBits, []).
bytelist_to_int_array(_Bytelist, 0, _NBits, Result) ->
	lists:reverse(Result);
bytelist_to_int_array(Bytelist, Size, NBits, Result) ->
	{Elem, Rest} = parse_int(Bytelist, NBits),
	bytelist_to_int_array(Rest, Size-1, NBits, [Elem|Result]).

parse_int_array(Bytelist, Size, NBits) ->
	parse_int_array(Bytelist, Size, NBits, []).
parse_int_array(Bytelist, 0, _NBits, Result) ->
	{lists:reverse(Result), Bytelist};
parse_int_array(Bytelist, Size, NBits, Result) ->
	{Elem, Rest} = parse_int(Bytelist, NBits),
	parse_int_array(Rest, Size-1, NBits, [Elem|Result]).

pointer_deref_int8(Pointer) ->
	Code = int_to_bytelist(1),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 8);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_array(Pointer, Index) ->
	Code = int_to_bytelist(2),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 8);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_assign(Pointer, Value) ->
	Code = int_to_bytelist(3),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 8),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(4),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 8),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int8() ->
	Code = int_to_bytelist(5),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int8_array(Size) ->
	Code = int_to_bytelist(6),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_int8(Pointer) ->
	Code = int_to_bytelist(7),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_int8_array(List) ->
	Size = length(List),
	Pointer = new_int8_array(Size),
	list_to_int8_array(List, Pointer, 0).
list_to_int8_array([], Pointer, _Index) -> Pointer;
list_to_int8_array([Value|List], Pointer, Index) ->
	pointer_deref_int8_array_assign(Pointer, Index, Value),
	list_to_int8_array(List, Pointer, Index+1).

int8_array_to_list(Pointer, Size) ->
	int8_array_to_list(Pointer, Size-1, []).
int8_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
int8_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_int8_array(Pointer, Size),
	int8_array_to_list(Pointer, Size-1, [Elem|Result]).

pointer_deref_int16(Pointer) ->
	Code = int_to_bytelist(8),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 16);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_array(Pointer, Index) ->
	Code = int_to_bytelist(9),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 16);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_assign(Pointer, Value) ->
	Code = int_to_bytelist(10),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 16),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(11),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 16),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int16() ->
	Code = int_to_bytelist(12),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int16_array(Size) ->
	Code = int_to_bytelist(13),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_int16(Pointer) ->
	Code = int_to_bytelist(14),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_int16_array(List) ->
	Size = length(List),
	Pointer = new_int16_array(Size),
	list_to_int16_array(List, Pointer, 0).
list_to_int16_array([], Pointer, _Index) -> Pointer;
list_to_int16_array([Value|List], Pointer, Index) ->
	pointer_deref_int16_array_assign(Pointer, Index, Value),
	list_to_int16_array(List, Pointer, Index+1).

int16_array_to_list(Pointer, Size) ->
	int16_array_to_list(Pointer, Size-1, []).
int16_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
int16_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_int16_array(Pointer, Size),
	int16_array_to_list(Pointer, Size-1, [Elem|Result]).

pointer_deref_int32(Pointer) ->
	Code = int_to_bytelist(15),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 32);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_array(Pointer, Index) ->
	Code = int_to_bytelist(16),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 32);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_assign(Pointer, Value) ->
	Code = int_to_bytelist(17),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 32),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(18),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 32),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int32() ->
	Code = int_to_bytelist(19),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int32_array(Size) ->
	Code = int_to_bytelist(20),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_int32(Pointer) ->
	Code = int_to_bytelist(21),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_int32_array(List) ->
	Size = length(List),
	Pointer = new_int32_array(Size),
	list_to_int32_array(List, Pointer, 0).
list_to_int32_array([], Pointer, _Index) -> Pointer;
list_to_int32_array([Value|List], Pointer, Index) ->
	pointer_deref_int32_array_assign(Pointer, Index, Value),
	list_to_int32_array(List, Pointer, Index+1).

int32_array_to_list(Pointer, Size) ->
	int32_array_to_list(Pointer, Size-1, []).
int32_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
int32_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_int32_array(Pointer, Size),
	int32_array_to_list(Pointer, Size-1, [Elem|Result]).

pointer_deref_int64(Pointer) ->
	Code = int_to_bytelist(22),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 64);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_array(Pointer, Index) ->
	Code = int_to_bytelist(23),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_int(DataList, 64);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_assign(Pointer, Value) ->
	Code = int_to_bytelist(24),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 64),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(25),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 64),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int64() ->
	Code = int_to_bytelist(26),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int64_array(Size) ->
	Code = int_to_bytelist(27),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_int64(Pointer) ->
	Code = int_to_bytelist(28),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_int64_array(List) ->
	Size = length(List),
	Pointer = new_int64_array(Size),
	list_to_int64_array(List, Pointer, 0).
list_to_int64_array([], Pointer, _Index) -> Pointer;
list_to_int64_array([Value|List], Pointer, Index) ->
	pointer_deref_int64_array_assign(Pointer, Index, Value),
	list_to_int64_array(List, Pointer, Index+1).

int64_array_to_list(Pointer, Size) ->
	int64_array_to_list(Pointer, Size-1, []).
int64_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
int64_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_int64_array(Pointer, Size),
	int64_array_to_list(Pointer, Size-1, [Elem|Result]).

pointer_deref_int(Pointer) ->
	pointer_deref_int32(Pointer).

pointer_deref_int_array(Pointer, Index) ->
	pointer_deref_int32_array(Pointer, Index).

pointer_deref_int_assign(Pointer, Value) ->
	pointer_deref_int32_assign(Pointer, Value).

pointer_deref_int_array_assign(Pointer, Index, Value) ->
	pointer_deref_int32_array_assign(Pointer, Index, Value).

new_int() ->
	new_int32().

new_int_array(Size) ->
	new_int32_array(Size).

delete_int(Pointer) ->
	delete_int32(Pointer).

list_to_int_array(List) ->
	list_to_int32_array(List).

int_array_to_list(Pointer, Size) ->
	int32_array_to_list(Pointer, Size).

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

float_array_to_bytelist(List, Size) when length(List)==Size ->
	[float_to_bytelist(E) || E<-List].

bytelist_to_float_array(Bytelist, Size) ->
	bytelist_to_float_array(Bytelist, Size, []).
bytelist_to_float_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_float_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_float(Bytelist),
	bytelist_to_float_array(Rest, Size-1, [Elem|Result]).

parse_float_array(Bytelist, Size) ->
	parse_float_array(Bytelist, Size, []).
parse_float_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_float_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_float(Bytelist),
	parse_float_array(Rest, Size-1, [Elem|Result]).

pointer_deref_float(Pointer) ->
	Code = int_to_bytelist(29),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_array(Pointer, Index) ->
	Code = int_to_bytelist(30),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_assign(Pointer, Value) ->
	Code = int_to_bytelist(31),
	PList = pointer_to_bytelist(Pointer),
	VList = float_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(32),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = float_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_float() ->
	Code = int_to_bytelist(33),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_float_array(Size) ->
	Code = int_to_bytelist(34),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_float(Pointer) ->
	Code = int_to_bytelist(35),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_float_array(List) ->
	Size = length(List),
	Pointer = new_float_array(Size),
	list_to_float_array(List, Pointer, 0).
list_to_float_array([], Pointer, _Index) -> Pointer;
list_to_float_array([Value|List], Pointer, Index) ->
	pointer_deref_float_array_assign(Pointer, Index, Value),
	list_to_float_array(List, Pointer, Index+1).

float_array_to_list(Pointer, Size) ->
	float_array_to_list(Pointer, Size-1, []).
float_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
float_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_float_array(Pointer, Size),
	float_array_to_list(Pointer, Size-1, [Elem|Result]).

% ---- Double ----

double_to_bytelist(Value) ->
	float_to_bytelist(Value, 64).

bytelist_to_double(Bytelist) ->
	bytelist_to_float(Bytelist, 64).

parse_double(Bytelist) ->
	parse_float(Bytelist, 64).

double_array_to_bytelist(List, Size) when length(List)==Size ->
	[double_to_bytelist(E) || E<-List].

bytelist_to_double_array(Bytelist, Size) ->
	bytelist_to_double_array(Bytelist, Size, []).
bytelist_to_double_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_double_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_double(Bytelist),
	bytelist_to_double_array(Rest, Size-1, [Elem|Result]).

parse_double_array(Bytelist, Size) ->
	parse_double_array(Bytelist, Size, []).
parse_double_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_double_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_double(Bytelist),
	parse_double_array(Rest, Size-1, [Elem|Result]).

pointer_deref_double(Pointer) ->
	Code = int_to_bytelist(36),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_double(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_array(Pointer, Index) ->
	Code = int_to_bytelist(37),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_double(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_assign(Pointer, Value) ->
	Code = int_to_bytelist(38),
	PList = pointer_to_bytelist(Pointer),
	VList = double_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(39),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = double_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_double() ->
	Code = int_to_bytelist(40),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_double_array(Size) ->
	Code = int_to_bytelist(41),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_double(Pointer) ->
	Code = int_to_bytelist(42),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_double_array(List) ->
	Size = length(List),
	Pointer = new_double_array(Size),
	list_to_double_array(List, Pointer, 0).
list_to_double_array([], Pointer, _Index) -> Pointer;
list_to_double_array([Value|List], Pointer, Index) ->
	pointer_deref_double_array_assign(Pointer, Index, Value),
	list_to_double_array(List, Pointer, Index+1).

double_array_to_list(Pointer, Size) ->
	double_array_to_list(Pointer, Size-1, []).
double_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
double_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_double_array(Pointer, Size),
	double_array_to_list(Pointer, Size-1, [Elem|Result]).

% ---- String ----

string_to_bytelist(Value, Encoding) ->
	binary:bin_to_list(unicode:characters_to_binary(Value, Encoding))++[$\0].

bytelist_to_string(Bytelist, Encoding) ->
	unicode:characters_to_list(binary:list_to_bin(Bytelist), Encoding).

string_to_bytelist(Value) ->
	string_to_bytelist(Value, utf8).

bytelist_to_string(Bytelist) ->
	bytelist_to_string(Bytelist, utf8).

parse_string(Bytelist, Enconding) ->
	parse_string(Bytelist, Enconding, []).
parse_string([$\0|Rest], Enconding, Result) ->
	{bytelist_to_string(lists:reverse(Result), Enconding), Rest};
parse_string([B|Rest], Enconding, Result) ->
	parse_string(Rest, Enconding, [B|Result]).

parse_string(Bytelist) ->
	parse_string(Bytelist, utf8).

string_array_to_bytelist(List, Size, Encoding) when length(List)==Size ->
	[string_to_bytelist(E, Encoding) || E<-List].

bytelist_to_string_array(Bytelist, Size, Encoding) ->
	bytelist_to_string_array(Bytelist, Size, Encoding, []).
bytelist_to_string_array(_Bytelist, 0, _Encoding, Result) ->
	lists:reverse(Result);
bytelist_to_string_array(Bytelist, Size, Encoding, Result) ->
	{Elem, Rest} = parse_string(Bytelist, Encoding),
	bytelist_to_string_array(Rest, Size-1, Encoding, [Elem|Result]).

parse_string_array(Bytelist, Size, Encoding) ->
	parse_string_array(Bytelist, Size, Encoding, []).
parse_string_array(Bytelist, 0, _Encoding, Result) ->
	{lists:reverse(Result), Bytelist};
parse_string_array(Bytelist, Size, Encoding, Result) ->
	{Elem, Rest} = parse_string(Bytelist, Encoding),
	parse_string_array(Rest, Size-1, Encoding, [Elem|Result]).

string_array_to_bytelist(List, Size) when length(List)==Size ->
	string_array_to_bytelist(List, Size, utf8).

bytelist_to_string_array(Bytelist, Size) ->
	bytelist_to_string_array(Bytelist, Size, utf8).

parse_string_array(Bytelist, Size) ->
	parse_string_array(Bytelist, Size, utf8).

pointer_deref_string(Pointer, Enconding) ->
	Code = int_to_bytelist(43),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_string(DataList, Enconding);
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_array(Pointer, Index, Enconding) ->
	Code = int_to_bytelist(44),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_string(DataList, Enconding);
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_assign(Pointer, Value, Enconding) ->
	Code = int_to_bytelist(45),
	PList = pointer_to_bytelist(Pointer),
	VList = string_to_bytelist(Value, Enconding),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_array_assign(Pointer, Index, Value, Enconding) ->
	Code = int_to_bytelist(46),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = string_to_bytelist(Value, Enconding),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_string() ->
	Code = int_to_bytelist(47),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_string_array(Size) ->
	Code = int_to_bytelist(48),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_string(Pointer) ->
	Code = int_to_bytelist(49),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_string_array(List, Enconding) ->
	Size = length(List),
	Pointer = new_string_array(Size),
	list_to_string_array(List, Enconding, Pointer, 0).
list_to_string_array([], _Enconding, Pointer, _Index) -> Pointer;
list_to_string_array([Value|List], Enconding, Pointer, Index) ->
	pointer_deref_string_array_assign(Pointer, Index, Value, Enconding),
	list_to_string_array(List, Enconding, Pointer, Index+1).

string_array_to_list(Pointer, Size, Enconding) ->
	string_array_to_list(Pointer, Size-1, Enconding, []).
string_array_to_list(_Pointer, Size, _Enconding, Result) when Size<0 -> Result;
string_array_to_list(Pointer, Size, Enconding, Result) ->
	Elem = pointer_deref_string_array(Pointer, Size, Enconding),
	string_array_to_list(Pointer, Size-1, Enconding, [Elem|Result]).

pointer_deref_string(Pointer) ->
	pointer_deref_string(Pointer, utf8).

pointer_deref_string_array(Pointer, Index) ->
	pointer_deref_string_array(Pointer, Index, utf8).

pointer_deref_string_assign(Pointer, Value) ->
	pointer_deref_string_assign(Pointer, Value, utf8).

pointer_deref_string_array_assign(Pointer, Index, Value) ->
	pointer_deref_string_array_assign(Pointer, Index, Value, utf8).

list_to_string_array(List) ->
	list_to_string_array(List, utf8).

string_array_to_list(Pointer, Size) ->
	string_array_to_list(Pointer, Size, utf8).

% ---- Pointer ----

pointer_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_pointer(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_pointer(Bytelist) ->
	parse_int(Bytelist, 64).

pointer_array_to_bytelist(List, Size) when length(List)==Size ->
	[pointer_to_bytelist(E) || E<-List].

bytelist_to_pointer_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size, []).
bytelist_to_pointer_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_pointer_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_pointer(Bytelist),
	bytelist_to_pointer_array(Rest, Size-1, [Elem|Result]).

parse_pointer_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size, []).
parse_pointer_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_pointer_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_pointer(Bytelist),
	parse_pointer_array(Rest, Size-1, [Elem|Result]).

pointer_deref_pointer(Pointer) ->
	Code = int_to_bytelist(50),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_array(Pointer, Index) ->
	Code = int_to_bytelist(51),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	sdl_port ! {self(), {command, [Code, PList, IList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_assign(Pointer, Value) ->
	Code = int_to_bytelist(52),
	PList = pointer_to_bytelist(Pointer),
	VList = pointer_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(53),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = pointer_to_bytelist(Value),
	sdl_port ! {self(), {command, [Code, PList, IList, VList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_pointer() ->
	Code = int_to_bytelist(54),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_pointer_array(Size) ->
	Code = int_to_bytelist(55),
	SList = int_to_bytelist(Size),
	sdl_port ! {self(), {command, [Code, SList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_pointer(Pointer) ->
	Code = int_to_bytelist(56),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_pointer_array(List) ->
	Size = length(List),
	Pointer = new_pointer_array(Size),
	list_to_pointer_array(List, Pointer, 0).
list_to_pointer_array([], Pointer, _Index) -> Pointer;
list_to_pointer_array([Value|List], Pointer, Index) ->
	pointer_deref_pointer_array_assign(Pointer, Index, Value),
	list_to_pointer_array(List, Pointer, Index+1).

pointer_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size-1, []).
pointer_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
pointer_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_pointer_array(Pointer, Size),
	pointer_array_to_list(Pointer, Size-1, [Elem|Result]).

