-module(sdl_ports_gen).
-compile(export_all).

-export([init/1, quit/0, create_window/6, get_window_surface/1, load_bmp/1, free_surface/1, blit_surface/4, blit_scaled/4, update_window_surface/1, destroy_window/1, get_window_size/1, get_error/0, poll_event/0]).

-define(SDL_INIT_TIMER, 16#00000001).
-define(SDL_INIT_AUDIO, 16#00000010).
-define(SDL_INIT_VIDEO, 16#00000020).


init_port() ->
	Port = open_port({spawn, "./sdl_handler"}, [{packet, 2}]),
	register(sdl_port, Port), Port.

%--------------------------------------------------------

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

double_to_bytelist(Value) ->
	float_to_bytelist(Value, 64).

bytelist_to_double(Bytelist) ->
	bytelist_to_float(Bytelist, 64).

parse_double(Bytelist) ->
	parse_float(Bytelist, 64).

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

pointer_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_pointer(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_pointer(Bytelist) ->
	parse_int(Bytelist, 64).

%--------------------------------------------------------

uint32_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

bytelist_to_uint32(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

parse_uint32(Bytelist) ->
	parse_int(Bytelist, 32).

uint8_to_bytelist(Value) ->
	int_to_bytelist(Value, 8).

bytelist_to_uint8(Bytelist) ->
	bytelist_to_int(Bytelist, 8).

parse_uint8(Bytelist) ->
	parse_int(Bytelist, 8).

window_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_window(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_window(Bytelist) ->
	parse_pointer(Bytelist).

-record(color, {r, g, b, a}).

color_to_bytelist(Value) ->
	Return = [uint8_to_bytelist(Value#color.r),
	uint8_to_bytelist(Value#color.g),
	uint8_to_bytelist(Value#color.b),
	uint8_to_bytelist(Value#color.a)],
	lists:flatten(Return).

bytelist_to_color(Bytelist) ->
	R0 = Bytelist,
	{R, R1} = parse_uint8(R0),
	{G, R2} = parse_uint8(R1),
	{B, R3} = parse_uint8(R2),
	{A, R4} = parse_uint8(R3),
	#color{r=R, g=G, b=B, a=A}.

parse_color(Bytelist) ->
	R0 = Bytelist,
	{R, R1} = parse_uint8(R0),
	{G, R2} = parse_uint8(R1),
	{B, R3} = parse_uint8(R2),
	{A, R4} = parse_uint8(R3),
	{#color{r=R, g=G, b=B, a=A}, R4}.

-record(palette, {ncolors, colors, version, refcount}).

palette_to_bytelist(Value) ->
	Return = [int_to_bytelist(Value#palette.ncolors),
	pointer_to_bytelist(Value#palette.colors),
	uint32_to_bytelist(Value#palette.version),
	int_to_bytelist(Value#palette.refcount)],
	lists:flatten(Return).

bytelist_to_palette(Bytelist) ->
	R0 = Bytelist,
	{Ncolors, R1} = parse_int(R0),
	{Colors, R2} = parse_pointer(R1),
	{Version, R3} = parse_uint32(R2),
	{Refcount, R4} = parse_int(R3),
	#palette{ncolors=Ncolors, colors=Colors, version=Version, refcount=Refcount}.

parse_palette(Bytelist) ->
	R0 = Bytelist,
	{Ncolors, R1} = parse_int(R0),
	{Colors, R2} = parse_pointer(R1),
	{Version, R3} = parse_uint32(R2),
	{Refcount, R4} = parse_int(R3),
	{#palette{ncolors=Ncolors, colors=Colors, version=Version, refcount=Refcount}, R4}.

-record(pixel_format, {format, palette, bits_per_pixel, bytes_per_pixel, r_mask, g_mask, b_mask, a_mask, r_loss, g_loss, b_loss, a_loss, r_shift, g_shift, b_shift, a_shift, refcount, next}).

pixel_format_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#pixel_format.format),
	pointer_to_bytelist(Value#pixel_format.palette),
	uint8_to_bytelist(Value#pixel_format.bits_per_pixel),
	uint8_to_bytelist(Value#pixel_format.bytes_per_pixel),
	uint32_to_bytelist(Value#pixel_format.r_mask),
	uint32_to_bytelist(Value#pixel_format.g_mask),
	uint32_to_bytelist(Value#pixel_format.b_mask),
	uint32_to_bytelist(Value#pixel_format.a_mask),
	uint8_to_bytelist(Value#pixel_format.r_loss),
	uint8_to_bytelist(Value#pixel_format.g_loss),
	uint8_to_bytelist(Value#pixel_format.b_loss),
	uint8_to_bytelist(Value#pixel_format.a_loss),
	uint8_to_bytelist(Value#pixel_format.r_shift),
	uint8_to_bytelist(Value#pixel_format.g_shift),
	uint8_to_bytelist(Value#pixel_format.b_shift),
	uint8_to_bytelist(Value#pixel_format.a_shift),
	int_to_bytelist(Value#pixel_format.refcount),
	pointer_to_bytelist(Value#pixel_format.next)],
	lists:flatten(Return).

bytelist_to_pixel_format(Bytelist) ->
	R0 = Bytelist,
	{Format, R1} = parse_uint32(R0),
	{Palette, R2} = parse_pointer(R1),
	{Bits_per_pixel, R3} = parse_uint8(R2),
	{Bytes_per_pixel, R4} = parse_uint8(R3),
	{R_mask, R5} = parse_uint32(R4),
	{G_mask, R6} = parse_uint32(R5),
	{B_mask, R7} = parse_uint32(R6),
	{A_mask, R8} = parse_uint32(R7),
	{R_loss, R9} = parse_uint8(R8),
	{G_loss, R10} = parse_uint8(R9),
	{B_loss, R11} = parse_uint8(R10),
	{A_loss, R12} = parse_uint8(R11),
	{R_shift, R13} = parse_uint8(R12),
	{G_shift, R14} = parse_uint8(R13),
	{B_shift, R15} = parse_uint8(R14),
	{A_shift, R16} = parse_uint8(R15),
	{Refcount, R17} = parse_int(R16),
	{Next, R18} = parse_pointer(R17),
	#pixel_format{format=Format, palette=Palette, bits_per_pixel=Bits_per_pixel, bytes_per_pixel=Bytes_per_pixel, r_mask=R_mask, g_mask=G_mask, b_mask=B_mask, a_mask=A_mask, r_loss=R_loss, g_loss=G_loss, b_loss=B_loss, a_loss=A_loss, r_shift=R_shift, g_shift=G_shift, b_shift=B_shift, a_shift=A_shift, refcount=Refcount, next=Next}.

parse_pixel_format(Bytelist) ->
	R0 = Bytelist,
	{Format, R1} = parse_uint32(R0),
	{Palette, R2} = parse_pointer(R1),
	{Bits_per_pixel, R3} = parse_uint8(R2),
	{Bytes_per_pixel, R4} = parse_uint8(R3),
	{R_mask, R5} = parse_uint32(R4),
	{G_mask, R6} = parse_uint32(R5),
	{B_mask, R7} = parse_uint32(R6),
	{A_mask, R8} = parse_uint32(R7),
	{R_loss, R9} = parse_uint8(R8),
	{G_loss, R10} = parse_uint8(R9),
	{B_loss, R11} = parse_uint8(R10),
	{A_loss, R12} = parse_uint8(R11),
	{R_shift, R13} = parse_uint8(R12),
	{G_shift, R14} = parse_uint8(R13),
	{B_shift, R15} = parse_uint8(R14),
	{A_shift, R16} = parse_uint8(R15),
	{Refcount, R17} = parse_int(R16),
	{Next, R18} = parse_pointer(R17),
	{#pixel_format{format=Format, palette=Palette, bits_per_pixel=Bits_per_pixel, bytes_per_pixel=Bytes_per_pixel, r_mask=R_mask, g_mask=G_mask, b_mask=B_mask, a_mask=A_mask, r_loss=R_loss, g_loss=G_loss, b_loss=B_loss, a_loss=A_loss, r_shift=R_shift, g_shift=G_shift, b_shift=B_shift, a_shift=A_shift, refcount=Refcount, next=Next}, R18}.

-record(rect, {x, y, w, h}).

rect_to_bytelist(Value) ->
	Return = [int_to_bytelist(Value#rect.x),
	int_to_bytelist(Value#rect.y),
	int_to_bytelist(Value#rect.w),
	int_to_bytelist(Value#rect.h)],
	lists:flatten(Return).

bytelist_to_rect(Bytelist) ->
	R0 = Bytelist,
	{X, R1} = parse_int(R0),
	{Y, R2} = parse_int(R1),
	{W, R3} = parse_int(R2),
	{H, R4} = parse_int(R3),
	#rect{x=X, y=Y, w=W, h=H}.

parse_rect(Bytelist) ->
	R0 = Bytelist,
	{X, R1} = parse_int(R0),
	{Y, R2} = parse_int(R1),
	{W, R3} = parse_int(R2),
	{H, R4} = parse_int(R3),
	{#rect{x=X, y=Y, w=W, h=H}, R4}.

blit_map_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_blit_map(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_blit_map(Bytelist) ->
	parse_pointer(Bytelist).

-record(surface, {flags, format, w, h, pitch, pixels, userdata, locked, lock_data, clip_rect, map, refcount}).

surface_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#surface.flags),
	pointer_to_bytelist(Value#surface.format),
	int_to_bytelist(Value#surface.w),
	int_to_bytelist(Value#surface.h),
	int_to_bytelist(Value#surface.pitch),
	pointer_to_bytelist(Value#surface.pixels),
	pointer_to_bytelist(Value#surface.userdata),
	int_to_bytelist(Value#surface.locked),
	pointer_to_bytelist(Value#surface.lock_data),
	rect_to_bytelist(Value#surface.clip_rect),
	pointer_to_bytelist(Value#surface.map),
	int_to_bytelist(Value#surface.refcount)],
	lists:flatten(Return).

bytelist_to_surface(Bytelist) ->
	R0 = Bytelist,
	{Flags, R1} = parse_uint32(R0),
	{Format, R2} = parse_pointer(R1),
	{W, R3} = parse_int(R2),
	{H, R4} = parse_int(R3),
	{Pitch, R5} = parse_int(R4),
	{Pixels, R6} = parse_pointer(R5),
	{Userdata, R7} = parse_pointer(R6),
	{Locked, R8} = parse_int(R7),
	{Lock_data, R9} = parse_pointer(R8),
	{Clip_rect, R10} = parse_rect(R9),
	{Map, R11} = parse_pointer(R10),
	{Refcount, R12} = parse_int(R11),
	#surface{flags=Flags, format=Format, w=W, h=H, pitch=Pitch, pixels=Pixels, userdata=Userdata, locked=Locked, lock_data=Lock_data, clip_rect=Clip_rect, map=Map, refcount=Refcount}.

parse_surface(Bytelist) ->
	R0 = Bytelist,
	{Flags, R1} = parse_uint32(R0),
	{Format, R2} = parse_pointer(R1),
	{W, R3} = parse_int(R2),
	{H, R4} = parse_int(R3),
	{Pitch, R5} = parse_int(R4),
	{Pixels, R6} = parse_pointer(R5),
	{Userdata, R7} = parse_pointer(R6),
	{Locked, R8} = parse_int(R7),
	{Lock_data, R9} = parse_pointer(R8),
	{Clip_rect, R10} = parse_rect(R9),
	{Map, R11} = parse_pointer(R10),
	{Refcount, R12} = parse_int(R11),
	{#surface{flags=Flags, format=Format, w=W, h=H, pitch=Pitch, pixels=Pixels, userdata=Userdata, locked=Locked, lock_data=Lock_data, clip_rect=Clip_rect, map=Map, refcount=Refcount}, R12}.

%--------------------------------------------------------

init(Uint32_1) ->
	Param1 = uint32_to_bytelist(Uint32_1),
	sdl_port ! {self(), {command, [1, Param1]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

quit() ->
	sdl_port ! {self(), {command, [2, []]}},
	receive
		{_, {data, []}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

create_window(String_1, Int_2, Int_3, Int_4, Int_5, Uint32_6) ->
	Param1 = string_to_bytelist(String_1),
	Param2 = int_to_bytelist(Int_2),
	Param3 = int_to_bytelist(Int_3),
	Param4 = int_to_bytelist(Int_4),
	Param5 = int_to_bytelist(Int_5),
	Param6 = uint32_to_bytelist(Uint32_6),
	sdl_port ! {self(), {command, [3, Param1, Param2, Param3, Param4, Param5, Param6]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

get_window_surface(P_Window_1) ->
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [4, Param1]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

load_bmp(String_1) ->
	Param1 = string_to_bytelist(String_1),
	sdl_port ! {self(), {command, [5, Param1]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

free_surface(P_Surface_1) ->
	Param1 = pointer_to_bytelist(P_Surface_1),
	sdl_port ! {self(), {command, [6, Param1]}},
	receive
		{_, {data, []}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

blit_surface(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),
	sdl_port ! {self(), {command, [7, Param1, Param2, Param3, Param4]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

blit_scaled(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),
	sdl_port ! {self(), {command, [8, Param1, Param2, Param3, Param4]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

update_window_surface(P_Window_1) ->
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [9, Param1]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

destroy_window(P_Window_1) ->
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [10, Param1]}},
	receive
		{_, {data, []}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

get_window_size(P_Window_1) ->
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [11, Param1]}},
	receive
		{_, {data, []}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

get_error() ->
	sdl_port ! {self(), {command, [12, []]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

poll_event() ->
	sdl_port ! {self(), {command, [13, []]}},
	receive
		{_, {data, DataList}} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

