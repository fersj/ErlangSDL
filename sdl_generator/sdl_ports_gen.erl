-module(sdl_ports_gen).
-include("sdl_ports_gen.hrl").
-compile(export_all).

-export([
	init_port/0,
	pointer_deref_color/1,
	new_color/0,
	delete_color/1,
	color_get_r/1,
	color_set_r/2,
	color_get_g/1,
	color_set_g/2,
	color_get_b/1,
	color_set_b/2,
	color_get_a/1,
	color_set_a/2,
	pointer_deref_palette/1,
	new_palette/0,
	delete_palette/1,
	palette_get_ncolors/1,
	palette_set_ncolors/2,
	palette_get_colors/1,
	palette_set_colors/2,
	palette_get_version/1,
	palette_set_version/2,
	palette_get_refcount/1,
	palette_set_refcount/2,
	pointer_deref_pixel_format/1,
	new_pixel_format/0,
	delete_pixel_format/1,
	pixel_format_get_format/1,
	pixel_format_set_format/2,
	pixel_format_get_palette/1,
	pixel_format_set_palette/2,
	pixel_format_get_bits_per_pixel/1,
	pixel_format_set_bits_per_pixel/2,
	pixel_format_get_bytes_per_pixel/1,
	pixel_format_set_bytes_per_pixel/2,
	pixel_format_get_r_mask/1,
	pixel_format_set_r_mask/2,
	pixel_format_get_g_mask/1,
	pixel_format_set_g_mask/2,
	pixel_format_get_b_mask/1,
	pixel_format_set_b_mask/2,
	pixel_format_get_a_mask/1,
	pixel_format_set_a_mask/2,
	pixel_format_get_r_loss/1,
	pixel_format_set_r_loss/2,
	pixel_format_get_g_loss/1,
	pixel_format_set_g_loss/2,
	pixel_format_get_b_loss/1,
	pixel_format_set_b_loss/2,
	pixel_format_get_a_loss/1,
	pixel_format_set_a_loss/2,
	pixel_format_get_r_shift/1,
	pixel_format_set_r_shift/2,
	pixel_format_get_g_shift/1,
	pixel_format_set_g_shift/2,
	pixel_format_get_b_shift/1,
	pixel_format_set_b_shift/2,
	pixel_format_get_a_shift/1,
	pixel_format_set_a_shift/2,
	pixel_format_get_refcount/1,
	pixel_format_set_refcount/2,
	pixel_format_get_next/1,
	pixel_format_set_next/2,
	pointer_deref_rect/1,
	new_rect/0,
	delete_rect/1,
	rect_get_x/1,
	rect_set_x/2,
	rect_get_y/1,
	rect_set_y/2,
	rect_get_w/1,
	rect_set_w/2,
	rect_get_h/1,
	rect_set_h/2,
	pointer_deref_surface/1,
	new_surface/0,
	delete_surface/1,
	surface_get_flags/1,
	surface_set_flags/2,
	surface_get_format/1,
	surface_set_format/2,
	surface_get_w/1,
	surface_set_w/2,
	surface_get_h/1,
	surface_set_h/2,
	surface_get_pitch/1,
	surface_set_pitch/2,
	surface_get_pixels/1,
	surface_set_pixels/2,
	surface_get_userdata/1,
	surface_set_userdata/2,
	surface_get_locked/1,
	surface_set_locked/2,
	surface_get_lock_data/1,
	surface_set_lock_data/2,
	surface_get_clip_rect/1,
	surface_set_clip_rect/2,
	surface_get_map/1,
	surface_set_map/2,
	surface_get_refcount/1,
	surface_set_refcount/2,
	pointer_deref_keysym/1,
	new_keysym/0,
	delete_keysym/1,
	keysym_get_scancode/1,
	keysym_set_scancode/2,
	keysym_get_sym/1,
	keysym_set_sym/2,
	keysym_get_mod/1,
	keysym_set_mod/2,
	keysym_get_unused/1,
	keysym_set_unused/2,
	pointer_deref_common_event/1,
	new_common_event/0,
	delete_common_event/1,
	common_event_get_type/1,
	common_event_set_type/2,
	common_event_get_timestamp/1,
	common_event_set_timestamp/2,
	pointer_deref_window_event/1,
	new_window_event/0,
	delete_window_event/1,
	window_event_get_type/1,
	window_event_set_type/2,
	window_event_get_timestamp/1,
	window_event_set_timestamp/2,
	window_event_get_windowID/1,
	window_event_set_windowID/2,
	window_event_get_event/1,
	window_event_set_event/2,
	window_event_get_padding1/1,
	window_event_set_padding1/2,
	window_event_get_padding2/1,
	window_event_set_padding2/2,
	window_event_get_padding3/1,
	window_event_set_padding3/2,
	window_event_get_data1/1,
	window_event_set_data1/2,
	window_event_get_data2/1,
	window_event_set_data2/2,
	pointer_deref_keyboard_event/1,
	new_keyboard_event/0,
	delete_keyboard_event/1,
	keyboard_event_get_type/1,
	keyboard_event_set_type/2,
	keyboard_event_get_timestamp/1,
	keyboard_event_set_timestamp/2,
	keyboard_event_get_windowID/1,
	keyboard_event_set_windowID/2,
	keyboard_event_get_state/1,
	keyboard_event_set_state/2,
	keyboard_event_get_repeat/1,
	keyboard_event_set_repeat/2,
	keyboard_event_get_padding2/1,
	keyboard_event_set_padding2/2,
	keyboard_event_get_padding3/1,
	keyboard_event_set_padding3/2,
	keyboard_event_get_keysym/1,
	keyboard_event_set_keysym/2,
	pointer_deref_text_editing_event/1,
	new_text_editing_event/0,
	delete_text_editing_event/1,
	text_editing_event_get_type/1,
	text_editing_event_set_type/2,
	text_editing_event_get_timestamp/1,
	text_editing_event_set_timestamp/2,
	text_editing_event_get_windowID/1,
	text_editing_event_set_windowID/2,
	text_editing_event_get_text/1,
	text_editing_event_set_text/2,
	text_editing_event_get_start/1,
	text_editing_event_set_start/2,
	text_editing_event_get_length/1,
	text_editing_event_set_length/2,
	pointer_deref_text_input_event/1,
	new_text_input_event/0,
	delete_text_input_event/1,
	text_input_event_get_type/1,
	text_input_event_set_type/2,
	text_input_event_get_timestamp/1,
	text_input_event_set_timestamp/2,
	text_input_event_get_windowID/1,
	text_input_event_set_windowID/2,
	text_input_event_get_text/1,
	text_input_event_set_text/2,
	pointer_deref_mouse_motion_event/1,
	new_mouse_motion_event/0,
	delete_mouse_motion_event/1,
	mouse_motion_event_get_type/1,
	mouse_motion_event_set_type/2,
	mouse_motion_event_get_timestamp/1,
	mouse_motion_event_set_timestamp/2,
	mouse_motion_event_get_windowID/1,
	mouse_motion_event_set_windowID/2,
	mouse_motion_event_get_which/1,
	mouse_motion_event_set_which/2,
	mouse_motion_event_get_state/1,
	mouse_motion_event_set_state/2,
	mouse_motion_event_get_x/1,
	mouse_motion_event_set_x/2,
	mouse_motion_event_get_y/1,
	mouse_motion_event_set_y/2,
	mouse_motion_event_get_xrel/1,
	mouse_motion_event_set_xrel/2,
	mouse_motion_event_get_yrel/1,
	mouse_motion_event_set_yrel/2,
	pointer_deref_mouse_button_event/1,
	new_mouse_button_event/0,
	delete_mouse_button_event/1,
	mouse_button_event_get_type/1,
	mouse_button_event_set_type/2,
	mouse_button_event_get_timestamp/1,
	mouse_button_event_set_timestamp/2,
	mouse_button_event_get_windowID/1,
	mouse_button_event_set_windowID/2,
	mouse_button_event_get_which/1,
	mouse_button_event_set_which/2,
	mouse_button_event_get_button/1,
	mouse_button_event_set_button/2,
	mouse_button_event_get_state/1,
	mouse_button_event_set_state/2,
	mouse_button_event_get_clicks/1,
	mouse_button_event_set_clicks/2,
	mouse_button_event_get_x/1,
	mouse_button_event_set_x/2,
	mouse_button_event_get_y/1,
	mouse_button_event_set_y/2,
	pointer_deref_mouse_wheel_event/1,
	new_mouse_wheel_event/0,
	delete_mouse_wheel_event/1,
	mouse_wheel_event_get_type/1,
	mouse_wheel_event_set_type/2,
	mouse_wheel_event_get_timestamp/1,
	mouse_wheel_event_set_timestamp/2,
	mouse_wheel_event_get_windowID/1,
	mouse_wheel_event_set_windowID/2,
	mouse_wheel_event_get_which/1,
	mouse_wheel_event_set_which/2,
	mouse_wheel_event_get_x/1,
	mouse_wheel_event_set_x/2,
	mouse_wheel_event_get_y/1,
	mouse_wheel_event_set_y/2,
	mouse_wheel_event_get_direction/1,
	mouse_wheel_event_set_direction/2,
	pointer_deref_joy_axis_event/1,
	new_joy_axis_event/0,
	delete_joy_axis_event/1,
	joy_axis_event_get_type/1,
	joy_axis_event_set_type/2,
	joy_axis_event_get_timestamp/1,
	joy_axis_event_set_timestamp/2,
	joy_axis_event_get_which/1,
	joy_axis_event_set_which/2,
	joy_axis_event_get_axis/1,
	joy_axis_event_set_axis/2,
	joy_axis_event_get_padding1/1,
	joy_axis_event_set_padding1/2,
	joy_axis_event_get_padding2/1,
	joy_axis_event_set_padding2/2,
	joy_axis_event_get_padding3/1,
	joy_axis_event_set_padding3/2,
	joy_axis_event_get_value/1,
	joy_axis_event_set_value/2,
	pointer_deref_joy_ball_event/1,
	new_joy_ball_event/0,
	delete_joy_ball_event/1,
	joy_ball_event_get_type/1,
	joy_ball_event_set_type/2,
	joy_ball_event_get_timestamp/1,
	joy_ball_event_set_timestamp/2,
	joy_ball_event_get_which/1,
	joy_ball_event_set_which/2,
	joy_ball_event_get_ball/1,
	joy_ball_event_set_ball/2,
	joy_ball_event_get_padding1/1,
	joy_ball_event_set_padding1/2,
	joy_ball_event_get_padding2/1,
	joy_ball_event_set_padding2/2,
	joy_ball_event_get_padding3/1,
	joy_ball_event_set_padding3/2,
	joy_ball_event_get_xrel/1,
	joy_ball_event_set_xrel/2,
	joy_ball_event_get_yrel/1,
	joy_ball_event_set_yrel/2,
	pointer_deref_joy_hat_event/1,
	new_joy_hat_event/0,
	delete_joy_hat_event/1,
	joy_hat_event_get_type/1,
	joy_hat_event_set_type/2,
	joy_hat_event_get_timestamp/1,
	joy_hat_event_set_timestamp/2,
	joy_hat_event_get_which/1,
	joy_hat_event_set_which/2,
	joy_hat_event_get_hat/1,
	joy_hat_event_set_hat/2,
	joy_hat_event_get_value/1,
	joy_hat_event_set_value/2,
	joy_hat_event_get_padding1/1,
	joy_hat_event_set_padding1/2,
	joy_hat_event_get_padding2/1,
	joy_hat_event_set_padding2/2,
	pointer_deref_joy_button_event/1,
	new_joy_button_event/0,
	delete_joy_button_event/1,
	joy_button_event_get_type/1,
	joy_button_event_set_type/2,
	joy_button_event_get_timestamp/1,
	joy_button_event_set_timestamp/2,
	joy_button_event_get_which/1,
	joy_button_event_set_which/2,
	joy_button_event_get_button/1,
	joy_button_event_set_button/2,
	joy_button_event_get_state/1,
	joy_button_event_set_state/2,
	joy_button_event_get_padding1/1,
	joy_button_event_set_padding1/2,
	joy_button_event_get_padding2/1,
	joy_button_event_set_padding2/2,
	pointer_deref_joy_device_event/1,
	new_joy_device_event/0,
	delete_joy_device_event/1,
	joy_device_event_get_type/1,
	joy_device_event_set_type/2,
	joy_device_event_get_timestamp/1,
	joy_device_event_set_timestamp/2,
	joy_device_event_get_which/1,
	joy_device_event_set_which/2,
	pointer_deref_controller_axis_event/1,
	new_controller_axis_event/0,
	delete_controller_axis_event/1,
	controller_axis_event_get_type/1,
	controller_axis_event_set_type/2,
	controller_axis_event_get_timestamp/1,
	controller_axis_event_set_timestamp/2,
	controller_axis_event_get_which/1,
	controller_axis_event_set_which/2,
	controller_axis_event_get_axis/1,
	controller_axis_event_set_axis/2,
	controller_axis_event_get_padding1/1,
	controller_axis_event_set_padding1/2,
	controller_axis_event_get_padding2/1,
	controller_axis_event_set_padding2/2,
	controller_axis_event_get_padding3/1,
	controller_axis_event_set_padding3/2,
	controller_axis_event_get_value/1,
	controller_axis_event_set_value/2,
	controller_axis_event_get_padding4/1,
	controller_axis_event_set_padding4/2,
	pointer_deref_controller_button_event/1,
	new_controller_button_event/0,
	delete_controller_button_event/1,
	controller_button_event_get_type/1,
	controller_button_event_set_type/2,
	controller_button_event_get_timestamp/1,
	controller_button_event_set_timestamp/2,
	controller_button_event_get_which/1,
	controller_button_event_set_which/2,
	controller_button_event_get_button/1,
	controller_button_event_set_button/2,
	controller_button_event_get_state/1,
	controller_button_event_set_state/2,
	controller_button_event_get_padding1/1,
	controller_button_event_set_padding1/2,
	controller_button_event_get_padding2/1,
	controller_button_event_set_padding2/2,
	pointer_deref_controller_device_event/1,
	new_controller_device_event/0,
	delete_controller_device_event/1,
	controller_device_event_get_type/1,
	controller_device_event_set_type/2,
	controller_device_event_get_timestamp/1,
	controller_device_event_set_timestamp/2,
	controller_device_event_get_which/1,
	controller_device_event_set_which/2,
	pointer_deref_audio_device_event/1,
	new_audio_device_event/0,
	delete_audio_device_event/1,
	audio_device_event_get_type/1,
	audio_device_event_set_type/2,
	audio_device_event_get_timestamp/1,
	audio_device_event_set_timestamp/2,
	audio_device_event_get_which/1,
	audio_device_event_set_which/2,
	audio_device_event_get_iscapture/1,
	audio_device_event_set_iscapture/2,
	audio_device_event_get_padding1/1,
	audio_device_event_set_padding1/2,
	audio_device_event_get_padding2/1,
	audio_device_event_set_padding2/2,
	audio_device_event_get_padding3/1,
	audio_device_event_set_padding3/2,
	pointer_deref_quit_event/1,
	new_quit_event/0,
	delete_quit_event/1,
	quit_event_get_type/1,
	quit_event_set_type/2,
	quit_event_get_timestamp/1,
	quit_event_set_timestamp/2,
	pointer_deref_user_event/1,
	new_user_event/0,
	delete_user_event/1,
	user_event_get_type/1,
	user_event_set_type/2,
	user_event_get_timestamp/1,
	user_event_set_timestamp/2,
	user_event_get_windowID/1,
	user_event_set_windowID/2,
	user_event_get_code/1,
	user_event_set_code/2,
	user_event_get_data1/1,
	user_event_set_data1/2,
	user_event_get_data2/1,
	user_event_set_data2/2,
	pointer_deref_syswm_event/1,
	new_syswm_event/0,
	delete_syswm_event/1,
	syswm_event_get_type/1,
	syswm_event_set_type/2,
	syswm_event_get_timestamp/1,
	syswm_event_set_timestamp/2,
	syswm_event_get_msg/1,
	syswm_event_set_msg/2,
	pointer_deref_touch_finger_event/1,
	new_touch_finger_event/0,
	delete_touch_finger_event/1,
	touch_finger_event_get_type/1,
	touch_finger_event_set_type/2,
	touch_finger_event_get_timestamp/1,
	touch_finger_event_set_timestamp/2,
	touch_finger_event_get_touchId/1,
	touch_finger_event_set_touchId/2,
	touch_finger_event_get_fingerId/1,
	touch_finger_event_set_fingerId/2,
	touch_finger_event_get_x/1,
	touch_finger_event_set_x/2,
	touch_finger_event_get_y/1,
	touch_finger_event_set_y/2,
	touch_finger_event_get_dx/1,
	touch_finger_event_set_dx/2,
	touch_finger_event_get_dy/1,
	touch_finger_event_set_dy/2,
	touch_finger_event_get_pressure/1,
	touch_finger_event_set_pressure/2,
	pointer_deref_multi_gesture_event/1,
	new_multi_gesture_event/0,
	delete_multi_gesture_event/1,
	multi_gesture_event_get_type/1,
	multi_gesture_event_set_type/2,
	multi_gesture_event_get_timestamp/1,
	multi_gesture_event_set_timestamp/2,
	multi_gesture_event_get_touchId/1,
	multi_gesture_event_set_touchId/2,
	multi_gesture_event_get_dTheta/1,
	multi_gesture_event_set_dTheta/2,
	multi_gesture_event_get_dDist/1,
	multi_gesture_event_set_dDist/2,
	multi_gesture_event_get_x/1,
	multi_gesture_event_set_x/2,
	multi_gesture_event_get_y/1,
	multi_gesture_event_set_y/2,
	multi_gesture_event_get_numFingers/1,
	multi_gesture_event_set_numFingers/2,
	multi_gesture_event_get_padding/1,
	multi_gesture_event_set_padding/2,
	pointer_deref_dollar_gesture_event/1,
	new_dollar_gesture_event/0,
	delete_dollar_gesture_event/1,
	dollar_gesture_event_get_type/1,
	dollar_gesture_event_set_type/2,
	dollar_gesture_event_get_timestamp/1,
	dollar_gesture_event_set_timestamp/2,
	dollar_gesture_event_get_touchId/1,
	dollar_gesture_event_set_touchId/2,
	dollar_gesture_event_get_gestureId/1,
	dollar_gesture_event_set_gestureId/2,
	dollar_gesture_event_get_numFingers/1,
	dollar_gesture_event_set_numFingers/2,
	dollar_gesture_event_get_error/1,
	dollar_gesture_event_set_error/2,
	dollar_gesture_event_get_x/1,
	dollar_gesture_event_set_x/2,
	dollar_gesture_event_get_y/1,
	dollar_gesture_event_set_y/2,
	pointer_deref_drop_event/1,
	new_drop_event/0,
	delete_drop_event/1,
	drop_event_get_type/1,
	drop_event_set_type/2,
	drop_event_get_timestamp/1,
	drop_event_set_timestamp/2,
	drop_event_get_file/1,
	drop_event_set_file/2,
	drop_event_get_windowID/1,
	drop_event_set_windowID/2,
	event_get_type/1,
	event_set_type/2,
	event_get_common/1,
	event_set_common/2,
	event_get_window/1,
	event_set_window/2,
	event_get_key/1,
	event_set_key/2,
	event_get_edit/1,
	event_set_edit/2,
	event_get_text/1,
	event_set_text/2,
	event_get_motion/1,
	event_set_motion/2,
	event_get_button/1,
	event_set_button/2,
	event_get_wheel/1,
	event_set_wheel/2,
	event_get_jaxis/1,
	event_set_jaxis/2,
	event_get_jball/1,
	event_set_jball/2,
	event_get_jhat/1,
	event_set_jhat/2,
	event_get_jbutton/1,
	event_set_jbutton/2,
	event_get_jdevice/1,
	event_set_jdevice/2,
	event_get_caxis/1,
	event_set_caxis/2,
	event_get_cbutton/1,
	event_set_cbutton/2,
	event_get_cdevice/1,
	event_set_cdevice/2,
	event_get_adevice/1,
	event_set_adevice/2,
	event_get_quit/1,
	event_set_quit/2,
	event_get_user/1,
	event_set_user/2,
	event_get_syswm/1,
	event_set_syswm/2,
	event_get_tfinger/1,
	event_set_tfinger/2,
	event_get_mgesture/1,
	event_set_mgesture/2,
	event_get_dgesture/1,
	event_set_dgesture/2,
	event_get_drop/1,
	event_set_drop/2,
	init/1,
	quit/0,
	create_window/6,
	get_window_surface/1,
	load_bmp/1,
	free_surface/1,
	blit_surface/4,
	blit_scaled/4,
	update_window_surface/1,
	destroy_window/1,
	get_window_size/1,
	get_error/0,
	poll_event/0]).

init_port() ->
	Port = open_port({spawn, "_checkouts/sdl_generator/sdl_ports_gen"}, [{packet, 2}]),
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

uint64_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_uint64(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_uint64(Bytelist) ->
	parse_int(Bytelist, 64).

uint32_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

bytelist_to_uint32(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

parse_uint32(Bytelist) ->
	parse_int(Bytelist, 32).

uint16_to_bytelist(Value) ->
	int_to_bytelist(Value, 16).

bytelist_to_uint16(Bytelist) ->
	bytelist_to_int(Bytelist, 16).

parse_uint16(Bytelist) ->
	parse_int(Bytelist, 16).

uint8_to_bytelist(Value) ->
	int_to_bytelist(Value, 8).

bytelist_to_uint8(Bytelist) ->
	bytelist_to_int(Bytelist, 8).

parse_uint8(Bytelist) ->
	parse_int(Bytelist, 8).

sint64_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_sint64(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_sint64(Bytelist) ->
	parse_int(Bytelist, 64).

sint32_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

bytelist_to_sint32(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

parse_sint32(Bytelist) ->
	parse_int(Bytelist, 32).

sint16_to_bytelist(Value) ->
	int_to_bytelist(Value, 16).

bytelist_to_sint16(Bytelist) ->
	bytelist_to_int(Bytelist, 16).

parse_sint16(Bytelist) ->
	parse_int(Bytelist, 16).

sint8_to_bytelist(Value) ->
	int_to_bytelist(Value, 8).

bytelist_to_sint8(Bytelist) ->
	bytelist_to_int(Bytelist, 8).

parse_sint8(Bytelist) ->
	parse_int(Bytelist, 8).

window_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_window(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_window(Bytelist) ->
	parse_pointer(Bytelist).

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

pointer_deref_color(Pointer) ->
	Code = int_to_bytelist(1),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_color(DataList);
		Msg ->
			{error, Msg}
	end.

new_color() ->
	Code = int_to_bytelist(2),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_color(Pointer) ->
	Code = int_to_bytelist(3),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_r(Pointer) ->
	Code = int_to_bytelist(4),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_r(Pointer, Attrib) ->
	Code = int_to_bytelist(5),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_g(Pointer) ->
	Code = int_to_bytelist(6),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_g(Pointer, Attrib) ->
	Code = int_to_bytelist(7),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_b(Pointer) ->
	Code = int_to_bytelist(8),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_b(Pointer, Attrib) ->
	Code = int_to_bytelist(9),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_a(Pointer) ->
	Code = int_to_bytelist(10),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_a(Pointer, Attrib) ->
	Code = int_to_bytelist(11),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

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

pointer_deref_palette(Pointer) ->
	Code = int_to_bytelist(12),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_palette(DataList);
		Msg ->
			{error, Msg}
	end.

new_palette() ->
	Code = int_to_bytelist(13),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_palette(Pointer) ->
	Code = int_to_bytelist(14),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_ncolors(Pointer) ->
	Code = int_to_bytelist(15),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_ncolors(Pointer, Attrib) ->
	Code = int_to_bytelist(16),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_colors(Pointer) ->
	Code = int_to_bytelist(17),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_colors(Pointer, Attrib) ->
	Code = int_to_bytelist(18),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_version(Pointer) ->
	Code = int_to_bytelist(19),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_version(Pointer, Attrib) ->
	Code = int_to_bytelist(20),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_refcount(Pointer) ->
	Code = int_to_bytelist(21),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(22),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

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

pointer_deref_pixel_format(Pointer) ->
	Code = int_to_bytelist(23),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pixel_format(DataList);
		Msg ->
			{error, Msg}
	end.

new_pixel_format() ->
	Code = int_to_bytelist(24),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_pixel_format(Pointer) ->
	Code = int_to_bytelist(25),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_format(Pointer) ->
	Code = int_to_bytelist(26),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_format(Pointer, Attrib) ->
	Code = int_to_bytelist(27),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_palette(Pointer) ->
	Code = int_to_bytelist(28),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_palette(Pointer, Attrib) ->
	Code = int_to_bytelist(29),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_bits_per_pixel(Pointer) ->
	Code = int_to_bytelist(30),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_bits_per_pixel(Pointer, Attrib) ->
	Code = int_to_bytelist(31),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_bytes_per_pixel(Pointer) ->
	Code = int_to_bytelist(32),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_bytes_per_pixel(Pointer, Attrib) ->
	Code = int_to_bytelist(33),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_mask(Pointer) ->
	Code = int_to_bytelist(34),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(35),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_mask(Pointer) ->
	Code = int_to_bytelist(36),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(37),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_mask(Pointer) ->
	Code = int_to_bytelist(38),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(39),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_mask(Pointer) ->
	Code = int_to_bytelist(40),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(41),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_loss(Pointer) ->
	Code = int_to_bytelist(42),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(43),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_loss(Pointer) ->
	Code = int_to_bytelist(44),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(45),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_loss(Pointer) ->
	Code = int_to_bytelist(46),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(47),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_loss(Pointer) ->
	Code = int_to_bytelist(48),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(49),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_shift(Pointer) ->
	Code = int_to_bytelist(50),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(51),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_shift(Pointer) ->
	Code = int_to_bytelist(52),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(53),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_shift(Pointer) ->
	Code = int_to_bytelist(54),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(55),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_shift(Pointer) ->
	Code = int_to_bytelist(56),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(57),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_refcount(Pointer) ->
	Code = int_to_bytelist(58),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(59),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_next(Pointer) ->
	Code = int_to_bytelist(60),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_next(Pointer, Attrib) ->
	Code = int_to_bytelist(61),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

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

pointer_deref_rect(Pointer) ->
	Code = int_to_bytelist(62),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_rect(DataList);
		Msg ->
			{error, Msg}
	end.

new_rect() ->
	Code = int_to_bytelist(63),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_rect(Pointer) ->
	Code = int_to_bytelist(64),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_x(Pointer) ->
	Code = int_to_bytelist(65),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(66),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_y(Pointer) ->
	Code = int_to_bytelist(67),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(68),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_w(Pointer) ->
	Code = int_to_bytelist(69),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_w(Pointer, Attrib) ->
	Code = int_to_bytelist(70),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_h(Pointer) ->
	Code = int_to_bytelist(71),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_h(Pointer, Attrib) ->
	Code = int_to_bytelist(72),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

blit_map_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_blit_map(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_blit_map(Bytelist) ->
	parse_pointer(Bytelist).

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

pointer_deref_surface(Pointer) ->
	Code = int_to_bytelist(73),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_surface(DataList);
		Msg ->
			{error, Msg}
	end.

new_surface() ->
	Code = int_to_bytelist(74),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_surface(Pointer) ->
	Code = int_to_bytelist(75),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_flags(Pointer) ->
	Code = int_to_bytelist(76),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_flags(Pointer, Attrib) ->
	Code = int_to_bytelist(77),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_format(Pointer) ->
	Code = int_to_bytelist(78),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_format(Pointer, Attrib) ->
	Code = int_to_bytelist(79),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_w(Pointer) ->
	Code = int_to_bytelist(80),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_w(Pointer, Attrib) ->
	Code = int_to_bytelist(81),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_h(Pointer) ->
	Code = int_to_bytelist(82),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_h(Pointer, Attrib) ->
	Code = int_to_bytelist(83),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_pitch(Pointer) ->
	Code = int_to_bytelist(84),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_pitch(Pointer, Attrib) ->
	Code = int_to_bytelist(85),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_pixels(Pointer) ->
	Code = int_to_bytelist(86),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_pixels(Pointer, Attrib) ->
	Code = int_to_bytelist(87),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_userdata(Pointer) ->
	Code = int_to_bytelist(88),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_userdata(Pointer, Attrib) ->
	Code = int_to_bytelist(89),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_locked(Pointer) ->
	Code = int_to_bytelist(90),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_locked(Pointer, Attrib) ->
	Code = int_to_bytelist(91),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_lock_data(Pointer) ->
	Code = int_to_bytelist(92),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_lock_data(Pointer, Attrib) ->
	Code = int_to_bytelist(93),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_clip_rect(Pointer) ->
	Code = int_to_bytelist(94),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_rect(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_clip_rect(Pointer, Attrib) ->
	Code = int_to_bytelist(95),
	PList = pointer_to_bytelist(Pointer),
	AList = rect_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_map(Pointer) ->
	Code = int_to_bytelist(96),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_map(Pointer, Attrib) ->
	Code = int_to_bytelist(97),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_refcount(Pointer) ->
	Code = int_to_bytelist(98),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(99),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

scancode_get_int(Atom) ->
	case Atom of
		scancode_unknown -> 0;
		scancode_a -> 4;
		scancode_b -> 5;
		scancode_c -> 6;
		scancode_d -> 7;
		scancode_e -> 8;
		scancode_f -> 9;
		scancode_g -> 10;
		scancode_h -> 11;
		scancode_i -> 12;
		scancode_j -> 13;
		scancode_k -> 14;
		scancode_l -> 15;
		scancode_m -> 16;
		scancode_n -> 17;
		scancode_o -> 18;
		scancode_p -> 19;
		scancode_q -> 20;
		scancode_r -> 21;
		scancode_s -> 22;
		scancode_t -> 23;
		scancode_u -> 24;
		scancode_v -> 25;
		scancode_w -> 26;
		scancode_x -> 27;
		scancode_y -> 28;
		scancode_z -> 29;
		scancode_1 -> 30;
		scancode_2 -> 31;
		scancode_3 -> 32;
		scancode_4 -> 33;
		scancode_5 -> 34;
		scancode_6 -> 35;
		scancode_7 -> 36;
		scancode_8 -> 37;
		scancode_9 -> 38;
		scancode_0 -> 39;
		scancode_return -> 40;
		scancode_escape -> 41;
		scancode_backspace -> 42;
		scancode_tab -> 43;
		scancode_space -> 44;
		scancode_minus -> 45;
		scancode_equals -> 46;
		scancode_leftbracket -> 47;
		scancode_rightbracket -> 48;
		scancode_backslash -> 49;
		scancode_nonushash -> 50;
		scancode_semicolon -> 51;
		scancode_apostrophe -> 52;
		scancode_grave -> 53;
		scancode_comma -> 54;
		scancode_period -> 55;
		scancode_slash -> 56;
		scancode_capslock -> 57;
		scancode_f1 -> 58;
		scancode_f2 -> 59;
		scancode_f3 -> 60;
		scancode_f4 -> 61;
		scancode_f5 -> 62;
		scancode_f6 -> 63;
		scancode_f7 -> 64;
		scancode_f8 -> 65;
		scancode_f9 -> 66;
		scancode_f10 -> 67;
		scancode_f11 -> 68;
		scancode_f12 -> 69;
		scancode_printscreen -> 70;
		scancode_scrolllock -> 71;
		scancode_pause -> 72;
		scancode_insert -> 73;
		scancode_home -> 74;
		scancode_pageup -> 75;
		scancode_delete -> 76;
		scancode_end -> 77;
		scancode_pagedown -> 78;
		scancode_right -> 79;
		scancode_left -> 80;
		scancode_down -> 81;
		scancode_up -> 82;
		scancode_numlockclear -> 83;
		scancode_kp_divide -> 84;
		scancode_kp_multiply -> 85;
		scancode_kp_minus -> 86;
		scancode_kp_plus -> 87;
		scancode_kp_enter -> 88;
		scancode_kp_1 -> 89;
		scancode_kp_2 -> 90;
		scancode_kp_3 -> 91;
		scancode_kp_4 -> 92;
		scancode_kp_5 -> 93;
		scancode_kp_6 -> 94;
		scancode_kp_7 -> 95;
		scancode_kp_8 -> 96;
		scancode_kp_9 -> 97;
		scancode_kp_0 -> 98;
		scancode_kp_period -> 99;
		scancode_nonusbackslash -> 100;
		scancode_application -> 101;
		scancode_power -> 102;
		scancode_kp_equals -> 103;
		scancode_f13 -> 104;
		scancode_f14 -> 105;
		scancode_f15 -> 106;
		scancode_f16 -> 107;
		scancode_f17 -> 108;
		scancode_f18 -> 109;
		scancode_f19 -> 110;
		scancode_f20 -> 111;
		scancode_f21 -> 112;
		scancode_f22 -> 113;
		scancode_f23 -> 114;
		scancode_f24 -> 115;
		scancode_execute -> 116;
		scancode_help -> 117;
		scancode_menu -> 118;
		scancode_select -> 119;
		scancode_stop -> 120;
		scancode_again -> 121;
		scancode_undo -> 122;
		scancode_cut -> 123;
		scancode_copy -> 124;
		scancode_paste -> 125;
		scancode_find -> 126;
		scancode_mute -> 127;
		scancode_volumeup -> 128;
		scancode_volumedown -> 129;
		scancode_kp_comma -> 133;
		scancode_kp_equalsas400 -> 134;
		scancode_international1 -> 135;
		scancode_international2 -> 136;
		scancode_international3 -> 137;
		scancode_international4 -> 138;
		scancode_international5 -> 139;
		scancode_international6 -> 140;
		scancode_international7 -> 141;
		scancode_international8 -> 142;
		scancode_international9 -> 143;
		scancode_lang1 -> 144;
		scancode_lang2 -> 145;
		scancode_lang3 -> 146;
		scancode_lang4 -> 147;
		scancode_lang5 -> 148;
		scancode_lang6 -> 149;
		scancode_lang7 -> 150;
		scancode_lang8 -> 151;
		scancode_lang9 -> 152;
		scancode_alterase -> 153;
		scancode_sysreq -> 154;
		scancode_cancel -> 155;
		scancode_clear -> 156;
		scancode_prior -> 157;
		scancode_return2 -> 158;
		scancode_separator -> 159;
		scancode_out -> 160;
		scancode_oper -> 161;
		scancode_crearagain -> 162;
		scancode_crsel -> 163;
		scancode_exsel -> 164;
		scancode_kp_00 -> 176;
		scancode_kp_000 -> 177;
		scancode_thousandsseparator -> 178;
		scancode_decimalseparator -> 179;
		scancode_currencyunit -> 180;
		scancode_currencysubunit -> 181;
		scancode_kp_leftparen -> 182;
		scancode_kp_rightparen -> 183;
		scancode_kp_leftbrace -> 184;
		scancode_kp_rightbrace -> 185;
		scancode_kp_tab -> 186;
		scancode_kp_backspace -> 187;
		scancode_kp_a -> 188;
		scancode_kp_b -> 189;
		scancode_kp_c -> 190;
		scancode_kp_d -> 191;
		scancode_kp_e -> 192;
		scancode_kp_f -> 193;
		scancode_kp_xor -> 194;
		scancode_kp_power -> 195;
		scancode_kp_percent -> 196;
		scancode_kp_less -> 197;
		scancode_kp_greater -> 198;
		scancode_kp_ampersand -> 199;
		scancode_kp_dblampersand -> 200;
		scancode_kp_verticalbar -> 201;
		scancode_kp_dblverticalbar -> 202;
		scancode_kp_colon -> 203;
		scancode_kp_hash -> 204;
		scancode_kp_space -> 205;
		scancode_kp_at -> 206;
		scancode_kp_exclam -> 207;
		scancode_kp_memstore -> 208;
		scancode_kp_memrecall -> 209;
		scancode_kp_memclear -> 210;
		scancode_kp_memadd -> 211;
		scancode_kp_memsubtract -> 212;
		scancode_kp_memultiply -> 213;
		scancode_kp_memdivide -> 214;
		scancode_kp_plusminus -> 215;
		scancode_kp_clear -> 216;
		scancode_kp_clearentry -> 217;
		scancode_kp_binary -> 218;
		scancode_kp_octal -> 219;
		scancode_kp_decimal -> 220;
		scancode_kp_hexadecimal -> 221;
		scancode_lctrl -> 224;
		scancode_lshift -> 225;
		scancode_lalt -> 226;
		scancode_lgui -> 227;
		scancode_rctrl -> 228;
		scancode_rshift -> 229;
		scancode_ralt -> 230;
		scancode_rgui -> 231;
		scancode_mode -> 257;
		scancode_audionext -> 258;
		scancode_audioprev -> 259;
		scancode_audiostop -> 260;
		scancode_audioplay -> 261;
		scancode_audiomute -> 262;
		scancode_mediaselect -> 263;
		scancode_www -> 264;
		scancode_mail -> 265;
		scancode_calculator -> 266;
		scancode_computer -> 267;
		scancode_ac_search -> 268;
		scancode_ac_home -> 269;
		scancode_ac_back -> 270;
		scancode_ac_forward -> 271;
		scancode_ac_stop -> 272;
		scancode_ac_refresh -> 273;
		scancode_ac_bookmarks -> 274;
		scancode_brightnessdown -> 275;
		scancode_brightnessup -> 276;
		scancode_displayswitch -> 277;
		scancode_kbdillumtoggle -> 278;
		scancode_kbdillumdown -> 279;
		scancode_kbdillumup -> 280;
		scancode_eject -> 281;
		scancode_sleep -> 282;
		scancode_app1 -> 283;
		scancode_app2 -> 284;
		scancode_audiorewind -> 285;
		scancode_audiofastforward -> 286;
		num_scancodes -> 512;
		_ -> undefined
	end.

scancode_get_atom(Int) ->
	case Int of
		0 -> scancode_unknown;
		4 -> scancode_a;
		5 -> scancode_b;
		6 -> scancode_c;
		7 -> scancode_d;
		8 -> scancode_e;
		9 -> scancode_f;
		10 -> scancode_g;
		11 -> scancode_h;
		12 -> scancode_i;
		13 -> scancode_j;
		14 -> scancode_k;
		15 -> scancode_l;
		16 -> scancode_m;
		17 -> scancode_n;
		18 -> scancode_o;
		19 -> scancode_p;
		20 -> scancode_q;
		21 -> scancode_r;
		22 -> scancode_s;
		23 -> scancode_t;
		24 -> scancode_u;
		25 -> scancode_v;
		26 -> scancode_w;
		27 -> scancode_x;
		28 -> scancode_y;
		29 -> scancode_z;
		30 -> scancode_1;
		31 -> scancode_2;
		32 -> scancode_3;
		33 -> scancode_4;
		34 -> scancode_5;
		35 -> scancode_6;
		36 -> scancode_7;
		37 -> scancode_8;
		38 -> scancode_9;
		39 -> scancode_0;
		40 -> scancode_return;
		41 -> scancode_escape;
		42 -> scancode_backspace;
		43 -> scancode_tab;
		44 -> scancode_space;
		45 -> scancode_minus;
		46 -> scancode_equals;
		47 -> scancode_leftbracket;
		48 -> scancode_rightbracket;
		49 -> scancode_backslash;
		50 -> scancode_nonushash;
		51 -> scancode_semicolon;
		52 -> scancode_apostrophe;
		53 -> scancode_grave;
		54 -> scancode_comma;
		55 -> scancode_period;
		56 -> scancode_slash;
		57 -> scancode_capslock;
		58 -> scancode_f1;
		59 -> scancode_f2;
		60 -> scancode_f3;
		61 -> scancode_f4;
		62 -> scancode_f5;
		63 -> scancode_f6;
		64 -> scancode_f7;
		65 -> scancode_f8;
		66 -> scancode_f9;
		67 -> scancode_f10;
		68 -> scancode_f11;
		69 -> scancode_f12;
		70 -> scancode_printscreen;
		71 -> scancode_scrolllock;
		72 -> scancode_pause;
		73 -> scancode_insert;
		74 -> scancode_home;
		75 -> scancode_pageup;
		76 -> scancode_delete;
		77 -> scancode_end;
		78 -> scancode_pagedown;
		79 -> scancode_right;
		80 -> scancode_left;
		81 -> scancode_down;
		82 -> scancode_up;
		83 -> scancode_numlockclear;
		84 -> scancode_kp_divide;
		85 -> scancode_kp_multiply;
		86 -> scancode_kp_minus;
		87 -> scancode_kp_plus;
		88 -> scancode_kp_enter;
		89 -> scancode_kp_1;
		90 -> scancode_kp_2;
		91 -> scancode_kp_3;
		92 -> scancode_kp_4;
		93 -> scancode_kp_5;
		94 -> scancode_kp_6;
		95 -> scancode_kp_7;
		96 -> scancode_kp_8;
		97 -> scancode_kp_9;
		98 -> scancode_kp_0;
		99 -> scancode_kp_period;
		100 -> scancode_nonusbackslash;
		101 -> scancode_application;
		102 -> scancode_power;
		103 -> scancode_kp_equals;
		104 -> scancode_f13;
		105 -> scancode_f14;
		106 -> scancode_f15;
		107 -> scancode_f16;
		108 -> scancode_f17;
		109 -> scancode_f18;
		110 -> scancode_f19;
		111 -> scancode_f20;
		112 -> scancode_f21;
		113 -> scancode_f22;
		114 -> scancode_f23;
		115 -> scancode_f24;
		116 -> scancode_execute;
		117 -> scancode_help;
		118 -> scancode_menu;
		119 -> scancode_select;
		120 -> scancode_stop;
		121 -> scancode_again;
		122 -> scancode_undo;
		123 -> scancode_cut;
		124 -> scancode_copy;
		125 -> scancode_paste;
		126 -> scancode_find;
		127 -> scancode_mute;
		128 -> scancode_volumeup;
		129 -> scancode_volumedown;
		133 -> scancode_kp_comma;
		134 -> scancode_kp_equalsas400;
		135 -> scancode_international1;
		136 -> scancode_international2;
		137 -> scancode_international3;
		138 -> scancode_international4;
		139 -> scancode_international5;
		140 -> scancode_international6;
		141 -> scancode_international7;
		142 -> scancode_international8;
		143 -> scancode_international9;
		144 -> scancode_lang1;
		145 -> scancode_lang2;
		146 -> scancode_lang3;
		147 -> scancode_lang4;
		148 -> scancode_lang5;
		149 -> scancode_lang6;
		150 -> scancode_lang7;
		151 -> scancode_lang8;
		152 -> scancode_lang9;
		153 -> scancode_alterase;
		154 -> scancode_sysreq;
		155 -> scancode_cancel;
		156 -> scancode_clear;
		157 -> scancode_prior;
		158 -> scancode_return2;
		159 -> scancode_separator;
		160 -> scancode_out;
		161 -> scancode_oper;
		162 -> scancode_crearagain;
		163 -> scancode_crsel;
		164 -> scancode_exsel;
		176 -> scancode_kp_00;
		177 -> scancode_kp_000;
		178 -> scancode_thousandsseparator;
		179 -> scancode_decimalseparator;
		180 -> scancode_currencyunit;
		181 -> scancode_currencysubunit;
		182 -> scancode_kp_leftparen;
		183 -> scancode_kp_rightparen;
		184 -> scancode_kp_leftbrace;
		185 -> scancode_kp_rightbrace;
		186 -> scancode_kp_tab;
		187 -> scancode_kp_backspace;
		188 -> scancode_kp_a;
		189 -> scancode_kp_b;
		190 -> scancode_kp_c;
		191 -> scancode_kp_d;
		192 -> scancode_kp_e;
		193 -> scancode_kp_f;
		194 -> scancode_kp_xor;
		195 -> scancode_kp_power;
		196 -> scancode_kp_percent;
		197 -> scancode_kp_less;
		198 -> scancode_kp_greater;
		199 -> scancode_kp_ampersand;
		200 -> scancode_kp_dblampersand;
		201 -> scancode_kp_verticalbar;
		202 -> scancode_kp_dblverticalbar;
		203 -> scancode_kp_colon;
		204 -> scancode_kp_hash;
		205 -> scancode_kp_space;
		206 -> scancode_kp_at;
		207 -> scancode_kp_exclam;
		208 -> scancode_kp_memstore;
		209 -> scancode_kp_memrecall;
		210 -> scancode_kp_memclear;
		211 -> scancode_kp_memadd;
		212 -> scancode_kp_memsubtract;
		213 -> scancode_kp_memultiply;
		214 -> scancode_kp_memdivide;
		215 -> scancode_kp_plusminus;
		216 -> scancode_kp_clear;
		217 -> scancode_kp_clearentry;
		218 -> scancode_kp_binary;
		219 -> scancode_kp_octal;
		220 -> scancode_kp_decimal;
		221 -> scancode_kp_hexadecimal;
		224 -> scancode_lctrl;
		225 -> scancode_lshift;
		226 -> scancode_lalt;
		227 -> scancode_lgui;
		228 -> scancode_rctrl;
		229 -> scancode_rshift;
		230 -> scancode_ralt;
		231 -> scancode_rgui;
		257 -> scancode_mode;
		258 -> scancode_audionext;
		259 -> scancode_audioprev;
		260 -> scancode_audiostop;
		261 -> scancode_audioplay;
		262 -> scancode_audiomute;
		263 -> scancode_mediaselect;
		264 -> scancode_www;
		265 -> scancode_mail;
		266 -> scancode_calculator;
		267 -> scancode_computer;
		268 -> scancode_ac_search;
		269 -> scancode_ac_home;
		270 -> scancode_ac_back;
		271 -> scancode_ac_forward;
		272 -> scancode_ac_stop;
		273 -> scancode_ac_refresh;
		274 -> scancode_ac_bookmarks;
		275 -> scancode_brightnessdown;
		276 -> scancode_brightnessup;
		277 -> scancode_displayswitch;
		278 -> scancode_kbdillumtoggle;
		279 -> scancode_kbdillumdown;
		280 -> scancode_kbdillumup;
		281 -> scancode_eject;
		282 -> scancode_sleep;
		283 -> scancode_app1;
		284 -> scancode_app2;
		285 -> scancode_audiorewind;
		286 -> scancode_audiofastforward;
		512 -> num_scancodes;
		_ -> undefined
	end.

scancode_to_bytelist(Value) ->
	Int = scancode_get_int(Value),
	int_to_bytelist(Int).

bytelist_to_scancode(Bytelist) ->
	Int = bytelist_to_int(Bytelist),
	scancode_get_atom(Int).

parse_scancode(Bytelist) ->
	{Int, RList} = parse_int(Bytelist),
	{scancode_get_atom(Int), RList}.

keycode_to_bytelist(Value) ->
	sint32_to_bytelist(Value).

bytelist_to_keycode(Bytelist) ->
	bytelist_to_sint32(Bytelist).

parse_keycode(Bytelist) ->
	parse_sint32(Bytelist).

keysym_to_bytelist(Value) ->
	Return = [scancode_to_bytelist(Value#keysym.scancode),
	keycode_to_bytelist(Value#keysym.sym),
	uint16_to_bytelist(Value#keysym.mod),
	uint32_to_bytelist(Value#keysym.unused)],
	lists:flatten(Return).

bytelist_to_keysym(Bytelist) ->
	R0 = Bytelist,
	{Scancode, R1} = parse_scancode(R0),
	{Sym, R2} = parse_keycode(R1),
	{Mod, R3} = parse_uint16(R2),
	{Unused, R4} = parse_uint32(R3),
	#keysym{scancode=Scancode, sym=Sym, mod=Mod, unused=Unused}.

parse_keysym(Bytelist) ->
	R0 = Bytelist,
	{Scancode, R1} = parse_scancode(R0),
	{Sym, R2} = parse_keycode(R1),
	{Mod, R3} = parse_uint16(R2),
	{Unused, R4} = parse_uint32(R3),
	{#keysym{scancode=Scancode, sym=Sym, mod=Mod, unused=Unused}, R4}.

pointer_deref_keysym(Pointer) ->
	Code = int_to_bytelist(100),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_keysym(DataList);
		Msg ->
			{error, Msg}
	end.

new_keysym() ->
	Code = int_to_bytelist(101),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_keysym(Pointer) ->
	Code = int_to_bytelist(102),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_scancode(Pointer) ->
	Code = int_to_bytelist(103),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_scancode(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_scancode(Pointer, Attrib) ->
	Code = int_to_bytelist(104),
	PList = pointer_to_bytelist(Pointer),
	AList = scancode_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_sym(Pointer) ->
	Code = int_to_bytelist(105),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_keycode(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_sym(Pointer, Attrib) ->
	Code = int_to_bytelist(106),
	PList = pointer_to_bytelist(Pointer),
	AList = keycode_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_mod(Pointer) ->
	Code = int_to_bytelist(107),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_mod(Pointer, Attrib) ->
	Code = int_to_bytelist(108),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_unused(Pointer) ->
	Code = int_to_bytelist(109),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_unused(Pointer, Attrib) ->
	Code = int_to_bytelist(110),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joystick_id_to_bytelist(Value) ->
	sint32_to_bytelist(Value).

bytelist_to_joystick_id(Bytelist) ->
	bytelist_to_sint32(Bytelist).

parse_joystick_id(Bytelist) ->
	parse_sint32(Bytelist).

syswm_msg_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_syswm_msg(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_syswm_msg(Bytelist) ->
	parse_pointer(Bytelist).

touch_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_touch_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_touch_id(Bytelist) ->
	parse_sint64(Bytelist).

finger_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_finger_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_finger_id(Bytelist) ->
	parse_sint64(Bytelist).

gesture_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_gesture_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_gesture_id(Bytelist) ->
	parse_sint64(Bytelist).

common_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#common_event.type),
	uint32_to_bytelist(Value#common_event.timestamp)],
	lists:flatten(Return).

bytelist_to_common_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	#common_event{type=Type, timestamp=Timestamp}.

parse_common_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{#common_event{type=Type, timestamp=Timestamp}, R2}.

pointer_deref_common_event(Pointer) ->
	Code = int_to_bytelist(111),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_common_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_common_event() ->
	Code = int_to_bytelist(112),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_common_event(Pointer) ->
	Code = int_to_bytelist(113),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

common_event_get_type(Pointer) ->
	Code = int_to_bytelist(114),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

common_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(115),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

common_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(116),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

common_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(117),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#window_event.type),
	uint32_to_bytelist(Value#window_event.timestamp),
	uint32_to_bytelist(Value#window_event.windowID),
	uint8_to_bytelist(Value#window_event.event),
	uint8_to_bytelist(Value#window_event.padding1),
	uint8_to_bytelist(Value#window_event.padding2),
	uint8_to_bytelist(Value#window_event.padding3),
	sint32_to_bytelist(Value#window_event.data1),
	sint32_to_bytelist(Value#window_event.data2)],
	lists:flatten(Return).

bytelist_to_window_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Event, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Data1, R8} = parse_sint32(R7),
	{Data2, R9} = parse_sint32(R8),
	#window_event{type=Type, timestamp=Timestamp, windowID=WindowID, event=Event, padding1=Padding1, padding2=Padding2, padding3=Padding3, data1=Data1, data2=Data2}.

parse_window_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Event, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Data1, R8} = parse_sint32(R7),
	{Data2, R9} = parse_sint32(R8),
	{#window_event{type=Type, timestamp=Timestamp, windowID=WindowID, event=Event, padding1=Padding1, padding2=Padding2, padding3=Padding3, data1=Data1, data2=Data2}, R9}.

pointer_deref_window_event(Pointer) ->
	Code = int_to_bytelist(118),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_window_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_window_event() ->
	Code = int_to_bytelist(119),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_window_event(Pointer) ->
	Code = int_to_bytelist(120),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_type(Pointer) ->
	Code = int_to_bytelist(121),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(122),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(123),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(124),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(125),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(126),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_event(Pointer) ->
	Code = int_to_bytelist(127),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_event(Pointer, Attrib) ->
	Code = int_to_bytelist(128),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(129),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(130),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(131),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(132),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(133),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(134),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_data1(Pointer) ->
	Code = int_to_bytelist(135),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_data1(Pointer, Attrib) ->
	Code = int_to_bytelist(136),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_data2(Pointer) ->
	Code = int_to_bytelist(137),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_data2(Pointer, Attrib) ->
	Code = int_to_bytelist(138),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#keyboard_event.type),
	uint32_to_bytelist(Value#keyboard_event.timestamp),
	uint32_to_bytelist(Value#keyboard_event.windowID),
	uint8_to_bytelist(Value#keyboard_event.state),
	uint8_to_bytelist(Value#keyboard_event.repeat),
	uint8_to_bytelist(Value#keyboard_event.padding2),
	uint8_to_bytelist(Value#keyboard_event.padding3),
	keysym_to_bytelist(Value#keyboard_event.keysym)],
	lists:flatten(Return).

bytelist_to_keyboard_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{State, R4} = parse_uint8(R3),
	{Repeat, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Keysym, R8} = parse_keysym(R7),
	#keyboard_event{type=Type, timestamp=Timestamp, windowID=WindowID, state=State, repeat=Repeat, padding2=Padding2, padding3=Padding3, keysym=Keysym}.

parse_keyboard_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{State, R4} = parse_uint8(R3),
	{Repeat, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Keysym, R8} = parse_keysym(R7),
	{#keyboard_event{type=Type, timestamp=Timestamp, windowID=WindowID, state=State, repeat=Repeat, padding2=Padding2, padding3=Padding3, keysym=Keysym}, R8}.

pointer_deref_keyboard_event(Pointer) ->
	Code = int_to_bytelist(139),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_keyboard_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_keyboard_event() ->
	Code = int_to_bytelist(140),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_keyboard_event(Pointer) ->
	Code = int_to_bytelist(141),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_type(Pointer) ->
	Code = int_to_bytelist(142),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(143),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(144),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(145),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(146),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(147),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_state(Pointer) ->
	Code = int_to_bytelist(148),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(149),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_repeat(Pointer) ->
	Code = int_to_bytelist(150),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_repeat(Pointer, Attrib) ->
	Code = int_to_bytelist(151),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(152),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(153),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(154),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(155),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_keysym(Pointer) ->
	Code = int_to_bytelist(156),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_keysym(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_keysym(Pointer, Attrib) ->
	Code = int_to_bytelist(157),
	PList = pointer_to_bytelist(Pointer),
	AList = keysym_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#text_editing_event.type),
	uint32_to_bytelist(Value#text_editing_event.timestamp),
	uint32_to_bytelist(Value#text_editing_event.windowID),
	string_to_bytelist(Value#text_editing_event.text),
	sint32_to_bytelist(Value#text_editing_event.start),
	sint32_to_bytelist(Value#text_editing_event.length)],
	lists:flatten(Return).

bytelist_to_text_editing_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Text, R4} = parse_string(R3),
	{Start, R5} = parse_sint32(R4),
	{Length, R6} = parse_sint32(R5),
	#text_editing_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text, start=Start, length=Length}.

parse_text_editing_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Text, R4} = parse_string(R3),
	{Start, R5} = parse_sint32(R4),
	{Length, R6} = parse_sint32(R5),
	{#text_editing_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text, start=Start, length=Length}, R6}.

pointer_deref_text_editing_event(Pointer) ->
	Code = int_to_bytelist(158),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_text_editing_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_editing_event() ->
	Code = int_to_bytelist(159),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_text_editing_event(Pointer) ->
	Code = int_to_bytelist(160),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_type(Pointer) ->
	Code = int_to_bytelist(161),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(162),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(163),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(164),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(165),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(166),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_text(Pointer) ->
	Code = int_to_bytelist(167),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(168),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_start(Pointer) ->
	Code = int_to_bytelist(169),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_start(Pointer, Attrib) ->
	Code = int_to_bytelist(170),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_length(Pointer) ->
	Code = int_to_bytelist(171),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_length(Pointer, Attrib) ->
	Code = int_to_bytelist(172),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#text_input_event.type),
	uint32_to_bytelist(Value#text_input_event.timestamp),
	uint32_to_bytelist(Value#text_input_event.windowID),
	string_to_bytelist(Value#text_input_event.text)],
	lists:flatten(Return).

bytelist_to_text_input_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Text, R4} = parse_string(R3),
	#text_input_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text}.

parse_text_input_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Text, R4} = parse_string(R3),
	{#text_input_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text}, R4}.

pointer_deref_text_input_event(Pointer) ->
	Code = int_to_bytelist(173),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_text_input_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_input_event() ->
	Code = int_to_bytelist(174),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_text_input_event(Pointer) ->
	Code = int_to_bytelist(175),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_type(Pointer) ->
	Code = int_to_bytelist(176),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(177),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(178),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(179),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(180),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(181),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_text(Pointer) ->
	Code = int_to_bytelist(182),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(183),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#mouse_motion_event.type),
	uint32_to_bytelist(Value#mouse_motion_event.timestamp),
	uint32_to_bytelist(Value#mouse_motion_event.windowID),
	uint32_to_bytelist(Value#mouse_motion_event.which),
	uint32_to_bytelist(Value#mouse_motion_event.state),
	sint32_to_bytelist(Value#mouse_motion_event.x),
	sint32_to_bytelist(Value#mouse_motion_event.y),
	sint32_to_bytelist(Value#mouse_motion_event.xrel),
	sint32_to_bytelist(Value#mouse_motion_event.yrel)],
	lists:flatten(Return).

bytelist_to_mouse_motion_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{State, R5} = parse_uint32(R4),
	{X, R6} = parse_sint32(R5),
	{Y, R7} = parse_sint32(R6),
	{Xrel, R8} = parse_sint32(R7),
	{Yrel, R9} = parse_sint32(R8),
	#mouse_motion_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, state=State, x=X, y=Y, xrel=Xrel, yrel=Yrel}.

parse_mouse_motion_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{State, R5} = parse_uint32(R4),
	{X, R6} = parse_sint32(R5),
	{Y, R7} = parse_sint32(R6),
	{Xrel, R8} = parse_sint32(R7),
	{Yrel, R9} = parse_sint32(R8),
	{#mouse_motion_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, state=State, x=X, y=Y, xrel=Xrel, yrel=Yrel}, R9}.

pointer_deref_mouse_motion_event(Pointer) ->
	Code = int_to_bytelist(184),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_mouse_motion_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_motion_event() ->
	Code = int_to_bytelist(185),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_mouse_motion_event(Pointer) ->
	Code = int_to_bytelist(186),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_type(Pointer) ->
	Code = int_to_bytelist(187),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(188),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(189),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(190),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(191),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(192),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_which(Pointer) ->
	Code = int_to_bytelist(193),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(194),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_state(Pointer) ->
	Code = int_to_bytelist(195),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(196),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_x(Pointer) ->
	Code = int_to_bytelist(197),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(198),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_y(Pointer) ->
	Code = int_to_bytelist(199),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(200),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_xrel(Pointer) ->
	Code = int_to_bytelist(201),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_xrel(Pointer, Attrib) ->
	Code = int_to_bytelist(202),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_yrel(Pointer) ->
	Code = int_to_bytelist(203),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_yrel(Pointer, Attrib) ->
	Code = int_to_bytelist(204),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#mouse_button_event.type),
	uint32_to_bytelist(Value#mouse_button_event.timestamp),
	uint32_to_bytelist(Value#mouse_button_event.windowID),
	uint32_to_bytelist(Value#mouse_button_event.which),
	uint8_to_bytelist(Value#mouse_button_event.button),
	uint8_to_bytelist(Value#mouse_button_event.state),
	uint8_to_bytelist(Value#mouse_button_event.clicks),
	sint32_to_bytelist(Value#mouse_button_event.x),
	sint32_to_bytelist(Value#mouse_button_event.y)],
	lists:flatten(Return).

bytelist_to_mouse_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{Button, R5} = parse_uint8(R4),
	{State, R6} = parse_uint8(R5),
	{Clicks, R7} = parse_uint8(R6),
	{X, R8} = parse_sint32(R7),
	{Y, R9} = parse_sint32(R8),
	#mouse_button_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, button=Button, state=State, clicks=Clicks, x=X, y=Y}.

parse_mouse_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{Button, R5} = parse_uint8(R4),
	{State, R6} = parse_uint8(R5),
	{Clicks, R7} = parse_uint8(R6),
	{X, R8} = parse_sint32(R7),
	{Y, R9} = parse_sint32(R8),
	{#mouse_button_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, button=Button, state=State, clicks=Clicks, x=X, y=Y}, R9}.

pointer_deref_mouse_button_event(Pointer) ->
	Code = int_to_bytelist(205),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_mouse_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_button_event() ->
	Code = int_to_bytelist(206),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_mouse_button_event(Pointer) ->
	Code = int_to_bytelist(207),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(208),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(209),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(210),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(211),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(212),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(213),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(214),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(215),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(216),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(217),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(218),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(219),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_clicks(Pointer) ->
	Code = int_to_bytelist(220),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_clicks(Pointer, Attrib) ->
	Code = int_to_bytelist(221),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_x(Pointer) ->
	Code = int_to_bytelist(222),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(223),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_y(Pointer) ->
	Code = int_to_bytelist(224),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(225),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#mouse_wheel_event.type),
	uint32_to_bytelist(Value#mouse_wheel_event.timestamp),
	uint32_to_bytelist(Value#mouse_wheel_event.windowID),
	uint32_to_bytelist(Value#mouse_wheel_event.which),
	sint32_to_bytelist(Value#mouse_wheel_event.x),
	sint32_to_bytelist(Value#mouse_wheel_event.y),
	uint32_to_bytelist(Value#mouse_wheel_event.direction)],
	lists:flatten(Return).

bytelist_to_mouse_wheel_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{X, R5} = parse_sint32(R4),
	{Y, R6} = parse_sint32(R5),
	{Direction, R7} = parse_uint32(R6),
	#mouse_wheel_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, x=X, y=Y, direction=Direction}.

parse_mouse_wheel_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Which, R4} = parse_uint32(R3),
	{X, R5} = parse_sint32(R4),
	{Y, R6} = parse_sint32(R5),
	{Direction, R7} = parse_uint32(R6),
	{#mouse_wheel_event{type=Type, timestamp=Timestamp, windowID=WindowID, which=Which, x=X, y=Y, direction=Direction}, R7}.

pointer_deref_mouse_wheel_event(Pointer) ->
	Code = int_to_bytelist(226),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_mouse_wheel_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_wheel_event() ->
	Code = int_to_bytelist(227),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_mouse_wheel_event(Pointer) ->
	Code = int_to_bytelist(228),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_type(Pointer) ->
	Code = int_to_bytelist(229),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(230),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(231),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(232),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(233),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(234),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_which(Pointer) ->
	Code = int_to_bytelist(235),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(236),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_x(Pointer) ->
	Code = int_to_bytelist(237),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(238),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_y(Pointer) ->
	Code = int_to_bytelist(239),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(240),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_direction(Pointer) ->
	Code = int_to_bytelist(241),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_direction(Pointer, Attrib) ->
	Code = int_to_bytelist(242),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#joy_axis_event.type),
	uint32_to_bytelist(Value#joy_axis_event.timestamp),
	joystick_id_to_bytelist(Value#joy_axis_event.which),
	uint8_to_bytelist(Value#joy_axis_event.axis),
	uint8_to_bytelist(Value#joy_axis_event.padding1),
	uint8_to_bytelist(Value#joy_axis_event.padding2),
	uint8_to_bytelist(Value#joy_axis_event.padding3),
	sint16_to_bytelist(Value#joy_axis_event.value)],
	lists:flatten(Return).

bytelist_to_joy_axis_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Axis, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Value, R8} = parse_sint16(R7),
	#joy_axis_event{type=Type, timestamp=Timestamp, which=Which, axis=Axis, padding1=Padding1, padding2=Padding2, padding3=Padding3, value=Value}.

parse_joy_axis_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Axis, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Value, R8} = parse_sint16(R7),
	{#joy_axis_event{type=Type, timestamp=Timestamp, which=Which, axis=Axis, padding1=Padding1, padding2=Padding2, padding3=Padding3, value=Value}, R8}.

pointer_deref_joy_axis_event(Pointer) ->
	Code = int_to_bytelist(243),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_joy_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_axis_event() ->
	Code = int_to_bytelist(244),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_joy_axis_event(Pointer) ->
	Code = int_to_bytelist(245),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_type(Pointer) ->
	Code = int_to_bytelist(246),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(247),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(248),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(249),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_which(Pointer) ->
	Code = int_to_bytelist(250),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(251),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_axis(Pointer) ->
	Code = int_to_bytelist(252),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_axis(Pointer, Attrib) ->
	Code = int_to_bytelist(253),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(254),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(255),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(256),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(257),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(258),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(259),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_value(Pointer) ->
	Code = int_to_bytelist(260),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(261),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#joy_ball_event.type),
	uint32_to_bytelist(Value#joy_ball_event.timestamp),
	joystick_id_to_bytelist(Value#joy_ball_event.which),
	uint8_to_bytelist(Value#joy_ball_event.ball),
	uint8_to_bytelist(Value#joy_ball_event.padding1),
	uint8_to_bytelist(Value#joy_ball_event.padding2),
	uint8_to_bytelist(Value#joy_ball_event.padding3),
	sint16_to_bytelist(Value#joy_ball_event.xrel),
	sint16_to_bytelist(Value#joy_ball_event.yrel)],
	lists:flatten(Return).

bytelist_to_joy_ball_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Ball, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Xrel, R8} = parse_sint16(R7),
	{Yrel, R9} = parse_sint16(R8),
	#joy_ball_event{type=Type, timestamp=Timestamp, which=Which, ball=Ball, padding1=Padding1, padding2=Padding2, padding3=Padding3, xrel=Xrel, yrel=Yrel}.

parse_joy_ball_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Ball, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Xrel, R8} = parse_sint16(R7),
	{Yrel, R9} = parse_sint16(R8),
	{#joy_ball_event{type=Type, timestamp=Timestamp, which=Which, ball=Ball, padding1=Padding1, padding2=Padding2, padding3=Padding3, xrel=Xrel, yrel=Yrel}, R9}.

pointer_deref_joy_ball_event(Pointer) ->
	Code = int_to_bytelist(262),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_joy_ball_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_ball_event() ->
	Code = int_to_bytelist(263),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_joy_ball_event(Pointer) ->
	Code = int_to_bytelist(264),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_type(Pointer) ->
	Code = int_to_bytelist(265),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(266),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(267),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(268),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_which(Pointer) ->
	Code = int_to_bytelist(269),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(270),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_ball(Pointer) ->
	Code = int_to_bytelist(271),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_ball(Pointer, Attrib) ->
	Code = int_to_bytelist(272),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(273),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(274),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(275),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(276),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(277),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(278),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_xrel(Pointer) ->
	Code = int_to_bytelist(279),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_xrel(Pointer, Attrib) ->
	Code = int_to_bytelist(280),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_yrel(Pointer) ->
	Code = int_to_bytelist(281),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_yrel(Pointer, Attrib) ->
	Code = int_to_bytelist(282),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#joy_hat_event.type),
	uint32_to_bytelist(Value#joy_hat_event.timestamp),
	joystick_id_to_bytelist(Value#joy_hat_event.which),
	uint8_to_bytelist(Value#joy_hat_event.hat),
	uint8_to_bytelist(Value#joy_hat_event.value),
	uint8_to_bytelist(Value#joy_hat_event.padding1),
	uint8_to_bytelist(Value#joy_hat_event.padding2)],
	lists:flatten(Return).

bytelist_to_joy_hat_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Hat, R4} = parse_uint8(R3),
	{Value, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	#joy_hat_event{type=Type, timestamp=Timestamp, which=Which, hat=Hat, value=Value, padding1=Padding1, padding2=Padding2}.

parse_joy_hat_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Hat, R4} = parse_uint8(R3),
	{Value, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	{#joy_hat_event{type=Type, timestamp=Timestamp, which=Which, hat=Hat, value=Value, padding1=Padding1, padding2=Padding2}, R7}.

pointer_deref_joy_hat_event(Pointer) ->
	Code = int_to_bytelist(283),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_joy_hat_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_hat_event() ->
	Code = int_to_bytelist(284),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_joy_hat_event(Pointer) ->
	Code = int_to_bytelist(285),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_type(Pointer) ->
	Code = int_to_bytelist(286),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(287),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(288),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(289),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_which(Pointer) ->
	Code = int_to_bytelist(290),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(291),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_hat(Pointer) ->
	Code = int_to_bytelist(292),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_hat(Pointer, Attrib) ->
	Code = int_to_bytelist(293),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_value(Pointer) ->
	Code = int_to_bytelist(294),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(295),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(296),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(297),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(298),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(299),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#joy_button_event.type),
	uint32_to_bytelist(Value#joy_button_event.timestamp),
	joystick_id_to_bytelist(Value#joy_button_event.which),
	uint8_to_bytelist(Value#joy_button_event.button),
	uint8_to_bytelist(Value#joy_button_event.state),
	uint8_to_bytelist(Value#joy_button_event.padding1),
	uint8_to_bytelist(Value#joy_button_event.padding2)],
	lists:flatten(Return).

bytelist_to_joy_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Button, R4} = parse_uint8(R3),
	{State, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	#joy_button_event{type=Type, timestamp=Timestamp, which=Which, button=Button, state=State, padding1=Padding1, padding2=Padding2}.

parse_joy_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Button, R4} = parse_uint8(R3),
	{State, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	{#joy_button_event{type=Type, timestamp=Timestamp, which=Which, button=Button, state=State, padding1=Padding1, padding2=Padding2}, R7}.

pointer_deref_joy_button_event(Pointer) ->
	Code = int_to_bytelist(300),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_joy_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_button_event() ->
	Code = int_to_bytelist(301),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_joy_button_event(Pointer) ->
	Code = int_to_bytelist(302),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(303),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(304),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(305),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(306),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(307),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(308),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(309),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(310),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(311),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(312),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(313),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(314),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(315),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(316),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#joy_device_event.type),
	uint32_to_bytelist(Value#joy_device_event.timestamp),
	sint32_to_bytelist(Value#joy_device_event.which)],
	lists:flatten(Return).

bytelist_to_joy_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	#joy_device_event{type=Type, timestamp=Timestamp, which=Which}.

parse_joy_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	{#joy_device_event{type=Type, timestamp=Timestamp, which=Which}, R3}.

pointer_deref_joy_device_event(Pointer) ->
	Code = int_to_bytelist(317),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_joy_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_device_event() ->
	Code = int_to_bytelist(318),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_joy_device_event(Pointer) ->
	Code = int_to_bytelist(319),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(320),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(321),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(322),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(323),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(324),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(325),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#controller_axis_event.type),
	uint32_to_bytelist(Value#controller_axis_event.timestamp),
	joystick_id_to_bytelist(Value#controller_axis_event.which),
	uint8_to_bytelist(Value#controller_axis_event.axis),
	uint8_to_bytelist(Value#controller_axis_event.padding1),
	uint8_to_bytelist(Value#controller_axis_event.padding2),
	uint8_to_bytelist(Value#controller_axis_event.padding3),
	sint16_to_bytelist(Value#controller_axis_event.value),
	uint16_to_bytelist(Value#controller_axis_event.padding4)],
	lists:flatten(Return).

bytelist_to_controller_axis_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Axis, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Value, R8} = parse_sint16(R7),
	{Padding4, R9} = parse_uint16(R8),
	#controller_axis_event{type=Type, timestamp=Timestamp, which=Which, axis=Axis, padding1=Padding1, padding2=Padding2, padding3=Padding3, value=Value, padding4=Padding4}.

parse_controller_axis_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Axis, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{Value, R8} = parse_sint16(R7),
	{Padding4, R9} = parse_uint16(R8),
	{#controller_axis_event{type=Type, timestamp=Timestamp, which=Which, axis=Axis, padding1=Padding1, padding2=Padding2, padding3=Padding3, value=Value, padding4=Padding4}, R9}.

pointer_deref_controller_axis_event(Pointer) ->
	Code = int_to_bytelist(326),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_controller_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_axis_event() ->
	Code = int_to_bytelist(327),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_controller_axis_event(Pointer) ->
	Code = int_to_bytelist(328),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_type(Pointer) ->
	Code = int_to_bytelist(329),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(330),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(331),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(332),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_which(Pointer) ->
	Code = int_to_bytelist(333),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(334),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_axis(Pointer) ->
	Code = int_to_bytelist(335),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_axis(Pointer, Attrib) ->
	Code = int_to_bytelist(336),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(337),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(338),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(339),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(340),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(341),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(342),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_value(Pointer) ->
	Code = int_to_bytelist(343),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(344),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding4(Pointer) ->
	Code = int_to_bytelist(345),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding4(Pointer, Attrib) ->
	Code = int_to_bytelist(346),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#controller_button_event.type),
	uint32_to_bytelist(Value#controller_button_event.timestamp),
	joystick_id_to_bytelist(Value#controller_button_event.which),
	uint8_to_bytelist(Value#controller_button_event.button),
	uint8_to_bytelist(Value#controller_button_event.state),
	uint8_to_bytelist(Value#controller_button_event.padding1),
	uint8_to_bytelist(Value#controller_button_event.padding2)],
	lists:flatten(Return).

bytelist_to_controller_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Button, R4} = parse_uint8(R3),
	{State, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	#controller_button_event{type=Type, timestamp=Timestamp, which=Which, button=Button, state=State, padding1=Padding1, padding2=Padding2}.

parse_controller_button_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_joystick_id(R2),
	{Button, R4} = parse_uint8(R3),
	{State, R5} = parse_uint8(R4),
	{Padding1, R6} = parse_uint8(R5),
	{Padding2, R7} = parse_uint8(R6),
	{#controller_button_event{type=Type, timestamp=Timestamp, which=Which, button=Button, state=State, padding1=Padding1, padding2=Padding2}, R7}.

pointer_deref_controller_button_event(Pointer) ->
	Code = int_to_bytelist(347),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_controller_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_button_event() ->
	Code = int_to_bytelist(348),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_controller_button_event(Pointer) ->
	Code = int_to_bytelist(349),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(350),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(351),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(352),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(353),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(354),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(355),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(356),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(357),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(358),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(359),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(360),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(361),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(362),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(363),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#controller_device_event.type),
	uint32_to_bytelist(Value#controller_device_event.timestamp),
	sint32_to_bytelist(Value#controller_device_event.which)],
	lists:flatten(Return).

bytelist_to_controller_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	#controller_device_event{type=Type, timestamp=Timestamp, which=Which}.

parse_controller_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	{#controller_device_event{type=Type, timestamp=Timestamp, which=Which}, R3}.

pointer_deref_controller_device_event(Pointer) ->
	Code = int_to_bytelist(364),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_controller_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_device_event() ->
	Code = int_to_bytelist(365),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_controller_device_event(Pointer) ->
	Code = int_to_bytelist(366),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(367),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(368),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(369),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(370),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(371),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(372),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#audio_device_event.type),
	uint32_to_bytelist(Value#audio_device_event.timestamp),
	uint32_to_bytelist(Value#audio_device_event.which),
	uint8_to_bytelist(Value#audio_device_event.iscapture),
	uint8_to_bytelist(Value#audio_device_event.padding1),
	uint8_to_bytelist(Value#audio_device_event.padding2),
	uint8_to_bytelist(Value#audio_device_event.padding3)],
	lists:flatten(Return).

bytelist_to_audio_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_uint32(R2),
	{Iscapture, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	#audio_device_event{type=Type, timestamp=Timestamp, which=Which, iscapture=Iscapture, padding1=Padding1, padding2=Padding2, padding3=Padding3}.

parse_audio_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_uint32(R2),
	{Iscapture, R4} = parse_uint8(R3),
	{Padding1, R5} = parse_uint8(R4),
	{Padding2, R6} = parse_uint8(R5),
	{Padding3, R7} = parse_uint8(R6),
	{#audio_device_event{type=Type, timestamp=Timestamp, which=Which, iscapture=Iscapture, padding1=Padding1, padding2=Padding2, padding3=Padding3}, R7}.

pointer_deref_audio_device_event(Pointer) ->
	Code = int_to_bytelist(373),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_audio_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_audio_device_event() ->
	Code = int_to_bytelist(374),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_audio_device_event(Pointer) ->
	Code = int_to_bytelist(375),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(376),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(377),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(378),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(379),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(380),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(381),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_iscapture(Pointer) ->
	Code = int_to_bytelist(382),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_iscapture(Pointer, Attrib) ->
	Code = int_to_bytelist(383),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(384),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(385),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(386),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(387),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(388),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(389),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

quit_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#quit_event.type),
	uint32_to_bytelist(Value#quit_event.timestamp)],
	lists:flatten(Return).

bytelist_to_quit_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	#quit_event{type=Type, timestamp=Timestamp}.

parse_quit_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{#quit_event{type=Type, timestamp=Timestamp}, R2}.

pointer_deref_quit_event(Pointer) ->
	Code = int_to_bytelist(390),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_quit_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_quit_event() ->
	Code = int_to_bytelist(391),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_quit_event(Pointer) ->
	Code = int_to_bytelist(392),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

quit_event_get_type(Pointer) ->
	Code = int_to_bytelist(393),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

quit_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(394),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

quit_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(395),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

quit_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(396),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#user_event.type),
	uint32_to_bytelist(Value#user_event.timestamp),
	uint32_to_bytelist(Value#user_event.windowID),
	sint32_to_bytelist(Value#user_event.code),
	pointer_to_bytelist(Value#user_event.data1),
	pointer_to_bytelist(Value#user_event.data2)],
	lists:flatten(Return).

bytelist_to_user_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Code, R4} = parse_sint32(R3),
	{Data1, R5} = parse_pointer(R4),
	{Data2, R6} = parse_pointer(R5),
	#user_event{type=Type, timestamp=Timestamp, windowID=WindowID, code=Code, data1=Data1, data2=Data2}.

parse_user_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Code, R4} = parse_sint32(R3),
	{Data1, R5} = parse_pointer(R4),
	{Data2, R6} = parse_pointer(R5),
	{#user_event{type=Type, timestamp=Timestamp, windowID=WindowID, code=Code, data1=Data1, data2=Data2}, R6}.

pointer_deref_user_event(Pointer) ->
	Code = int_to_bytelist(397),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_user_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_user_event() ->
	Code = int_to_bytelist(398),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_user_event(Pointer) ->
	Code = int_to_bytelist(399),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_type(Pointer) ->
	Code = int_to_bytelist(400),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(401),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(402),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(403),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(404),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(405),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_code(Pointer) ->
	Code = int_to_bytelist(406),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_code(Pointer, Attrib) ->
	Code = int_to_bytelist(407),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_data1(Pointer) ->
	Code = int_to_bytelist(408),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_data1(Pointer, Attrib) ->
	Code = int_to_bytelist(409),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_data2(Pointer) ->
	Code = int_to_bytelist(410),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_data2(Pointer, Attrib) ->
	Code = int_to_bytelist(411),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#syswm_event.type),
	uint32_to_bytelist(Value#syswm_event.timestamp),
	pointer_to_bytelist(Value#syswm_event.msg)],
	lists:flatten(Return).

bytelist_to_syswm_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Msg, R3} = parse_pointer(R2),
	#syswm_event{type=Type, timestamp=Timestamp, msg=Msg}.

parse_syswm_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Msg, R3} = parse_pointer(R2),
	{#syswm_event{type=Type, timestamp=Timestamp, msg=Msg}, R3}.

pointer_deref_syswm_event(Pointer) ->
	Code = int_to_bytelist(412),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_syswm_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_syswm_event() ->
	Code = int_to_bytelist(413),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_syswm_event(Pointer) ->
	Code = int_to_bytelist(414),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_get_type(Pointer) ->
	Code = int_to_bytelist(415),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(416),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(417),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(418),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_get_msg(Pointer) ->
	Code = int_to_bytelist(419),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_msg(Pointer, Attrib) ->
	Code = int_to_bytelist(420),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#touch_finger_event.type),
	uint32_to_bytelist(Value#touch_finger_event.timestamp),
	touch_id_to_bytelist(Value#touch_finger_event.touchId),
	finger_id_to_bytelist(Value#touch_finger_event.fingerId),
	float_to_bytelist(Value#touch_finger_event.x),
	float_to_bytelist(Value#touch_finger_event.y),
	float_to_bytelist(Value#touch_finger_event.dx),
	float_to_bytelist(Value#touch_finger_event.dy),
	float_to_bytelist(Value#touch_finger_event.pressure)],
	lists:flatten(Return).

bytelist_to_touch_finger_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{FingerId, R4} = parse_finger_id(R3),
	{X, R5} = parse_float(R4),
	{Y, R6} = parse_float(R5),
	{Dx, R7} = parse_float(R6),
	{Dy, R8} = parse_float(R7),
	{Pressure, R9} = parse_float(R8),
	#touch_finger_event{type=Type, timestamp=Timestamp, touchId=TouchId, fingerId=FingerId, x=X, y=Y, dx=Dx, dy=Dy, pressure=Pressure}.

parse_touch_finger_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{FingerId, R4} = parse_finger_id(R3),
	{X, R5} = parse_float(R4),
	{Y, R6} = parse_float(R5),
	{Dx, R7} = parse_float(R6),
	{Dy, R8} = parse_float(R7),
	{Pressure, R9} = parse_float(R8),
	{#touch_finger_event{type=Type, timestamp=Timestamp, touchId=TouchId, fingerId=FingerId, x=X, y=Y, dx=Dx, dy=Dy, pressure=Pressure}, R9}.

pointer_deref_touch_finger_event(Pointer) ->
	Code = int_to_bytelist(421),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_touch_finger_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_touch_finger_event() ->
	Code = int_to_bytelist(422),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_touch_finger_event(Pointer) ->
	Code = int_to_bytelist(423),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_type(Pointer) ->
	Code = int_to_bytelist(424),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(425),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(426),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(427),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(428),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(429),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_fingerId(Pointer) ->
	Code = int_to_bytelist(430),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_finger_id(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_fingerId(Pointer, Attrib) ->
	Code = int_to_bytelist(431),
	PList = pointer_to_bytelist(Pointer),
	AList = finger_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_x(Pointer) ->
	Code = int_to_bytelist(432),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(433),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_y(Pointer) ->
	Code = int_to_bytelist(434),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(435),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_dx(Pointer) ->
	Code = int_to_bytelist(436),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_dx(Pointer, Attrib) ->
	Code = int_to_bytelist(437),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_dy(Pointer) ->
	Code = int_to_bytelist(438),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_dy(Pointer, Attrib) ->
	Code = int_to_bytelist(439),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_pressure(Pointer) ->
	Code = int_to_bytelist(440),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_pressure(Pointer, Attrib) ->
	Code = int_to_bytelist(441),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#multi_gesture_event.type),
	uint32_to_bytelist(Value#multi_gesture_event.timestamp),
	touch_id_to_bytelist(Value#multi_gesture_event.touchId),
	float_to_bytelist(Value#multi_gesture_event.dTheta),
	float_to_bytelist(Value#multi_gesture_event.dDist),
	float_to_bytelist(Value#multi_gesture_event.x),
	float_to_bytelist(Value#multi_gesture_event.y),
	uint16_to_bytelist(Value#multi_gesture_event.numFingers),
	uint16_to_bytelist(Value#multi_gesture_event.padding)],
	lists:flatten(Return).

bytelist_to_multi_gesture_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{DTheta, R4} = parse_float(R3),
	{DDist, R5} = parse_float(R4),
	{X, R6} = parse_float(R5),
	{Y, R7} = parse_float(R6),
	{NumFingers, R8} = parse_uint16(R7),
	{Padding, R9} = parse_uint16(R8),
	#multi_gesture_event{type=Type, timestamp=Timestamp, touchId=TouchId, dTheta=DTheta, dDist=DDist, x=X, y=Y, numFingers=NumFingers, padding=Padding}.

parse_multi_gesture_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{DTheta, R4} = parse_float(R3),
	{DDist, R5} = parse_float(R4),
	{X, R6} = parse_float(R5),
	{Y, R7} = parse_float(R6),
	{NumFingers, R8} = parse_uint16(R7),
	{Padding, R9} = parse_uint16(R8),
	{#multi_gesture_event{type=Type, timestamp=Timestamp, touchId=TouchId, dTheta=DTheta, dDist=DDist, x=X, y=Y, numFingers=NumFingers, padding=Padding}, R9}.

pointer_deref_multi_gesture_event(Pointer) ->
	Code = int_to_bytelist(442),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_multi_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_multi_gesture_event() ->
	Code = int_to_bytelist(443),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_multi_gesture_event(Pointer) ->
	Code = int_to_bytelist(444),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_type(Pointer) ->
	Code = int_to_bytelist(445),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(446),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(447),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(448),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(449),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(450),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_dTheta(Pointer) ->
	Code = int_to_bytelist(451),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_dTheta(Pointer, Attrib) ->
	Code = int_to_bytelist(452),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_dDist(Pointer) ->
	Code = int_to_bytelist(453),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_dDist(Pointer, Attrib) ->
	Code = int_to_bytelist(454),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_x(Pointer) ->
	Code = int_to_bytelist(455),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(456),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_y(Pointer) ->
	Code = int_to_bytelist(457),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(458),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_numFingers(Pointer) ->
	Code = int_to_bytelist(459),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_numFingers(Pointer, Attrib) ->
	Code = int_to_bytelist(460),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_padding(Pointer) ->
	Code = int_to_bytelist(461),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_padding(Pointer, Attrib) ->
	Code = int_to_bytelist(462),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#dollar_gesture_event.type),
	uint32_to_bytelist(Value#dollar_gesture_event.timestamp),
	touch_id_to_bytelist(Value#dollar_gesture_event.touchId),
	gesture_id_to_bytelist(Value#dollar_gesture_event.gestureId),
	uint32_to_bytelist(Value#dollar_gesture_event.numFingers),
	float_to_bytelist(Value#dollar_gesture_event.error),
	float_to_bytelist(Value#dollar_gesture_event.x),
	float_to_bytelist(Value#dollar_gesture_event.y)],
	lists:flatten(Return).

bytelist_to_dollar_gesture_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{GestureId, R4} = parse_gesture_id(R3),
	{NumFingers, R5} = parse_uint32(R4),
	{Error, R6} = parse_float(R5),
	{X, R7} = parse_float(R6),
	{Y, R8} = parse_float(R7),
	#dollar_gesture_event{type=Type, timestamp=Timestamp, touchId=TouchId, gestureId=GestureId, numFingers=NumFingers, error=Error, x=X, y=Y}.

parse_dollar_gesture_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{TouchId, R3} = parse_touch_id(R2),
	{GestureId, R4} = parse_gesture_id(R3),
	{NumFingers, R5} = parse_uint32(R4),
	{Error, R6} = parse_float(R5),
	{X, R7} = parse_float(R6),
	{Y, R8} = parse_float(R7),
	{#dollar_gesture_event{type=Type, timestamp=Timestamp, touchId=TouchId, gestureId=GestureId, numFingers=NumFingers, error=Error, x=X, y=Y}, R8}.

pointer_deref_dollar_gesture_event(Pointer) ->
	Code = int_to_bytelist(463),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_dollar_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_dollar_gesture_event() ->
	Code = int_to_bytelist(464),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_dollar_gesture_event(Pointer) ->
	Code = int_to_bytelist(465),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_type(Pointer) ->
	Code = int_to_bytelist(466),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(467),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(468),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(469),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(470),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(471),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_gestureId(Pointer) ->
	Code = int_to_bytelist(472),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_gesture_id(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_gestureId(Pointer, Attrib) ->
	Code = int_to_bytelist(473),
	PList = pointer_to_bytelist(Pointer),
	AList = gesture_id_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_numFingers(Pointer) ->
	Code = int_to_bytelist(474),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_numFingers(Pointer, Attrib) ->
	Code = int_to_bytelist(475),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_error(Pointer) ->
	Code = int_to_bytelist(476),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_error(Pointer, Attrib) ->
	Code = int_to_bytelist(477),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_x(Pointer) ->
	Code = int_to_bytelist(478),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(479),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_y(Pointer) ->
	Code = int_to_bytelist(480),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(481),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#drop_event.type),
	uint32_to_bytelist(Value#drop_event.timestamp),
	string_to_bytelist(Value#drop_event.file),
	uint32_to_bytelist(Value#drop_event.windowID)],
	lists:flatten(Return).

bytelist_to_drop_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{File, R3} = parse_string(R2),
	{WindowID, R4} = parse_uint32(R3),
	#drop_event{type=Type, timestamp=Timestamp, file=File, windowID=WindowID}.

parse_drop_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{File, R3} = parse_string(R2),
	{WindowID, R4} = parse_uint32(R3),
	{#drop_event{type=Type, timestamp=Timestamp, file=File, windowID=WindowID}, R4}.

pointer_deref_drop_event(Pointer) ->
	Code = int_to_bytelist(482),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_drop_event(DataList);
		Msg ->
			{error, Msg}
	end.

new_drop_event() ->
	Code = int_to_bytelist(483),
	sdl_port ! {self(), {command, [Code]}},
	receive
		{_, { data, DataList}} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

delete_drop_event(Pointer) ->
	Code = int_to_bytelist(484),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{_, { data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_type(Pointer) ->
	Code = int_to_bytelist(485),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(486),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(487),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(488),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_file(Pointer) ->
	Code = int_to_bytelist(489),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_file(Pointer, Attrib) ->
	Code = int_to_bytelist(490),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(491),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(492),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_event(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_event(Bytelist) ->
	parse_pointer(Bytelist).

event_get_type(Pointer) ->
	Code = int_to_bytelist(493),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(494),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_common(Pointer) ->
	Code = int_to_bytelist(495),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_common_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_common(Pointer, Attrib) ->
	Code = int_to_bytelist(496),
	PList = pointer_to_bytelist(Pointer),
	AList = common_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_window(Pointer) ->
	Code = int_to_bytelist(497),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_window_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_window(Pointer, Attrib) ->
	Code = int_to_bytelist(498),
	PList = pointer_to_bytelist(Pointer),
	AList = window_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_key(Pointer) ->
	Code = int_to_bytelist(499),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_keyboard_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_key(Pointer, Attrib) ->
	Code = int_to_bytelist(500),
	PList = pointer_to_bytelist(Pointer),
	AList = keyboard_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_edit(Pointer) ->
	Code = int_to_bytelist(501),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_text_editing_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_edit(Pointer, Attrib) ->
	Code = int_to_bytelist(502),
	PList = pointer_to_bytelist(Pointer),
	AList = text_editing_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_text(Pointer) ->
	Code = int_to_bytelist(503),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_text_input_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(504),
	PList = pointer_to_bytelist(Pointer),
	AList = text_input_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_motion(Pointer) ->
	Code = int_to_bytelist(505),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_mouse_motion_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_motion(Pointer, Attrib) ->
	Code = int_to_bytelist(506),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_motion_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_button(Pointer) ->
	Code = int_to_bytelist(507),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_mouse_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(508),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_button_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_wheel(Pointer) ->
	Code = int_to_bytelist(509),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_mouse_wheel_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_wheel(Pointer, Attrib) ->
	Code = int_to_bytelist(510),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_wheel_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jaxis(Pointer) ->
	Code = int_to_bytelist(511),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joy_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jaxis(Pointer, Attrib) ->
	Code = int_to_bytelist(512),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_axis_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jball(Pointer) ->
	Code = int_to_bytelist(513),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joy_ball_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jball(Pointer, Attrib) ->
	Code = int_to_bytelist(514),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_ball_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jhat(Pointer) ->
	Code = int_to_bytelist(515),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joy_hat_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jhat(Pointer, Attrib) ->
	Code = int_to_bytelist(516),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_hat_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jbutton(Pointer) ->
	Code = int_to_bytelist(517),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joy_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jbutton(Pointer, Attrib) ->
	Code = int_to_bytelist(518),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_button_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jdevice(Pointer) ->
	Code = int_to_bytelist(519),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_joy_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jdevice(Pointer, Attrib) ->
	Code = int_to_bytelist(520),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_device_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_caxis(Pointer) ->
	Code = int_to_bytelist(521),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_controller_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_caxis(Pointer, Attrib) ->
	Code = int_to_bytelist(522),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_axis_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_cbutton(Pointer) ->
	Code = int_to_bytelist(523),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_controller_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_cbutton(Pointer, Attrib) ->
	Code = int_to_bytelist(524),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_button_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_cdevice(Pointer) ->
	Code = int_to_bytelist(525),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_controller_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_cdevice(Pointer, Attrib) ->
	Code = int_to_bytelist(526),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_device_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_adevice(Pointer) ->
	Code = int_to_bytelist(527),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_audio_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_adevice(Pointer, Attrib) ->
	Code = int_to_bytelist(528),
	PList = pointer_to_bytelist(Pointer),
	AList = audio_device_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_quit(Pointer) ->
	Code = int_to_bytelist(529),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_quit_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_quit(Pointer, Attrib) ->
	Code = int_to_bytelist(530),
	PList = pointer_to_bytelist(Pointer),
	AList = quit_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_user(Pointer) ->
	Code = int_to_bytelist(531),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_user_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_user(Pointer, Attrib) ->
	Code = int_to_bytelist(532),
	PList = pointer_to_bytelist(Pointer),
	AList = user_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_syswm(Pointer) ->
	Code = int_to_bytelist(533),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_syswm_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_syswm(Pointer, Attrib) ->
	Code = int_to_bytelist(534),
	PList = pointer_to_bytelist(Pointer),
	AList = syswm_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_tfinger(Pointer) ->
	Code = int_to_bytelist(535),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_touch_finger_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_tfinger(Pointer, Attrib) ->
	Code = int_to_bytelist(536),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_finger_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_mgesture(Pointer) ->
	Code = int_to_bytelist(537),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_multi_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_mgesture(Pointer, Attrib) ->
	Code = int_to_bytelist(538),
	PList = pointer_to_bytelist(Pointer),
	AList = multi_gesture_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_dgesture(Pointer) ->
	Code = int_to_bytelist(539),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_dollar_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_dgesture(Pointer, Attrib) ->
	Code = int_to_bytelist(540),
	PList = pointer_to_bytelist(Pointer),
	AList = dollar_gesture_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_drop(Pointer) ->
	Code = int_to_bytelist(541),
	PList = pointer_to_bytelist(Pointer),
	sdl_port ! {self(), {command, [Code, PList]}},
	receive
		{ _, { data, DataList }} ->
			bytelist_to_drop_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_drop(Pointer, Attrib) ->
	Code = int_to_bytelist(542),
	PList = pointer_to_bytelist(Pointer),
	AList = drop_event_to_bytelist(Attrib),
	sdl_port ! {self(), {command, [Code, PList, AList]}},
	receive
		{ _, { data, _DataList }} ->
			ok;
		Msg ->
			{error, Msg}
	end.

%--------------------------------------------------------

init(Uint32_1) ->
	Code = int_to_bytelist(543),
	Param1 = uint32_to_bytelist(Uint32_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_int(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

quit() ->
	Code = int_to_bytelist(544),
	sdl_port ! {self(), {command, [Code, []]}},
	receive
		{_, {data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

create_window(String_1, Int_2, Int_3, Int_4, Int_5, Uint32_6) ->
	Code = int_to_bytelist(545),
	Param1 = string_to_bytelist(String_1),
	Param2 = int_to_bytelist(Int_2),
	Param3 = int_to_bytelist(Int_3),
	Param4 = int_to_bytelist(Int_4),
	Param5 = int_to_bytelist(Int_5),
	Param6 = uint32_to_bytelist(Uint32_6),
	sdl_port ! {self(), {command, [Code, Param1, Param2, Param3, Param4, Param5, Param6]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_pointer(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

get_window_surface(P_Window_1) ->
	Code = int_to_bytelist(546),
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_pointer(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

load_bmp(String_1) ->
	Code = int_to_bytelist(547),
	Param1 = string_to_bytelist(String_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_pointer(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

free_surface(P_Surface_1) ->
	Code = int_to_bytelist(548),
	Param1 = pointer_to_bytelist(P_Surface_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

blit_surface(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Code = int_to_bytelist(549),
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),
	sdl_port ! {self(), {command, [Code, Param1, Param2, Param3, Param4]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_int(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

blit_scaled(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Code = int_to_bytelist(550),
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),
	sdl_port ! {self(), {command, [Code, Param1, Param2, Param3, Param4]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_int(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

update_window_surface(P_Window_1) ->
	Code = int_to_bytelist(551),
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_int(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

destroy_window(P_Window_1) ->
	Code = int_to_bytelist(552),
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, _DataList}} ->
			ok;
		Msg ->
			{error, Msg}
	end.

get_window_size(P_Window_1) ->
	Code = int_to_bytelist(553),
	Param1 = pointer_to_bytelist(P_Window_1),
	sdl_port ! {self(), {command, [Code, Param1]}},
	receive
		{_, {data, DataList}} ->
			R0 = DataList,
			{RetParam1, R1} = parse_int(R0),
			{RetParam2, R2} = parse_int(R1),
			{RetParam1, RetParam2};
		Msg ->
			{error, Msg}
	end.

get_error() ->
	Code = int_to_bytelist(554),
	sdl_port ! {self(), {command, [Code, []]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_string(DataList),
			RetParam1;
		Msg ->
			{error, Msg}
	end.

poll_event() ->
	Code = int_to_bytelist(555),
	sdl_port ! {self(), {command, [Code, []]}},
	receive
		{_, {data, DataList}} ->
			{RetParam1, R1} = parse_int(DataList),
			{RetParam2, R2} = parse_event(R1),
			{RetParam1, RetParam2};
		Msg ->
			{error, Msg}
	end.

