-module(sdl_ports_gen).
-include("sdl_ports_gen.hrl").

-compile(export_all).

%--------------------------------------------------------

-export([
	init_port/0,
	pointer_deref_int8/1,
	pointer_deref_int8_array/2,
	pointer_deref_int8_assign/2,
	pointer_deref_int8_array_assign/3,
	new_int8/0,
	new_int8_array/1,
	delete_int8/1,
	pointer_deref_int16/1,
	pointer_deref_int16_array/2,
	pointer_deref_int16_assign/2,
	pointer_deref_int16_array_assign/3,
	new_int16/0,
	new_int16_array/1,
	delete_int16/1,
	pointer_deref_int32/1,
	pointer_deref_int32_array/2,
	pointer_deref_int32_assign/2,
	pointer_deref_int32_array_assign/3,
	new_int32/0,
	new_int32_array/1,
	delete_int32/1,
	pointer_deref_int64/1,
	pointer_deref_int64_array/2,
	pointer_deref_int64_assign/2,
	pointer_deref_int64_array_assign/3,
	new_int64/0,
	new_int64_array/1,
	delete_int64/1,
	pointer_deref_float/1,
	pointer_deref_float_array/2,
	pointer_deref_float_assign/2,
	pointer_deref_float_array_assign/3,
	new_float/0,
	new_float_array/1,
	delete_float/1,
	pointer_deref_double/1,
	pointer_deref_double_array/2,
	pointer_deref_double_assign/2,
	pointer_deref_double_array_assign/3,
	new_double/0,
	new_double_array/1,
	delete_double/1,
	pointer_deref_string/2,
	pointer_deref_string_array/3,
	pointer_deref_string_assign/3,
	pointer_deref_string_array_assign/4,
	new_string/0,
	new_string_array/1,
	delete_string/1,
	pointer_deref_pointer/1,
	pointer_deref_pointer_array/2,
	pointer_deref_pointer_assign/2,
	pointer_deref_pointer_array_assign/3,
	new_pointer/0,
	new_pointer_array/1,
	delete_pointer/1,
	pointer_deref_arrayA/1,
	pointer_deref_arrayA_array/2,
	pointer_deref_arrayA_assign/2,
	pointer_deref_arrayA_array_assign/3,
	new_arrayA/0,
	new_arrayA_array/1,
	delete_arrayA/1,
	arrayA_get_id/1,
	arrayA_set_id/2,
	arrayA_get_values/1,
	arrayA_set_values/2,
	pointer_deref_arrayB/1,
	pointer_deref_arrayB_array/2,
	pointer_deref_arrayB_assign/2,
	pointer_deref_arrayB_array_assign/3,
	new_arrayB/0,
	new_arrayB_array/1,
	delete_arrayB/1,
	arrayB_get_id/1,
	arrayB_set_id/2,
	arrayB_get_values/1,
	arrayB_set_values/2,
	pointer_deref_arrayC/1,
	pointer_deref_arrayC_array/2,
	pointer_deref_arrayC_assign/2,
	pointer_deref_arrayC_array_assign/3,
	new_arrayC/0,
	new_arrayC_array/1,
	delete_arrayC/1,
	arrayC_get_id/1,
	arrayC_set_id/2,
	arrayC_get_values/1,
	arrayC_set_values/2,
	arrayC_get_size/1,
	arrayC_set_size/2,
	pointer_deref_window_array/2,
	pointer_deref_window_array_assign/3,
	new_window_array/1,
	delete_window/1,
	pointer_deref_color/1,
	pointer_deref_color_array/2,
	pointer_deref_color_assign/2,
	pointer_deref_color_array_assign/3,
	new_color/0,
	new_color_array/1,
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
	pointer_deref_palette_array/2,
	pointer_deref_palette_assign/2,
	pointer_deref_palette_array_assign/3,
	new_palette/0,
	new_palette_array/1,
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
	pointer_deref_pixel_format_array/2,
	pointer_deref_pixel_format_assign/2,
	pointer_deref_pixel_format_array_assign/3,
	new_pixel_format/0,
	new_pixel_format_array/1,
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
	pointer_deref_rect_array/2,
	pointer_deref_rect_assign/2,
	pointer_deref_rect_array_assign/3,
	new_rect/0,
	new_rect_array/1,
	delete_rect/1,
	rect_get_x/1,
	rect_set_x/2,
	rect_get_y/1,
	rect_set_y/2,
	rect_get_w/1,
	rect_set_w/2,
	rect_get_h/1,
	rect_set_h/2,
	pointer_deref_blit_map_array/2,
	pointer_deref_blit_map_array_assign/3,
	new_blit_map_array/1,
	delete_blit_map/1,
	pointer_deref_surface/1,
	pointer_deref_surface_array/2,
	pointer_deref_surface_assign/2,
	pointer_deref_surface_array_assign/3,
	new_surface/0,
	new_surface_array/1,
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
	pointer_deref_keysym_array/2,
	pointer_deref_keysym_assign/2,
	pointer_deref_keysym_array_assign/3,
	new_keysym/0,
	new_keysym_array/1,
	delete_keysym/1,
	keysym_get_scancode/1,
	keysym_set_scancode/2,
	keysym_get_sym/1,
	keysym_set_sym/2,
	keysym_get_mod/1,
	keysym_set_mod/2,
	keysym_get_unused/1,
	keysym_set_unused/2,
	pointer_deref_syswm_msg_array/2,
	pointer_deref_syswm_msg_array_assign/3,
	new_syswm_msg_array/1,
	delete_syswm_msg/1,
	pointer_deref_common_event/1,
	pointer_deref_common_event_array/2,
	pointer_deref_common_event_assign/2,
	pointer_deref_common_event_array_assign/3,
	new_common_event/0,
	new_common_event_array/1,
	delete_common_event/1,
	common_event_get_type/1,
	common_event_set_type/2,
	common_event_get_timestamp/1,
	common_event_set_timestamp/2,
	pointer_deref_window_event/1,
	pointer_deref_window_event_array/2,
	pointer_deref_window_event_assign/2,
	pointer_deref_window_event_array_assign/3,
	new_window_event/0,
	new_window_event_array/1,
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
	pointer_deref_keyboard_event_array/2,
	pointer_deref_keyboard_event_assign/2,
	pointer_deref_keyboard_event_array_assign/3,
	new_keyboard_event/0,
	new_keyboard_event_array/1,
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
	pointer_deref_text_editing_event_array/2,
	pointer_deref_text_editing_event_assign/2,
	pointer_deref_text_editing_event_array_assign/3,
	new_text_editing_event/0,
	new_text_editing_event_array/1,
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
	pointer_deref_text_input_event_array/2,
	pointer_deref_text_input_event_assign/2,
	pointer_deref_text_input_event_array_assign/3,
	new_text_input_event/0,
	new_text_input_event_array/1,
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
	pointer_deref_mouse_motion_event_array/2,
	pointer_deref_mouse_motion_event_assign/2,
	pointer_deref_mouse_motion_event_array_assign/3,
	new_mouse_motion_event/0,
	new_mouse_motion_event_array/1,
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
	pointer_deref_mouse_button_event_array/2,
	pointer_deref_mouse_button_event_assign/2,
	pointer_deref_mouse_button_event_array_assign/3,
	new_mouse_button_event/0,
	new_mouse_button_event_array/1,
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
	pointer_deref_mouse_wheel_event_array/2,
	pointer_deref_mouse_wheel_event_assign/2,
	pointer_deref_mouse_wheel_event_array_assign/3,
	new_mouse_wheel_event/0,
	new_mouse_wheel_event_array/1,
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
	pointer_deref_joy_axis_event_array/2,
	pointer_deref_joy_axis_event_assign/2,
	pointer_deref_joy_axis_event_array_assign/3,
	new_joy_axis_event/0,
	new_joy_axis_event_array/1,
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
	pointer_deref_joy_ball_event_array/2,
	pointer_deref_joy_ball_event_assign/2,
	pointer_deref_joy_ball_event_array_assign/3,
	new_joy_ball_event/0,
	new_joy_ball_event_array/1,
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
	pointer_deref_joy_hat_event_array/2,
	pointer_deref_joy_hat_event_assign/2,
	pointer_deref_joy_hat_event_array_assign/3,
	new_joy_hat_event/0,
	new_joy_hat_event_array/1,
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
	pointer_deref_joy_button_event_array/2,
	pointer_deref_joy_button_event_assign/2,
	pointer_deref_joy_button_event_array_assign/3,
	new_joy_button_event/0,
	new_joy_button_event_array/1,
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
	pointer_deref_joy_device_event_array/2,
	pointer_deref_joy_device_event_assign/2,
	pointer_deref_joy_device_event_array_assign/3,
	new_joy_device_event/0,
	new_joy_device_event_array/1,
	delete_joy_device_event/1,
	joy_device_event_get_type/1,
	joy_device_event_set_type/2,
	joy_device_event_get_timestamp/1,
	joy_device_event_set_timestamp/2,
	joy_device_event_get_which/1,
	joy_device_event_set_which/2,
	pointer_deref_controller_axis_event/1,
	pointer_deref_controller_axis_event_array/2,
	pointer_deref_controller_axis_event_assign/2,
	pointer_deref_controller_axis_event_array_assign/3,
	new_controller_axis_event/0,
	new_controller_axis_event_array/1,
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
	pointer_deref_controller_button_event_array/2,
	pointer_deref_controller_button_event_assign/2,
	pointer_deref_controller_button_event_array_assign/3,
	new_controller_button_event/0,
	new_controller_button_event_array/1,
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
	pointer_deref_controller_device_event_array/2,
	pointer_deref_controller_device_event_assign/2,
	pointer_deref_controller_device_event_array_assign/3,
	new_controller_device_event/0,
	new_controller_device_event_array/1,
	delete_controller_device_event/1,
	controller_device_event_get_type/1,
	controller_device_event_set_type/2,
	controller_device_event_get_timestamp/1,
	controller_device_event_set_timestamp/2,
	controller_device_event_get_which/1,
	controller_device_event_set_which/2,
	pointer_deref_audio_device_event/1,
	pointer_deref_audio_device_event_array/2,
	pointer_deref_audio_device_event_assign/2,
	pointer_deref_audio_device_event_array_assign/3,
	new_audio_device_event/0,
	new_audio_device_event_array/1,
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
	pointer_deref_quit_event_array/2,
	pointer_deref_quit_event_assign/2,
	pointer_deref_quit_event_array_assign/3,
	new_quit_event/0,
	new_quit_event_array/1,
	delete_quit_event/1,
	quit_event_get_type/1,
	quit_event_set_type/2,
	quit_event_get_timestamp/1,
	quit_event_set_timestamp/2,
	pointer_deref_user_event/1,
	pointer_deref_user_event_array/2,
	pointer_deref_user_event_assign/2,
	pointer_deref_user_event_array_assign/3,
	new_user_event/0,
	new_user_event_array/1,
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
	pointer_deref_syswm_event_array/2,
	pointer_deref_syswm_event_assign/2,
	pointer_deref_syswm_event_array_assign/3,
	new_syswm_event/0,
	new_syswm_event_array/1,
	delete_syswm_event/1,
	syswm_event_get_type/1,
	syswm_event_set_type/2,
	syswm_event_get_timestamp/1,
	syswm_event_set_timestamp/2,
	syswm_event_get_msg/1,
	syswm_event_set_msg/2,
	pointer_deref_touch_finger_event/1,
	pointer_deref_touch_finger_event_array/2,
	pointer_deref_touch_finger_event_assign/2,
	pointer_deref_touch_finger_event_array_assign/3,
	new_touch_finger_event/0,
	new_touch_finger_event_array/1,
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
	pointer_deref_multi_gesture_event_array/2,
	pointer_deref_multi_gesture_event_assign/2,
	pointer_deref_multi_gesture_event_array_assign/3,
	new_multi_gesture_event/0,
	new_multi_gesture_event_array/1,
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
	pointer_deref_dollar_gesture_event_array/2,
	pointer_deref_dollar_gesture_event_assign/2,
	pointer_deref_dollar_gesture_event_array_assign/3,
	new_dollar_gesture_event/0,
	new_dollar_gesture_event_array/1,
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
	pointer_deref_drop_event_array/2,
	pointer_deref_drop_event_assign/2,
	pointer_deref_drop_event_array_assign/3,
	new_drop_event/0,
	new_drop_event_array/1,
	delete_drop_event/1,
	drop_event_get_type/1,
	drop_event_set_type/2,
	drop_event_get_timestamp/1,
	drop_event_set_timestamp/2,
	drop_event_get_file/1,
	drop_event_set_file/2,
	drop_event_get_windowID/1,
	drop_event_set_windowID/2,
	new_event/0,
	new_event_array/1,
	delete_event/1,
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
	poll_event/0,
	maxint/2,
	apply_int/3]).

-ifdef(debug).
-define(DEBUG(X, Y), io:format(X, Y)).
-else.
-define(DEBUG(X, Y), ok).
-endif.

-define(PORT_NAME, sdl_port).

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
	Pid = spawn(fun() -> start_port_owner("../sdl_generator/generated/sdl_ports_gen") end),
	%io:format("PID Owner: ~w~n", [Pid]),
	register(?PORT_NAME, Pid),
	code:ensure_loaded(erlang_gc),
	io:format("sdl_port initialized.~n"),
	ok.

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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 8);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_array(Pointer, Index) ->
	Code = int_to_bytelist(2),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 8);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_assign(Pointer, Value) ->
	Code = int_to_bytelist(3),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 8),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int8_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(4),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 8),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int8() ->
	Code = int_to_bytelist(5),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int8_auto() ->
	Pointer = new_int8(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int8, P);
		Error -> Error
	end.

new_int8_array(Size) ->
	Code = int_to_bytelist(6),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int8_array_auto(Size) ->
	Pointer = new_int8_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int8, P);
		Error -> Error
	end.

delete_int8(Pointer) ->
	Code = int_to_bytelist(7),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 16);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_array(Pointer, Index) ->
	Code = int_to_bytelist(9),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 16);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_assign(Pointer, Value) ->
	Code = int_to_bytelist(10),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 16),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int16_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(11),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 16),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int16() ->
	Code = int_to_bytelist(12),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int16_auto() ->
	Pointer = new_int16(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int16, P);
		Error -> Error
	end.

new_int16_array(Size) ->
	Code = int_to_bytelist(13),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int16_array_auto(Size) ->
	Pointer = new_int16_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int16, P);
		Error -> Error
	end.

delete_int16(Pointer) ->
	Code = int_to_bytelist(14),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 32);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_array(Pointer, Index) ->
	Code = int_to_bytelist(16),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 32);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_assign(Pointer, Value) ->
	Code = int_to_bytelist(17),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 32),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int32_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(18),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 32),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int32() ->
	Code = int_to_bytelist(19),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int32_auto() ->
	Pointer = new_int32(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int32, P);
		Error -> Error
	end.

new_int32_array(Size) ->
	Code = int_to_bytelist(20),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int32_array_auto(Size) ->
	Pointer = new_int32_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int32, P);
		Error -> Error
	end.

delete_int32(Pointer) ->
	Code = int_to_bytelist(21),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 64);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_array(Pointer, Index) ->
	Code = int_to_bytelist(23),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList, 64);
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_assign(Pointer, Value) ->
	Code = int_to_bytelist(24),
	PList = pointer_to_bytelist(Pointer),
	VList = int_to_bytelist(Value, 64),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_int64_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(25),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = int_to_bytelist(Value, 64),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_int64() ->
	Code = int_to_bytelist(26),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int64_auto() ->
	Pointer = new_int64(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int64, P);
		Error -> Error
	end.

new_int64_array(Size) ->
	Code = int_to_bytelist(27),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_int64_array_auto(Size) ->
	Pointer = new_int64_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_int64, P);
		Error -> Error
	end.

delete_int64(Pointer) ->
	Code = int_to_bytelist(28),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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

new_int_auto() ->
	new_int32_auto().

new_int_array(Size) ->
	new_int32_array(Size).

new_int_array_auto(Size) ->
	new_int32_array_auto(Size).

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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_array(Pointer, Index) ->
	Code = int_to_bytelist(30),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_assign(Pointer, Value) ->
	Code = int_to_bytelist(31),
	PList = pointer_to_bytelist(Pointer),
	VList = float_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_float_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(32),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = float_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_float() ->
	Code = int_to_bytelist(33),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_float_auto() ->
	Pointer = new_float(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_float, P);
		Error -> Error
	end.

new_float_array(Size) ->
	Code = int_to_bytelist(34),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_float_array_auto(Size) ->
	Pointer = new_float_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_float, P);
		Error -> Error
	end.

delete_float(Pointer) ->
	Code = int_to_bytelist(35),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_double(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_array(Pointer, Index) ->
	Code = int_to_bytelist(37),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_double(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_assign(Pointer, Value) ->
	Code = int_to_bytelist(38),
	PList = pointer_to_bytelist(Pointer),
	VList = double_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_double_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(39),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = double_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_double() ->
	Code = int_to_bytelist(40),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_double_auto() ->
	Pointer = new_double(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_double, P);
		Error -> Error
	end.

new_double_array(Size) ->
	Code = int_to_bytelist(41),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_double_array_auto(Size) ->
	Pointer = new_double_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_double, P);
		Error -> Error
	end.

delete_double(Pointer) ->
	Code = int_to_bytelist(42),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_string(DataList, Enconding);
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_array(Pointer, Index, Enconding) ->
	Code = int_to_bytelist(44),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_string(DataList, Enconding);
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_assign(Pointer, Value, Enconding) ->
	Code = int_to_bytelist(45),
	PList = pointer_to_bytelist(Pointer),
	VList = string_to_bytelist(Value, Enconding),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_string_array_assign(Pointer, Index, Value, Enconding) ->
	Code = int_to_bytelist(46),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = string_to_bytelist(Value, Enconding),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_string() ->
	Code = int_to_bytelist(47),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_string_auto() ->
	Pointer = new_string(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_string, P);
		Error -> Error
	end.

new_string_array(Size) ->
	Code = int_to_bytelist(48),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_string_array_auto(Size) ->
	Pointer = new_string_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_string, P);
		Error -> Error
	end.

delete_string(Pointer) ->
	Code = int_to_bytelist(49),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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

get_ptr({managed_pointer, _} = Managed) ->
	erlang_gc:get_wrapped_pointer(Managed);
get_ptr({raw_pointer, Ptr}) -> Ptr.

pointer_to_bytelist(Value) ->
	int_to_bytelist(get_ptr(Value), 64).

bytelist_to_pointer(Bytelist) ->
	{raw_pointer, bytelist_to_int(Bytelist, 64)}.

parse_pointer(Bytelist) ->
	{RawPtr, RestBytes} = parse_int(Bytelist, 64),
	{{raw_pointer, RawPtr}, RestBytes}.

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
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_array(Pointer, Index) ->
	Code = int_to_bytelist(51),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_assign(Pointer, Value) ->
	Code = int_to_bytelist(52),
	PList = pointer_to_bytelist(Pointer),
	VList = pointer_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_pointer_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(53),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = pointer_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_pointer() ->
	Code = int_to_bytelist(54),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_pointer_auto() ->
	Pointer = new_pointer(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_pointer, P);
		Error -> Error
	end.

new_pointer_array(Size) ->
	Code = int_to_bytelist(55),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_pointer_array_auto(Size) ->
	Pointer = new_pointer_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_pointer, P);
		Error -> Error
	end.

delete_pointer(Pointer) ->
	Code = int_to_bytelist(56),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
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

%--------------------------------------------------------

arrayA_to_bytelist(Value) ->
	Return = [int_to_bytelist(Value#arrayA.id),
	int_array_to_bytelist(Value#arrayA.values, 10)],
	lists:flatten(Return).

bytelist_to_arrayA(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, _} = parse_int_array(R1, 10),
	#arrayA{id=Id, values=Values}.

parse_arrayA(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, R2} = parse_int_array(R1, 10),
	{#arrayA{id=Id, values=Values}, R2}.

arrayA_array_to_bytelist(List, Size) when length(List)==Size ->
	[arrayA_to_bytelist(E) || E<-List].

bytelist_to_arrayA_array(Bytelist, Size) ->
	bytelist_to_arrayA_array(Bytelist, Size, []).
bytelist_to_arrayA_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_arrayA_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayA(Bytelist),
	bytelist_to_arrayA_array(Rest, Size-1, [Elem|Result]).

parse_arrayA_array(Bytelist, Size) ->
	parse_arrayA_array(Bytelist, Size, []).
parse_arrayA_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_arrayA_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayA(Bytelist),
	parse_arrayA_array(Rest, Size-1, [Elem|Result]).

pointer_deref_arrayA(Pointer) ->
	Code = int_to_bytelist(57),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayA(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayA_array(Pointer, Index) ->
	Code = int_to_bytelist(58),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayA(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayA_assign(Pointer, Value) ->
	Code = int_to_bytelist(59),
	PList = pointer_to_bytelist(Pointer),
	VList = arrayA_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayA_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(60),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = arrayA_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_arrayA() ->
	Code = int_to_bytelist(61),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayA_auto() ->
	Pointer = new_arrayA(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayA, P);
		Error -> Error
	end.

new_arrayA_array(Size) ->
	Code = int_to_bytelist(62),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayA_array_auto(Size) ->
	Pointer = new_arrayA_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayA, P);
		Error -> Error
	end.

delete_arrayA(Pointer) ->
	Code = int_to_bytelist(63),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_arrayA_array(List) ->
	Size = length(List),
	Pointer = new_arrayA_array(Size),
	list_to_arrayA_array(List, Pointer, 0).
list_to_arrayA_array([], Pointer, _Index) -> Pointer;
list_to_arrayA_array([Value|List], Pointer, Index) ->
	pointer_deref_arrayA_array_assign(Pointer, Index, Value),
	list_to_arrayA_array(List, Pointer, Index+1).

arrayA_array_to_list(Pointer, Size) ->
	arrayA_array_to_list(Pointer, Size-1, []).
arrayA_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
arrayA_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_arrayA_array(Pointer, Size),
	arrayA_array_to_list(Pointer, Size-1, [Elem|Result]).

arrayA_get_id(Pointer) ->
	Code = int_to_bytelist(64),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

arrayA_set_id(Pointer, Attrib) ->
	Code = int_to_bytelist(65),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayA_get_values(Pointer) ->
	Code = int_to_bytelist(66),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int_array(DataList, 10);
		Msg ->
			{error, Msg}
	end.

arrayA_set_values(Pointer, Attrib) ->
	Code = int_to_bytelist(67),
	PList = pointer_to_bytelist(Pointer),
	AList = int_array_to_bytelist(Attrib, 10),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayB_to_bytelist(Value) ->
	Return = [int_to_bytelist(Value#arrayB.id),
	pointer_to_bytelist(Value#arrayB.values)],
	lists:flatten(Return).

bytelist_to_arrayB(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, _} = parse_pointer(R1),
	#arrayB{id=Id, values=Values}.

parse_arrayB(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, R2} = parse_pointer(R1),
	{#arrayB{id=Id, values=Values}, R2}.

arrayB_array_to_bytelist(List, Size) when length(List)==Size ->
	[arrayB_to_bytelist(E) || E<-List].

bytelist_to_arrayB_array(Bytelist, Size) ->
	bytelist_to_arrayB_array(Bytelist, Size, []).
bytelist_to_arrayB_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_arrayB_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayB(Bytelist),
	bytelist_to_arrayB_array(Rest, Size-1, [Elem|Result]).

parse_arrayB_array(Bytelist, Size) ->
	parse_arrayB_array(Bytelist, Size, []).
parse_arrayB_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_arrayB_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayB(Bytelist),
	parse_arrayB_array(Rest, Size-1, [Elem|Result]).

pointer_deref_arrayB(Pointer) ->
	Code = int_to_bytelist(68),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayB(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayB_array(Pointer, Index) ->
	Code = int_to_bytelist(69),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayB(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayB_assign(Pointer, Value) ->
	Code = int_to_bytelist(70),
	PList = pointer_to_bytelist(Pointer),
	VList = arrayB_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayB_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(71),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = arrayB_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_arrayB() ->
	Code = int_to_bytelist(72),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayB_auto() ->
	Pointer = new_arrayB(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayB, P);
		Error -> Error
	end.

new_arrayB_array(Size) ->
	Code = int_to_bytelist(73),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayB_array_auto(Size) ->
	Pointer = new_arrayB_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayB, P);
		Error -> Error
	end.

delete_arrayB(Pointer) ->
	Code = int_to_bytelist(74),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_arrayB_array(List) ->
	Size = length(List),
	Pointer = new_arrayB_array(Size),
	list_to_arrayB_array(List, Pointer, 0).
list_to_arrayB_array([], Pointer, _Index) -> Pointer;
list_to_arrayB_array([Value|List], Pointer, Index) ->
	pointer_deref_arrayB_array_assign(Pointer, Index, Value),
	list_to_arrayB_array(List, Pointer, Index+1).

arrayB_array_to_list(Pointer, Size) ->
	arrayB_array_to_list(Pointer, Size-1, []).
arrayB_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
arrayB_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_arrayB_array(Pointer, Size),
	arrayB_array_to_list(Pointer, Size-1, [Elem|Result]).

arrayB_get_id(Pointer) ->
	Code = int_to_bytelist(75),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

arrayB_set_id(Pointer, Attrib) ->
	Code = int_to_bytelist(76),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayB_get_values(Pointer) ->
	Code = int_to_bytelist(77),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

arrayB_set_values(Pointer, Attrib) ->
	Code = int_to_bytelist(78),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayC_to_bytelist(Value) ->
	Return = [int_to_bytelist(Value#arrayC.id),
	pointer_to_bytelist(Value#arrayC.values),
	int_to_bytelist(Value#arrayC.size)],
	lists:flatten(Return).

bytelist_to_arrayC(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, R2} = parse_pointer(R1),
	{Size, _} = parse_int(R2),
	#arrayC{id=Id, values=Values, size=Size}.

parse_arrayC(Bytelist) ->
	R0 = Bytelist,
	{Id, R1} = parse_int(R0),
	{Values, R2} = parse_pointer(R1),
	{Size, R3} = parse_int(R2),
	{#arrayC{id=Id, values=Values, size=Size}, R3}.

arrayC_array_to_bytelist(List, Size) when length(List)==Size ->
	[arrayC_to_bytelist(E) || E<-List].

bytelist_to_arrayC_array(Bytelist, Size) ->
	bytelist_to_arrayC_array(Bytelist, Size, []).
bytelist_to_arrayC_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_arrayC_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayC(Bytelist),
	bytelist_to_arrayC_array(Rest, Size-1, [Elem|Result]).

parse_arrayC_array(Bytelist, Size) ->
	parse_arrayC_array(Bytelist, Size, []).
parse_arrayC_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_arrayC_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_arrayC(Bytelist),
	parse_arrayC_array(Rest, Size-1, [Elem|Result]).

pointer_deref_arrayC(Pointer) ->
	Code = int_to_bytelist(79),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayC(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayC_array(Pointer, Index) ->
	Code = int_to_bytelist(80),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_arrayC(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayC_assign(Pointer, Value) ->
	Code = int_to_bytelist(81),
	PList = pointer_to_bytelist(Pointer),
	VList = arrayC_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_arrayC_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(82),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = arrayC_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_arrayC() ->
	Code = int_to_bytelist(83),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayC_auto() ->
	Pointer = new_arrayC(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayC, P);
		Error -> Error
	end.

new_arrayC_array(Size) ->
	Code = int_to_bytelist(84),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_arrayC_array_auto(Size) ->
	Pointer = new_arrayC_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_arrayC, P);
		Error -> Error
	end.

delete_arrayC(Pointer) ->
	Code = int_to_bytelist(85),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_arrayC_array(List) ->
	Size = length(List),
	Pointer = new_arrayC_array(Size),
	list_to_arrayC_array(List, Pointer, 0).
list_to_arrayC_array([], Pointer, _Index) -> Pointer;
list_to_arrayC_array([Value|List], Pointer, Index) ->
	pointer_deref_arrayC_array_assign(Pointer, Index, Value),
	list_to_arrayC_array(List, Pointer, Index+1).

arrayC_array_to_list(Pointer, Size) ->
	arrayC_array_to_list(Pointer, Size-1, []).
arrayC_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
arrayC_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_arrayC_array(Pointer, Size),
	arrayC_array_to_list(Pointer, Size-1, [Elem|Result]).

arrayC_get_id(Pointer) ->
	Code = int_to_bytelist(86),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

arrayC_set_id(Pointer, Attrib) ->
	Code = int_to_bytelist(87),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayC_get_values(Pointer) ->
	Code = int_to_bytelist(88),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

arrayC_set_values(Pointer, Attrib) ->
	Code = int_to_bytelist(89),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayC_get_size(Pointer) ->
	Code = int_to_bytelist(90),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

arrayC_set_size(Pointer, Attrib) ->
	Code = int_to_bytelist(91),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

arrayC_get_arraylist_values(Pointer) ->
	ArrayPtr = arrayC_get_values(Pointer),
	Size = arrayC_get_size(Pointer),
	int_array_to_list(ArrayPtr, Size).

arrayC_set_arraylist_values(Pointer, List) ->
	ArrayPtr = arrayC_get_values(Pointer),
	Size = arrayC_get_size(Pointer),
	arrayC_set_arraylist_values(ArrayPtr, List, Size, 0).
arrayC_set_arraylist_values(_ArrayPtr, _List, Size, Index) when Size==Index -> ok;
arrayC_set_arraylist_values(ArrayPtr, [Value|List], Size, Index) ->
	pointer_deref_int_array_assign(ArrayPtr, Index, Value),
	arrayC_set_arraylist_values(ArrayPtr, List, Size, Index+1).

uint64_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_uint64(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_uint64(Bytelist) ->
	parse_int(Bytelist, 64).

uint64_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 64).

bytelist_to_uint64_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 64).

parse_uint64_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 64).

pointer_deref_uint64(Pointer) ->
	pointer_deref_int64(Pointer).

pointer_deref_uint64_array(Pointer, Index) ->
	pointer_deref_int64_array(Pointer, Index).

pointer_deref_uint64_assign(Pointer, Value) ->
	pointer_deref_int64_assign(Pointer, Value).

pointer_deref_uint64_array_assign(Pointer, Index, Value) ->
	pointer_deref_int64_array_assign(Pointer, Index, Value).

new_uint64() ->
	new_int64().

new_uint64_auto() ->
	new_int64_auto().

new_uint64_array(Size) ->
	new_int64_array(Size).

new_uint64_array_auto(Size) ->
	new_int64_array_auto(Size).

delete_uint64(Pointer) ->
	delete_int64(Pointer).

list_to_uint64_array(List) ->
	list_to_int64_array(List).

uint64_array_to_list(Pointer, Size) ->
	int64_array_to_list(Pointer, Size).

uint32_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

bytelist_to_uint32(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

parse_uint32(Bytelist) ->
	parse_int(Bytelist, 32).

uint32_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 32).

bytelist_to_uint32_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 32).

parse_uint32_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 32).

pointer_deref_uint32(Pointer) ->
	pointer_deref_int32(Pointer).

pointer_deref_uint32_array(Pointer, Index) ->
	pointer_deref_int32_array(Pointer, Index).

pointer_deref_uint32_assign(Pointer, Value) ->
	pointer_deref_int32_assign(Pointer, Value).

pointer_deref_uint32_array_assign(Pointer, Index, Value) ->
	pointer_deref_int32_array_assign(Pointer, Index, Value).

new_uint32() ->
	new_int32().

new_uint32_auto() ->
	new_int32_auto().

new_uint32_array(Size) ->
	new_int32_array(Size).

new_uint32_array_auto(Size) ->
	new_int32_array_auto(Size).

delete_uint32(Pointer) ->
	delete_int32(Pointer).

list_to_uint32_array(List) ->
	list_to_int32_array(List).

uint32_array_to_list(Pointer, Size) ->
	int32_array_to_list(Pointer, Size).

uint16_to_bytelist(Value) ->
	int_to_bytelist(Value, 16).

bytelist_to_uint16(Bytelist) ->
	bytelist_to_int(Bytelist, 16).

parse_uint16(Bytelist) ->
	parse_int(Bytelist, 16).

uint16_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 16).

bytelist_to_uint16_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 16).

parse_uint16_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 16).

pointer_deref_uint16(Pointer) ->
	pointer_deref_int16(Pointer).

pointer_deref_uint16_array(Pointer, Index) ->
	pointer_deref_int16_array(Pointer, Index).

pointer_deref_uint16_assign(Pointer, Value) ->
	pointer_deref_int16_assign(Pointer, Value).

pointer_deref_uint16_array_assign(Pointer, Index, Value) ->
	pointer_deref_int16_array_assign(Pointer, Index, Value).

new_uint16() ->
	new_int16().

new_uint16_auto() ->
	new_int16_auto().

new_uint16_array(Size) ->
	new_int16_array(Size).

new_uint16_array_auto(Size) ->
	new_int16_array_auto(Size).

delete_uint16(Pointer) ->
	delete_int16(Pointer).

list_to_uint16_array(List) ->
	list_to_int16_array(List).

uint16_array_to_list(Pointer, Size) ->
	int16_array_to_list(Pointer, Size).

uint8_to_bytelist(Value) ->
	int_to_bytelist(Value, 8).

bytelist_to_uint8(Bytelist) ->
	bytelist_to_int(Bytelist, 8).

parse_uint8(Bytelist) ->
	parse_int(Bytelist, 8).

uint8_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 8).

bytelist_to_uint8_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 8).

parse_uint8_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 8).

pointer_deref_uint8(Pointer) ->
	pointer_deref_int8(Pointer).

pointer_deref_uint8_array(Pointer, Index) ->
	pointer_deref_int8_array(Pointer, Index).

pointer_deref_uint8_assign(Pointer, Value) ->
	pointer_deref_int8_assign(Pointer, Value).

pointer_deref_uint8_array_assign(Pointer, Index, Value) ->
	pointer_deref_int8_array_assign(Pointer, Index, Value).

new_uint8() ->
	new_int8().

new_uint8_auto() ->
	new_int8_auto().

new_uint8_array(Size) ->
	new_int8_array(Size).

new_uint8_array_auto(Size) ->
	new_int8_array_auto(Size).

delete_uint8(Pointer) ->
	delete_int8(Pointer).

list_to_uint8_array(List) ->
	list_to_int8_array(List).

uint8_array_to_list(Pointer, Size) ->
	int8_array_to_list(Pointer, Size).

sint64_to_bytelist(Value) ->
	int_to_bytelist(Value, 64).

bytelist_to_sint64(Bytelist) ->
	bytelist_to_int(Bytelist, 64).

parse_sint64(Bytelist) ->
	parse_int(Bytelist, 64).

sint64_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 64).

bytelist_to_sint64_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 64).

parse_sint64_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 64).

pointer_deref_sint64(Pointer) ->
	pointer_deref_int64(Pointer).

pointer_deref_sint64_array(Pointer, Index) ->
	pointer_deref_int64_array(Pointer, Index).

pointer_deref_sint64_assign(Pointer, Value) ->
	pointer_deref_int64_assign(Pointer, Value).

pointer_deref_sint64_array_assign(Pointer, Index, Value) ->
	pointer_deref_int64_array_assign(Pointer, Index, Value).

new_sint64() ->
	new_int64().

new_sint64_auto() ->
	new_int64_auto().

new_sint64_array(Size) ->
	new_int64_array(Size).

new_sint64_array_auto(Size) ->
	new_int64_array_auto(Size).

delete_sint64(Pointer) ->
	delete_int64(Pointer).

list_to_sint64_array(List) ->
	list_to_int64_array(List).

sint64_array_to_list(Pointer, Size) ->
	int64_array_to_list(Pointer, Size).

sint32_to_bytelist(Value) ->
	int_to_bytelist(Value, 32).

bytelist_to_sint32(Bytelist) ->
	bytelist_to_int(Bytelist, 32).

parse_sint32(Bytelist) ->
	parse_int(Bytelist, 32).

sint32_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 32).

bytelist_to_sint32_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 32).

parse_sint32_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 32).

pointer_deref_sint32(Pointer) ->
	pointer_deref_int32(Pointer).

pointer_deref_sint32_array(Pointer, Index) ->
	pointer_deref_int32_array(Pointer, Index).

pointer_deref_sint32_assign(Pointer, Value) ->
	pointer_deref_int32_assign(Pointer, Value).

pointer_deref_sint32_array_assign(Pointer, Index, Value) ->
	pointer_deref_int32_array_assign(Pointer, Index, Value).

new_sint32() ->
	new_int32().

new_sint32_auto() ->
	new_int32_auto().

new_sint32_array(Size) ->
	new_int32_array(Size).

new_sint32_array_auto(Size) ->
	new_int32_array_auto(Size).

delete_sint32(Pointer) ->
	delete_int32(Pointer).

list_to_sint32_array(List) ->
	list_to_int32_array(List).

sint32_array_to_list(Pointer, Size) ->
	int32_array_to_list(Pointer, Size).

sint16_to_bytelist(Value) ->
	int_to_bytelist(Value, 16).

bytelist_to_sint16(Bytelist) ->
	bytelist_to_int(Bytelist, 16).

parse_sint16(Bytelist) ->
	parse_int(Bytelist, 16).

sint16_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 16).

bytelist_to_sint16_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 16).

parse_sint16_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 16).

pointer_deref_sint16(Pointer) ->
	pointer_deref_int16(Pointer).

pointer_deref_sint16_array(Pointer, Index) ->
	pointer_deref_int16_array(Pointer, Index).

pointer_deref_sint16_assign(Pointer, Value) ->
	pointer_deref_int16_assign(Pointer, Value).

pointer_deref_sint16_array_assign(Pointer, Index, Value) ->
	pointer_deref_int16_array_assign(Pointer, Index, Value).

new_sint16() ->
	new_int16().

new_sint16_auto() ->
	new_int16_auto().

new_sint16_array(Size) ->
	new_int16_array(Size).

new_sint16_array_auto(Size) ->
	new_int16_array_auto(Size).

delete_sint16(Pointer) ->
	delete_int16(Pointer).

list_to_sint16_array(List) ->
	list_to_int16_array(List).

sint16_array_to_list(Pointer, Size) ->
	int16_array_to_list(Pointer, Size).

sint8_to_bytelist(Value) ->
	int_to_bytelist(Value, 8).

bytelist_to_sint8(Bytelist) ->
	bytelist_to_int(Bytelist, 8).

parse_sint8(Bytelist) ->
	parse_int(Bytelist, 8).

sint8_array_to_bytelist(List, Size) ->
	int_array_to_bytelist(List, Size, 8).

bytelist_to_sint8_array(Bytelist, Size) ->
	bytelist_to_int_array(Bytelist, Size, 8).

parse_sint8_array(Bytelist, Size) ->
	parse_int_array(Bytelist, Size, 8).

pointer_deref_sint8(Pointer) ->
	pointer_deref_int8(Pointer).

pointer_deref_sint8_array(Pointer, Index) ->
	pointer_deref_int8_array(Pointer, Index).

pointer_deref_sint8_assign(Pointer, Value) ->
	pointer_deref_int8_assign(Pointer, Value).

pointer_deref_sint8_array_assign(Pointer, Index, Value) ->
	pointer_deref_int8_array_assign(Pointer, Index, Value).

new_sint8() ->
	new_int8().

new_sint8_auto() ->
	new_int8_auto().

new_sint8_array(Size) ->
	new_int8_array(Size).

new_sint8_array_auto(Size) ->
	new_int8_array_auto(Size).

delete_sint8(Pointer) ->
	delete_int8(Pointer).

list_to_sint8_array(List) ->
	list_to_int8_array(List).

sint8_array_to_list(Pointer, Size) ->
	int8_array_to_list(Pointer, Size).

window_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_window(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_window(Bytelist) ->
	parse_pointer(Bytelist).

pointer_deref_window_array(Pointer, Index) ->
	pointer_deref_pointer_array(Pointer, Index).

pointer_deref_window_array_assign(Pointer, Index, Value) ->
	pointer_deref_pointer_array_assign(Pointer, Index, Value).

window_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_window_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_window_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_window_array(Size) ->
	new_pointer_array(Size).

new_window_array_auto(Size) ->
	new_pointer_array_auto(Size).

delete_window(Pointer) ->
	delete_pointer(Pointer).

list_to_window_array(List) ->
	list_to_pointer_array(List).

window_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

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
	{A, _} = parse_uint8(R3),
	#color{r=R, g=G, b=B, a=A}.

parse_color(Bytelist) ->
	R0 = Bytelist,
	{R, R1} = parse_uint8(R0),
	{G, R2} = parse_uint8(R1),
	{B, R3} = parse_uint8(R2),
	{A, R4} = parse_uint8(R3),
	{#color{r=R, g=G, b=B, a=A}, R4}.

color_array_to_bytelist(List, Size) when length(List)==Size ->
	[color_to_bytelist(E) || E<-List].

bytelist_to_color_array(Bytelist, Size) ->
	bytelist_to_color_array(Bytelist, Size, []).
bytelist_to_color_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_color_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_color(Bytelist),
	bytelist_to_color_array(Rest, Size-1, [Elem|Result]).

parse_color_array(Bytelist, Size) ->
	parse_color_array(Bytelist, Size, []).
parse_color_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_color_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_color(Bytelist),
	parse_color_array(Rest, Size-1, [Elem|Result]).

pointer_deref_color(Pointer) ->
	Code = int_to_bytelist(92),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_color(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_color_array(Pointer, Index) ->
	Code = int_to_bytelist(93),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_color(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_color_assign(Pointer, Value) ->
	Code = int_to_bytelist(94),
	PList = pointer_to_bytelist(Pointer),
	VList = color_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_color_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(95),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = color_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_color() ->
	Code = int_to_bytelist(96),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_color_auto() ->
	Pointer = new_color(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_color, P);
		Error -> Error
	end.

new_color_array(Size) ->
	Code = int_to_bytelist(97),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_color_array_auto(Size) ->
	Pointer = new_color_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_color, P);
		Error -> Error
	end.

delete_color(Pointer) ->
	Code = int_to_bytelist(98),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_color_array(List) ->
	Size = length(List),
	Pointer = new_color_array(Size),
	list_to_color_array(List, Pointer, 0).
list_to_color_array([], Pointer, _Index) -> Pointer;
list_to_color_array([Value|List], Pointer, Index) ->
	pointer_deref_color_array_assign(Pointer, Index, Value),
	list_to_color_array(List, Pointer, Index+1).

color_array_to_list(Pointer, Size) ->
	color_array_to_list(Pointer, Size-1, []).
color_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
color_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_color_array(Pointer, Size),
	color_array_to_list(Pointer, Size-1, [Elem|Result]).

color_get_r(Pointer) ->
	Code = int_to_bytelist(99),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_r(Pointer, Attrib) ->
	Code = int_to_bytelist(100),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_g(Pointer) ->
	Code = int_to_bytelist(101),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_g(Pointer, Attrib) ->
	Code = int_to_bytelist(102),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_b(Pointer) ->
	Code = int_to_bytelist(103),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_b(Pointer, Attrib) ->
	Code = int_to_bytelist(104),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

color_get_a(Pointer) ->
	Code = int_to_bytelist(105),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

color_set_a(Pointer, Attrib) ->
	Code = int_to_bytelist(106),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Refcount, _} = parse_int(R3),
	#palette{ncolors=Ncolors, colors=Colors, version=Version, refcount=Refcount}.

parse_palette(Bytelist) ->
	R0 = Bytelist,
	{Ncolors, R1} = parse_int(R0),
	{Colors, R2} = parse_pointer(R1),
	{Version, R3} = parse_uint32(R2),
	{Refcount, R4} = parse_int(R3),
	{#palette{ncolors=Ncolors, colors=Colors, version=Version, refcount=Refcount}, R4}.

palette_array_to_bytelist(List, Size) when length(List)==Size ->
	[palette_to_bytelist(E) || E<-List].

bytelist_to_palette_array(Bytelist, Size) ->
	bytelist_to_palette_array(Bytelist, Size, []).
bytelist_to_palette_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_palette_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_palette(Bytelist),
	bytelist_to_palette_array(Rest, Size-1, [Elem|Result]).

parse_palette_array(Bytelist, Size) ->
	parse_palette_array(Bytelist, Size, []).
parse_palette_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_palette_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_palette(Bytelist),
	parse_palette_array(Rest, Size-1, [Elem|Result]).

pointer_deref_palette(Pointer) ->
	Code = int_to_bytelist(107),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_palette(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_palette_array(Pointer, Index) ->
	Code = int_to_bytelist(108),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_palette(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_palette_assign(Pointer, Value) ->
	Code = int_to_bytelist(109),
	PList = pointer_to_bytelist(Pointer),
	VList = palette_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_palette_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(110),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = palette_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_palette() ->
	Code = int_to_bytelist(111),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_palette_auto() ->
	Pointer = new_palette(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_palette, P);
		Error -> Error
	end.

new_palette_array(Size) ->
	Code = int_to_bytelist(112),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_palette_array_auto(Size) ->
	Pointer = new_palette_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_palette, P);
		Error -> Error
	end.

delete_palette(Pointer) ->
	Code = int_to_bytelist(113),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_palette_array(List) ->
	Size = length(List),
	Pointer = new_palette_array(Size),
	list_to_palette_array(List, Pointer, 0).
list_to_palette_array([], Pointer, _Index) -> Pointer;
list_to_palette_array([Value|List], Pointer, Index) ->
	pointer_deref_palette_array_assign(Pointer, Index, Value),
	list_to_palette_array(List, Pointer, Index+1).

palette_array_to_list(Pointer, Size) ->
	palette_array_to_list(Pointer, Size-1, []).
palette_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
palette_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_palette_array(Pointer, Size),
	palette_array_to_list(Pointer, Size-1, [Elem|Result]).

palette_get_ncolors(Pointer) ->
	Code = int_to_bytelist(114),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_ncolors(Pointer, Attrib) ->
	Code = int_to_bytelist(115),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_colors(Pointer) ->
	Code = int_to_bytelist(116),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_colors(Pointer, Attrib) ->
	Code = int_to_bytelist(117),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_version(Pointer) ->
	Code = int_to_bytelist(118),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_version(Pointer, Attrib) ->
	Code = int_to_bytelist(119),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

palette_get_refcount(Pointer) ->
	Code = int_to_bytelist(120),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

palette_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(121),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Next, _} = parse_pointer(R17),
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

pixel_format_array_to_bytelist(List, Size) when length(List)==Size ->
	[pixel_format_to_bytelist(E) || E<-List].

bytelist_to_pixel_format_array(Bytelist, Size) ->
	bytelist_to_pixel_format_array(Bytelist, Size, []).
bytelist_to_pixel_format_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_pixel_format_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_pixel_format(Bytelist),
	bytelist_to_pixel_format_array(Rest, Size-1, [Elem|Result]).

parse_pixel_format_array(Bytelist, Size) ->
	parse_pixel_format_array(Bytelist, Size, []).
parse_pixel_format_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_pixel_format_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_pixel_format(Bytelist),
	parse_pixel_format_array(Rest, Size-1, [Elem|Result]).

pointer_deref_pixel_format(Pointer) ->
	Code = int_to_bytelist(122),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pixel_format(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pixel_format_array(Pointer, Index) ->
	Code = int_to_bytelist(123),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pixel_format(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_pixel_format_assign(Pointer, Value) ->
	Code = int_to_bytelist(124),
	PList = pointer_to_bytelist(Pointer),
	VList = pixel_format_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_pixel_format_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(125),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = pixel_format_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_pixel_format() ->
	Code = int_to_bytelist(126),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_pixel_format_auto() ->
	Pointer = new_pixel_format(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_pixel_format, P);
		Error -> Error
	end.

new_pixel_format_array(Size) ->
	Code = int_to_bytelist(127),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_pixel_format_array_auto(Size) ->
	Pointer = new_pixel_format_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_pixel_format, P);
		Error -> Error
	end.

delete_pixel_format(Pointer) ->
	Code = int_to_bytelist(128),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_pixel_format_array(List) ->
	Size = length(List),
	Pointer = new_pixel_format_array(Size),
	list_to_pixel_format_array(List, Pointer, 0).
list_to_pixel_format_array([], Pointer, _Index) -> Pointer;
list_to_pixel_format_array([Value|List], Pointer, Index) ->
	pointer_deref_pixel_format_array_assign(Pointer, Index, Value),
	list_to_pixel_format_array(List, Pointer, Index+1).

pixel_format_array_to_list(Pointer, Size) ->
	pixel_format_array_to_list(Pointer, Size-1, []).
pixel_format_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
pixel_format_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_pixel_format_array(Pointer, Size),
	pixel_format_array_to_list(Pointer, Size-1, [Elem|Result]).

pixel_format_get_format(Pointer) ->
	Code = int_to_bytelist(129),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_format(Pointer, Attrib) ->
	Code = int_to_bytelist(130),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_palette(Pointer) ->
	Code = int_to_bytelist(131),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_palette(Pointer, Attrib) ->
	Code = int_to_bytelist(132),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_bits_per_pixel(Pointer) ->
	Code = int_to_bytelist(133),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_bits_per_pixel(Pointer, Attrib) ->
	Code = int_to_bytelist(134),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_bytes_per_pixel(Pointer) ->
	Code = int_to_bytelist(135),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_bytes_per_pixel(Pointer, Attrib) ->
	Code = int_to_bytelist(136),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_mask(Pointer) ->
	Code = int_to_bytelist(137),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(138),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_mask(Pointer) ->
	Code = int_to_bytelist(139),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(140),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_mask(Pointer) ->
	Code = int_to_bytelist(141),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(142),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_mask(Pointer) ->
	Code = int_to_bytelist(143),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_mask(Pointer, Attrib) ->
	Code = int_to_bytelist(144),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_loss(Pointer) ->
	Code = int_to_bytelist(145),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(146),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_loss(Pointer) ->
	Code = int_to_bytelist(147),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(148),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_loss(Pointer) ->
	Code = int_to_bytelist(149),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(150),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_loss(Pointer) ->
	Code = int_to_bytelist(151),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_loss(Pointer, Attrib) ->
	Code = int_to_bytelist(152),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_r_shift(Pointer) ->
	Code = int_to_bytelist(153),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_r_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(154),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_g_shift(Pointer) ->
	Code = int_to_bytelist(155),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_g_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(156),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_b_shift(Pointer) ->
	Code = int_to_bytelist(157),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_b_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(158),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_a_shift(Pointer) ->
	Code = int_to_bytelist(159),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_a_shift(Pointer, Attrib) ->
	Code = int_to_bytelist(160),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_refcount(Pointer) ->
	Code = int_to_bytelist(161),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(162),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pixel_format_get_next(Pointer) ->
	Code = int_to_bytelist(163),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

pixel_format_set_next(Pointer, Attrib) ->
	Code = int_to_bytelist(164),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{H, _} = parse_int(R3),
	#rect{x=X, y=Y, w=W, h=H}.

parse_rect(Bytelist) ->
	R0 = Bytelist,
	{X, R1} = parse_int(R0),
	{Y, R2} = parse_int(R1),
	{W, R3} = parse_int(R2),
	{H, R4} = parse_int(R3),
	{#rect{x=X, y=Y, w=W, h=H}, R4}.

rect_array_to_bytelist(List, Size) when length(List)==Size ->
	[rect_to_bytelist(E) || E<-List].

bytelist_to_rect_array(Bytelist, Size) ->
	bytelist_to_rect_array(Bytelist, Size, []).
bytelist_to_rect_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_rect_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_rect(Bytelist),
	bytelist_to_rect_array(Rest, Size-1, [Elem|Result]).

parse_rect_array(Bytelist, Size) ->
	parse_rect_array(Bytelist, Size, []).
parse_rect_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_rect_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_rect(Bytelist),
	parse_rect_array(Rest, Size-1, [Elem|Result]).

pointer_deref_rect(Pointer) ->
	Code = int_to_bytelist(165),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_rect(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_rect_array(Pointer, Index) ->
	Code = int_to_bytelist(166),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_rect(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_rect_assign(Pointer, Value) ->
	Code = int_to_bytelist(167),
	PList = pointer_to_bytelist(Pointer),
	VList = rect_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_rect_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(168),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = rect_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_rect() ->
	Code = int_to_bytelist(169),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_rect_auto() ->
	Pointer = new_rect(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_rect, P);
		Error -> Error
	end.

new_rect_array(Size) ->
	Code = int_to_bytelist(170),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_rect_array_auto(Size) ->
	Pointer = new_rect_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_rect, P);
		Error -> Error
	end.

delete_rect(Pointer) ->
	Code = int_to_bytelist(171),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_rect_array(List) ->
	Size = length(List),
	Pointer = new_rect_array(Size),
	list_to_rect_array(List, Pointer, 0).
list_to_rect_array([], Pointer, _Index) -> Pointer;
list_to_rect_array([Value|List], Pointer, Index) ->
	pointer_deref_rect_array_assign(Pointer, Index, Value),
	list_to_rect_array(List, Pointer, Index+1).

rect_array_to_list(Pointer, Size) ->
	rect_array_to_list(Pointer, Size-1, []).
rect_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
rect_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_rect_array(Pointer, Size),
	rect_array_to_list(Pointer, Size-1, [Elem|Result]).

rect_get_x(Pointer) ->
	Code = int_to_bytelist(172),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(173),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_y(Pointer) ->
	Code = int_to_bytelist(174),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(175),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_w(Pointer) ->
	Code = int_to_bytelist(176),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_w(Pointer, Attrib) ->
	Code = int_to_bytelist(177),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

rect_get_h(Pointer) ->
	Code = int_to_bytelist(178),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

rect_set_h(Pointer, Attrib) ->
	Code = int_to_bytelist(179),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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

pointer_deref_blit_map_array(Pointer, Index) ->
	pointer_deref_pointer_array(Pointer, Index).

pointer_deref_blit_map_array_assign(Pointer, Index, Value) ->
	pointer_deref_pointer_array_assign(Pointer, Index, Value).

blit_map_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_blit_map_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_blit_map_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_blit_map_array(Size) ->
	new_pointer_array(Size).

new_blit_map_array_auto(Size) ->
	new_pointer_array_auto(Size).

delete_blit_map(Pointer) ->
	delete_pointer(Pointer).

list_to_blit_map_array(List) ->
	list_to_pointer_array(List).

blit_map_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

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
	{Refcount, _} = parse_int(R11),
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

surface_array_to_bytelist(List, Size) when length(List)==Size ->
	[surface_to_bytelist(E) || E<-List].

bytelist_to_surface_array(Bytelist, Size) ->
	bytelist_to_surface_array(Bytelist, Size, []).
bytelist_to_surface_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_surface_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_surface(Bytelist),
	bytelist_to_surface_array(Rest, Size-1, [Elem|Result]).

parse_surface_array(Bytelist, Size) ->
	parse_surface_array(Bytelist, Size, []).
parse_surface_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_surface_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_surface(Bytelist),
	parse_surface_array(Rest, Size-1, [Elem|Result]).

pointer_deref_surface(Pointer) ->
	Code = int_to_bytelist(180),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_surface(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_surface_array(Pointer, Index) ->
	Code = int_to_bytelist(181),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_surface(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_surface_assign(Pointer, Value) ->
	Code = int_to_bytelist(182),
	PList = pointer_to_bytelist(Pointer),
	VList = surface_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_surface_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(183),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = surface_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_surface() ->
	Code = int_to_bytelist(184),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_surface_auto() ->
	Pointer = new_surface(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_surface, P);
		Error -> Error
	end.

new_surface_array(Size) ->
	Code = int_to_bytelist(185),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_surface_array_auto(Size) ->
	Pointer = new_surface_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_surface, P);
		Error -> Error
	end.

delete_surface(Pointer) ->
	Code = int_to_bytelist(186),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_surface_array(List) ->
	Size = length(List),
	Pointer = new_surface_array(Size),
	list_to_surface_array(List, Pointer, 0).
list_to_surface_array([], Pointer, _Index) -> Pointer;
list_to_surface_array([Value|List], Pointer, Index) ->
	pointer_deref_surface_array_assign(Pointer, Index, Value),
	list_to_surface_array(List, Pointer, Index+1).

surface_array_to_list(Pointer, Size) ->
	surface_array_to_list(Pointer, Size-1, []).
surface_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
surface_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_surface_array(Pointer, Size),
	surface_array_to_list(Pointer, Size-1, [Elem|Result]).

surface_get_flags(Pointer) ->
	Code = int_to_bytelist(187),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_flags(Pointer, Attrib) ->
	Code = int_to_bytelist(188),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_format(Pointer) ->
	Code = int_to_bytelist(189),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_format(Pointer, Attrib) ->
	Code = int_to_bytelist(190),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_w(Pointer) ->
	Code = int_to_bytelist(191),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_w(Pointer, Attrib) ->
	Code = int_to_bytelist(192),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_h(Pointer) ->
	Code = int_to_bytelist(193),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_h(Pointer, Attrib) ->
	Code = int_to_bytelist(194),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_pitch(Pointer) ->
	Code = int_to_bytelist(195),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_pitch(Pointer, Attrib) ->
	Code = int_to_bytelist(196),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_pixels(Pointer) ->
	Code = int_to_bytelist(197),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_pixels(Pointer, Attrib) ->
	Code = int_to_bytelist(198),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_userdata(Pointer) ->
	Code = int_to_bytelist(199),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_userdata(Pointer, Attrib) ->
	Code = int_to_bytelist(200),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_locked(Pointer) ->
	Code = int_to_bytelist(201),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_locked(Pointer, Attrib) ->
	Code = int_to_bytelist(202),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_lock_data(Pointer) ->
	Code = int_to_bytelist(203),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_lock_data(Pointer, Attrib) ->
	Code = int_to_bytelist(204),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_clip_rect(Pointer) ->
	Code = int_to_bytelist(205),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_rect(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_clip_rect(Pointer, Attrib) ->
	Code = int_to_bytelist(206),
	PList = pointer_to_bytelist(Pointer),
	AList = rect_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_map(Pointer) ->
	Code = int_to_bytelist(207),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_map(Pointer, Attrib) ->
	Code = int_to_bytelist(208),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

surface_get_refcount(Pointer) ->
	Code = int_to_bytelist(209),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_int(DataList);
		Msg ->
			{error, Msg}
	end.

surface_set_refcount(Pointer, Attrib) ->
	Code = int_to_bytelist(210),
	PList = pointer_to_bytelist(Pointer),
	AList = int_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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

scancode_array_to_bytelist(List, Size) when length(List)==Size ->
	[int_to_bytelist(scancode_get_int(E)) || E<-List].

bytelist_to_scancode_array(Bytelist, Size) ->
	bytelist_to_scancode_array(Bytelist, Size, []).
bytelist_to_scancode_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_scancode_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_int(Bytelist),
	bytelist_to_scancode_array(Rest, Size-1, [scancode_get_atom(Elem)|Result]).

parse_scancode_array(Bytelist, Size) ->
	parse_scancode_array(Bytelist, Size, []).
parse_scancode_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_scancode_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_int(Bytelist),
	parse_scancode_array(Rest, Size-1, [scancode_get_atom(Elem)|Result]).

pointer_deref_scancode(Pointer) ->
	scancode_get_atom(pointer_deref_int(Pointer)).

pointer_deref_scancode_array(Pointer, Index) ->
	scancode_get_atom(pointer_deref_int_array(Pointer, Index)).

pointer_deref_scancode_assign(Pointer, Value) ->
	pointer_deref_int_assign(Pointer, scancode_get_int(Value)).

pointer_deref_scancode_array_assign(Pointer, Index, Value) ->
	pointer_deref_int_array_assign(Pointer, Index, scancode_get_int(Value)).

new_scancode() ->
	new_int().

new_scancode_auto() ->
	new_int_auto().

new_scancode_array(Size) ->
	new_int_array(Size).

new_scancode_array_auto(Size) ->
	new_int_array_auto(Size).

delete_scancode(Pointer) ->
	delete_int(Pointer).

list_to_scancode_array(List) ->
	Size = length(List),
	Pointer = new_scancode_array(Size),
	list_to_scancode_array(List, Pointer, 0).
list_to_scancode_array([], Pointer, _Index) -> Pointer;
list_to_scancode_array([Value|List], Pointer, Index) ->
	pointer_deref_scancode_array_assign(Pointer, Index, Value),
	list_to_scancode_array(List, Pointer, Index+1).

scancode_array_to_list(Pointer, Size) ->
	scancode_array_to_list(Pointer, Size-1, []).
scancode_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
scancode_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_scancode_array(Pointer, Size),
	scancode_array_to_list(Pointer, Size-1, [Elem|Result]).

keycode_to_bytelist(Value) ->
	sint32_to_bytelist(Value).

bytelist_to_keycode(Bytelist) ->
	bytelist_to_sint32(Bytelist).

parse_keycode(Bytelist) ->
	parse_sint32(Bytelist).

keycode_array_to_bytelist(List, Size) ->
	sint32_array_to_bytelist(List, Size).

bytelist_to_keycode_array(Bytelist, Size) ->
	bytelist_to_sint32_array(Bytelist, Size).

parse_keycode_array(Bytelist, Size) ->
	parse_sint32_array(Bytelist, Size).

pointer_deref_keycode(Pointer) ->
	pointer_deref_sint32(Pointer).

pointer_deref_keycode_array(Pointer, Index) ->
	pointer_deref_sint32_array(Pointer, Index).

pointer_deref_keycode_assign(Pointer, Value) ->
	pointer_deref_sint32_assign(Pointer, Value).

pointer_deref_keycode_array_assign(Pointer, Index, Value) ->
	pointer_deref_sint32_array_assign(Pointer, Index, Value).

new_keycode() ->
	new_sint32().

new_keycode_auto() ->
	new_sint32_auto().

new_keycode_array(Size) ->
	new_sint32_array(Size).

new_keycode_array_auto(Size) ->
	new_sint32_array_auto(Size).

delete_keycode(Pointer) ->
	delete_sint32(Pointer).

list_to_keycode_array(List) ->
	list_to_sint32_array(List).

keycode_array_to_list(Pointer, Size) ->
	sint32_array_to_list(Pointer, Size).

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
	{Unused, _} = parse_uint32(R3),
	#keysym{scancode=Scancode, sym=Sym, mod=Mod, unused=Unused}.

parse_keysym(Bytelist) ->
	R0 = Bytelist,
	{Scancode, R1} = parse_scancode(R0),
	{Sym, R2} = parse_keycode(R1),
	{Mod, R3} = parse_uint16(R2),
	{Unused, R4} = parse_uint32(R3),
	{#keysym{scancode=Scancode, sym=Sym, mod=Mod, unused=Unused}, R4}.

keysym_array_to_bytelist(List, Size) when length(List)==Size ->
	[keysym_to_bytelist(E) || E<-List].

bytelist_to_keysym_array(Bytelist, Size) ->
	bytelist_to_keysym_array(Bytelist, Size, []).
bytelist_to_keysym_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_keysym_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_keysym(Bytelist),
	bytelist_to_keysym_array(Rest, Size-1, [Elem|Result]).

parse_keysym_array(Bytelist, Size) ->
	parse_keysym_array(Bytelist, Size, []).
parse_keysym_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_keysym_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_keysym(Bytelist),
	parse_keysym_array(Rest, Size-1, [Elem|Result]).

pointer_deref_keysym(Pointer) ->
	Code = int_to_bytelist(211),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keysym(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_keysym_array(Pointer, Index) ->
	Code = int_to_bytelist(212),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keysym(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_keysym_assign(Pointer, Value) ->
	Code = int_to_bytelist(213),
	PList = pointer_to_bytelist(Pointer),
	VList = keysym_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_keysym_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(214),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = keysym_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_keysym() ->
	Code = int_to_bytelist(215),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_keysym_auto() ->
	Pointer = new_keysym(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_keysym, P);
		Error -> Error
	end.

new_keysym_array(Size) ->
	Code = int_to_bytelist(216),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_keysym_array_auto(Size) ->
	Pointer = new_keysym_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_keysym, P);
		Error -> Error
	end.

delete_keysym(Pointer) ->
	Code = int_to_bytelist(217),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_keysym_array(List) ->
	Size = length(List),
	Pointer = new_keysym_array(Size),
	list_to_keysym_array(List, Pointer, 0).
list_to_keysym_array([], Pointer, _Index) -> Pointer;
list_to_keysym_array([Value|List], Pointer, Index) ->
	pointer_deref_keysym_array_assign(Pointer, Index, Value),
	list_to_keysym_array(List, Pointer, Index+1).

keysym_array_to_list(Pointer, Size) ->
	keysym_array_to_list(Pointer, Size-1, []).
keysym_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
keysym_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_keysym_array(Pointer, Size),
	keysym_array_to_list(Pointer, Size-1, [Elem|Result]).

keysym_get_scancode(Pointer) ->
	Code = int_to_bytelist(218),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_scancode(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_scancode(Pointer, Attrib) ->
	Code = int_to_bytelist(219),
	PList = pointer_to_bytelist(Pointer),
	AList = scancode_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_sym(Pointer) ->
	Code = int_to_bytelist(220),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keycode(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_sym(Pointer, Attrib) ->
	Code = int_to_bytelist(221),
	PList = pointer_to_bytelist(Pointer),
	AList = keycode_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_mod(Pointer) ->
	Code = int_to_bytelist(222),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_mod(Pointer, Attrib) ->
	Code = int_to_bytelist(223),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keysym_get_unused(Pointer) ->
	Code = int_to_bytelist(224),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keysym_set_unused(Pointer, Attrib) ->
	Code = int_to_bytelist(225),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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

joystick_id_array_to_bytelist(List, Size) ->
	sint32_array_to_bytelist(List, Size).

bytelist_to_joystick_id_array(Bytelist, Size) ->
	bytelist_to_sint32_array(Bytelist, Size).

parse_joystick_id_array(Bytelist, Size) ->
	parse_sint32_array(Bytelist, Size).

pointer_deref_joystick_id(Pointer) ->
	pointer_deref_sint32(Pointer).

pointer_deref_joystick_id_array(Pointer, Index) ->
	pointer_deref_sint32_array(Pointer, Index).

pointer_deref_joystick_id_assign(Pointer, Value) ->
	pointer_deref_sint32_assign(Pointer, Value).

pointer_deref_joystick_id_array_assign(Pointer, Index, Value) ->
	pointer_deref_sint32_array_assign(Pointer, Index, Value).

new_joystick_id() ->
	new_sint32().

new_joystick_id_auto() ->
	new_sint32_auto().

new_joystick_id_array(Size) ->
	new_sint32_array(Size).

new_joystick_id_array_auto(Size) ->
	new_sint32_array_auto(Size).

delete_joystick_id(Pointer) ->
	delete_sint32(Pointer).

list_to_joystick_id_array(List) ->
	list_to_sint32_array(List).

joystick_id_array_to_list(Pointer, Size) ->
	sint32_array_to_list(Pointer, Size).

syswm_msg_to_bytelist(Value) ->
	pointer_to_bytelist(Value).

bytelist_to_syswm_msg(Bytelist) ->
	bytelist_to_pointer(Bytelist).

parse_syswm_msg(Bytelist) ->
	parse_pointer(Bytelist).

pointer_deref_syswm_msg_array(Pointer, Index) ->
	pointer_deref_pointer_array(Pointer, Index).

pointer_deref_syswm_msg_array_assign(Pointer, Index, Value) ->
	pointer_deref_pointer_array_assign(Pointer, Index, Value).

syswm_msg_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_syswm_msg_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_syswm_msg_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_syswm_msg_array(Size) ->
	new_pointer_array(Size).

new_syswm_msg_array_auto(Size) ->
	new_pointer_array_auto(Size).

delete_syswm_msg(Pointer) ->
	delete_pointer(Pointer).

list_to_syswm_msg_array(List) ->
	list_to_pointer_array(List).

syswm_msg_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

touch_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_touch_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_touch_id(Bytelist) ->
	parse_sint64(Bytelist).

touch_id_array_to_bytelist(List, Size) ->
	sint64_array_to_bytelist(List, Size).

bytelist_to_touch_id_array(Bytelist, Size) ->
	bytelist_to_sint64_array(Bytelist, Size).

parse_touch_id_array(Bytelist, Size) ->
	parse_sint64_array(Bytelist, Size).

pointer_deref_touch_id(Pointer) ->
	pointer_deref_sint64(Pointer).

pointer_deref_touch_id_array(Pointer, Index) ->
	pointer_deref_sint64_array(Pointer, Index).

pointer_deref_touch_id_assign(Pointer, Value) ->
	pointer_deref_sint64_assign(Pointer, Value).

pointer_deref_touch_id_array_assign(Pointer, Index, Value) ->
	pointer_deref_sint64_array_assign(Pointer, Index, Value).

new_touch_id() ->
	new_sint64().

new_touch_id_auto() ->
	new_sint64_auto().

new_touch_id_array(Size) ->
	new_sint64_array(Size).

new_touch_id_array_auto(Size) ->
	new_sint64_array_auto(Size).

delete_touch_id(Pointer) ->
	delete_sint64(Pointer).

list_to_touch_id_array(List) ->
	list_to_sint64_array(List).

touch_id_array_to_list(Pointer, Size) ->
	sint64_array_to_list(Pointer, Size).

finger_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_finger_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_finger_id(Bytelist) ->
	parse_sint64(Bytelist).

finger_id_array_to_bytelist(List, Size) ->
	sint64_array_to_bytelist(List, Size).

bytelist_to_finger_id_array(Bytelist, Size) ->
	bytelist_to_sint64_array(Bytelist, Size).

parse_finger_id_array(Bytelist, Size) ->
	parse_sint64_array(Bytelist, Size).

pointer_deref_finger_id(Pointer) ->
	pointer_deref_sint64(Pointer).

pointer_deref_finger_id_array(Pointer, Index) ->
	pointer_deref_sint64_array(Pointer, Index).

pointer_deref_finger_id_assign(Pointer, Value) ->
	pointer_deref_sint64_assign(Pointer, Value).

pointer_deref_finger_id_array_assign(Pointer, Index, Value) ->
	pointer_deref_sint64_array_assign(Pointer, Index, Value).

new_finger_id() ->
	new_sint64().

new_finger_id_auto() ->
	new_sint64_auto().

new_finger_id_array(Size) ->
	new_sint64_array(Size).

new_finger_id_array_auto(Size) ->
	new_sint64_array_auto(Size).

delete_finger_id(Pointer) ->
	delete_sint64(Pointer).

list_to_finger_id_array(List) ->
	list_to_sint64_array(List).

finger_id_array_to_list(Pointer, Size) ->
	sint64_array_to_list(Pointer, Size).

gesture_id_to_bytelist(Value) ->
	sint64_to_bytelist(Value).

bytelist_to_gesture_id(Bytelist) ->
	bytelist_to_sint64(Bytelist).

parse_gesture_id(Bytelist) ->
	parse_sint64(Bytelist).

gesture_id_array_to_bytelist(List, Size) ->
	sint64_array_to_bytelist(List, Size).

bytelist_to_gesture_id_array(Bytelist, Size) ->
	bytelist_to_sint64_array(Bytelist, Size).

parse_gesture_id_array(Bytelist, Size) ->
	parse_sint64_array(Bytelist, Size).

pointer_deref_gesture_id(Pointer) ->
	pointer_deref_sint64(Pointer).

pointer_deref_gesture_id_array(Pointer, Index) ->
	pointer_deref_sint64_array(Pointer, Index).

pointer_deref_gesture_id_assign(Pointer, Value) ->
	pointer_deref_sint64_assign(Pointer, Value).

pointer_deref_gesture_id_array_assign(Pointer, Index, Value) ->
	pointer_deref_sint64_array_assign(Pointer, Index, Value).

new_gesture_id() ->
	new_sint64().

new_gesture_id_auto() ->
	new_sint64_auto().

new_gesture_id_array(Size) ->
	new_sint64_array(Size).

new_gesture_id_array_auto(Size) ->
	new_sint64_array_auto(Size).

delete_gesture_id(Pointer) ->
	delete_sint64(Pointer).

list_to_gesture_id_array(List) ->
	list_to_sint64_array(List).

gesture_id_array_to_list(Pointer, Size) ->
	sint64_array_to_list(Pointer, Size).

common_event_to_bytelist(Value) ->
	Return = [uint32_to_bytelist(Value#common_event.type),
	uint32_to_bytelist(Value#common_event.timestamp)],
	lists:flatten(Return).

bytelist_to_common_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, _} = parse_uint32(R1),
	#common_event{type=Type, timestamp=Timestamp}.

parse_common_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{#common_event{type=Type, timestamp=Timestamp}, R2}.

common_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[common_event_to_bytelist(E) || E<-List].

bytelist_to_common_event_array(Bytelist, Size) ->
	bytelist_to_common_event_array(Bytelist, Size, []).
bytelist_to_common_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_common_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_common_event(Bytelist),
	bytelist_to_common_event_array(Rest, Size-1, [Elem|Result]).

parse_common_event_array(Bytelist, Size) ->
	parse_common_event_array(Bytelist, Size, []).
parse_common_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_common_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_common_event(Bytelist),
	parse_common_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_common_event(Pointer) ->
	Code = int_to_bytelist(226),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_common_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_common_event_array(Pointer, Index) ->
	Code = int_to_bytelist(227),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_common_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_common_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(228),
	PList = pointer_to_bytelist(Pointer),
	VList = common_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_common_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(229),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = common_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_common_event() ->
	Code = int_to_bytelist(230),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_common_event_auto() ->
	Pointer = new_common_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_common_event, P);
		Error -> Error
	end.

new_common_event_array(Size) ->
	Code = int_to_bytelist(231),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_common_event_array_auto(Size) ->
	Pointer = new_common_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_common_event, P);
		Error -> Error
	end.

delete_common_event(Pointer) ->
	Code = int_to_bytelist(232),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_common_event_array(List) ->
	Size = length(List),
	Pointer = new_common_event_array(Size),
	list_to_common_event_array(List, Pointer, 0).
list_to_common_event_array([], Pointer, _Index) -> Pointer;
list_to_common_event_array([Value|List], Pointer, Index) ->
	pointer_deref_common_event_array_assign(Pointer, Index, Value),
	list_to_common_event_array(List, Pointer, Index+1).

common_event_array_to_list(Pointer, Size) ->
	common_event_array_to_list(Pointer, Size-1, []).
common_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
common_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_common_event_array(Pointer, Size),
	common_event_array_to_list(Pointer, Size-1, [Elem|Result]).

common_event_get_type(Pointer) ->
	Code = int_to_bytelist(233),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

common_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(234),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

common_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(235),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

common_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(236),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Data2, _} = parse_sint32(R8),
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

window_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[window_event_to_bytelist(E) || E<-List].

bytelist_to_window_event_array(Bytelist, Size) ->
	bytelist_to_window_event_array(Bytelist, Size, []).
bytelist_to_window_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_window_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_window_event(Bytelist),
	bytelist_to_window_event_array(Rest, Size-1, [Elem|Result]).

parse_window_event_array(Bytelist, Size) ->
	parse_window_event_array(Bytelist, Size, []).
parse_window_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_window_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_window_event(Bytelist),
	parse_window_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_window_event(Pointer) ->
	Code = int_to_bytelist(237),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_window_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_window_event_array(Pointer, Index) ->
	Code = int_to_bytelist(238),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_window_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_window_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(239),
	PList = pointer_to_bytelist(Pointer),
	VList = window_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_window_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(240),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = window_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_window_event() ->
	Code = int_to_bytelist(241),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_window_event_auto() ->
	Pointer = new_window_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_window_event, P);
		Error -> Error
	end.

new_window_event_array(Size) ->
	Code = int_to_bytelist(242),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_window_event_array_auto(Size) ->
	Pointer = new_window_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_window_event, P);
		Error -> Error
	end.

delete_window_event(Pointer) ->
	Code = int_to_bytelist(243),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_window_event_array(List) ->
	Size = length(List),
	Pointer = new_window_event_array(Size),
	list_to_window_event_array(List, Pointer, 0).
list_to_window_event_array([], Pointer, _Index) -> Pointer;
list_to_window_event_array([Value|List], Pointer, Index) ->
	pointer_deref_window_event_array_assign(Pointer, Index, Value),
	list_to_window_event_array(List, Pointer, Index+1).

window_event_array_to_list(Pointer, Size) ->
	window_event_array_to_list(Pointer, Size-1, []).
window_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
window_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_window_event_array(Pointer, Size),
	window_event_array_to_list(Pointer, Size-1, [Elem|Result]).

window_event_get_type(Pointer) ->
	Code = int_to_bytelist(244),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(245),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(246),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(247),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(248),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(249),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_event(Pointer) ->
	Code = int_to_bytelist(250),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_event(Pointer, Attrib) ->
	Code = int_to_bytelist(251),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(252),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(253),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(254),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(255),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(256),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(257),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_data1(Pointer) ->
	Code = int_to_bytelist(258),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_data1(Pointer, Attrib) ->
	Code = int_to_bytelist(259),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

window_event_get_data2(Pointer) ->
	Code = int_to_bytelist(260),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

window_event_set_data2(Pointer, Attrib) ->
	Code = int_to_bytelist(261),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Keysym, _} = parse_keysym(R7),
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

keyboard_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[keyboard_event_to_bytelist(E) || E<-List].

bytelist_to_keyboard_event_array(Bytelist, Size) ->
	bytelist_to_keyboard_event_array(Bytelist, Size, []).
bytelist_to_keyboard_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_keyboard_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_keyboard_event(Bytelist),
	bytelist_to_keyboard_event_array(Rest, Size-1, [Elem|Result]).

parse_keyboard_event_array(Bytelist, Size) ->
	parse_keyboard_event_array(Bytelist, Size, []).
parse_keyboard_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_keyboard_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_keyboard_event(Bytelist),
	parse_keyboard_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_keyboard_event(Pointer) ->
	Code = int_to_bytelist(262),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keyboard_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_keyboard_event_array(Pointer, Index) ->
	Code = int_to_bytelist(263),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keyboard_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_keyboard_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(264),
	PList = pointer_to_bytelist(Pointer),
	VList = keyboard_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_keyboard_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(265),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = keyboard_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_keyboard_event() ->
	Code = int_to_bytelist(266),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_keyboard_event_auto() ->
	Pointer = new_keyboard_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_keyboard_event, P);
		Error -> Error
	end.

new_keyboard_event_array(Size) ->
	Code = int_to_bytelist(267),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_keyboard_event_array_auto(Size) ->
	Pointer = new_keyboard_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_keyboard_event, P);
		Error -> Error
	end.

delete_keyboard_event(Pointer) ->
	Code = int_to_bytelist(268),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_keyboard_event_array(List) ->
	Size = length(List),
	Pointer = new_keyboard_event_array(Size),
	list_to_keyboard_event_array(List, Pointer, 0).
list_to_keyboard_event_array([], Pointer, _Index) -> Pointer;
list_to_keyboard_event_array([Value|List], Pointer, Index) ->
	pointer_deref_keyboard_event_array_assign(Pointer, Index, Value),
	list_to_keyboard_event_array(List, Pointer, Index+1).

keyboard_event_array_to_list(Pointer, Size) ->
	keyboard_event_array_to_list(Pointer, Size-1, []).
keyboard_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
keyboard_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_keyboard_event_array(Pointer, Size),
	keyboard_event_array_to_list(Pointer, Size-1, [Elem|Result]).

keyboard_event_get_type(Pointer) ->
	Code = int_to_bytelist(269),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(270),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(271),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(272),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(273),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(274),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_state(Pointer) ->
	Code = int_to_bytelist(275),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(276),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_repeat(Pointer) ->
	Code = int_to_bytelist(277),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_repeat(Pointer, Attrib) ->
	Code = int_to_bytelist(278),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(279),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(280),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(281),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(282),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

keyboard_event_get_keysym(Pointer) ->
	Code = int_to_bytelist(283),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keysym(DataList);
		Msg ->
			{error, Msg}
	end.

keyboard_event_set_keysym(Pointer, Attrib) ->
	Code = int_to_bytelist(284),
	PList = pointer_to_bytelist(Pointer),
	AList = keysym_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Length, _} = parse_sint32(R5),
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

text_editing_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[text_editing_event_to_bytelist(E) || E<-List].

bytelist_to_text_editing_event_array(Bytelist, Size) ->
	bytelist_to_text_editing_event_array(Bytelist, Size, []).
bytelist_to_text_editing_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_text_editing_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_text_editing_event(Bytelist),
	bytelist_to_text_editing_event_array(Rest, Size-1, [Elem|Result]).

parse_text_editing_event_array(Bytelist, Size) ->
	parse_text_editing_event_array(Bytelist, Size, []).
parse_text_editing_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_text_editing_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_text_editing_event(Bytelist),
	parse_text_editing_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_text_editing_event(Pointer) ->
	Code = int_to_bytelist(285),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_editing_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_editing_event_array(Pointer, Index) ->
	Code = int_to_bytelist(286),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_editing_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_editing_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(287),
	PList = pointer_to_bytelist(Pointer),
	VList = text_editing_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_editing_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(288),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = text_editing_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_text_editing_event() ->
	Code = int_to_bytelist(289),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_editing_event_auto() ->
	Pointer = new_text_editing_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_text_editing_event, P);
		Error -> Error
	end.

new_text_editing_event_array(Size) ->
	Code = int_to_bytelist(290),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_editing_event_array_auto(Size) ->
	Pointer = new_text_editing_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_text_editing_event, P);
		Error -> Error
	end.

delete_text_editing_event(Pointer) ->
	Code = int_to_bytelist(291),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_text_editing_event_array(List) ->
	Size = length(List),
	Pointer = new_text_editing_event_array(Size),
	list_to_text_editing_event_array(List, Pointer, 0).
list_to_text_editing_event_array([], Pointer, _Index) -> Pointer;
list_to_text_editing_event_array([Value|List], Pointer, Index) ->
	pointer_deref_text_editing_event_array_assign(Pointer, Index, Value),
	list_to_text_editing_event_array(List, Pointer, Index+1).

text_editing_event_array_to_list(Pointer, Size) ->
	text_editing_event_array_to_list(Pointer, Size-1, []).
text_editing_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
text_editing_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_text_editing_event_array(Pointer, Size),
	text_editing_event_array_to_list(Pointer, Size-1, [Elem|Result]).

text_editing_event_get_type(Pointer) ->
	Code = int_to_bytelist(292),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(293),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(294),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(295),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(296),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(297),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_text(Pointer) ->
	Code = int_to_bytelist(298),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(299),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_start(Pointer) ->
	Code = int_to_bytelist(300),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_start(Pointer, Attrib) ->
	Code = int_to_bytelist(301),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_editing_event_get_length(Pointer) ->
	Code = int_to_bytelist(302),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_editing_event_set_length(Pointer, Attrib) ->
	Code = int_to_bytelist(303),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Text, _} = parse_string(R3),
	#text_input_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text}.

parse_text_input_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{WindowID, R3} = parse_uint32(R2),
	{Text, R4} = parse_string(R3),
	{#text_input_event{type=Type, timestamp=Timestamp, windowID=WindowID, text=Text}, R4}.

text_input_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[text_input_event_to_bytelist(E) || E<-List].

bytelist_to_text_input_event_array(Bytelist, Size) ->
	bytelist_to_text_input_event_array(Bytelist, Size, []).
bytelist_to_text_input_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_text_input_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_text_input_event(Bytelist),
	bytelist_to_text_input_event_array(Rest, Size-1, [Elem|Result]).

parse_text_input_event_array(Bytelist, Size) ->
	parse_text_input_event_array(Bytelist, Size, []).
parse_text_input_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_text_input_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_text_input_event(Bytelist),
	parse_text_input_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_text_input_event(Pointer) ->
	Code = int_to_bytelist(304),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_input_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_input_event_array(Pointer, Index) ->
	Code = int_to_bytelist(305),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_input_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_input_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(306),
	PList = pointer_to_bytelist(Pointer),
	VList = text_input_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_text_input_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(307),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = text_input_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_text_input_event() ->
	Code = int_to_bytelist(308),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_input_event_auto() ->
	Pointer = new_text_input_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_text_input_event, P);
		Error -> Error
	end.

new_text_input_event_array(Size) ->
	Code = int_to_bytelist(309),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_text_input_event_array_auto(Size) ->
	Pointer = new_text_input_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_text_input_event, P);
		Error -> Error
	end.

delete_text_input_event(Pointer) ->
	Code = int_to_bytelist(310),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_text_input_event_array(List) ->
	Size = length(List),
	Pointer = new_text_input_event_array(Size),
	list_to_text_input_event_array(List, Pointer, 0).
list_to_text_input_event_array([], Pointer, _Index) -> Pointer;
list_to_text_input_event_array([Value|List], Pointer, Index) ->
	pointer_deref_text_input_event_array_assign(Pointer, Index, Value),
	list_to_text_input_event_array(List, Pointer, Index+1).

text_input_event_array_to_list(Pointer, Size) ->
	text_input_event_array_to_list(Pointer, Size-1, []).
text_input_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
text_input_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_text_input_event_array(Pointer, Size),
	text_input_event_array_to_list(Pointer, Size-1, [Elem|Result]).

text_input_event_get_type(Pointer) ->
	Code = int_to_bytelist(311),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(312),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(313),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(314),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(315),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(316),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

text_input_event_get_text(Pointer) ->
	Code = int_to_bytelist(317),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

text_input_event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(318),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Yrel, _} = parse_sint32(R8),
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

mouse_motion_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[mouse_motion_event_to_bytelist(E) || E<-List].

bytelist_to_mouse_motion_event_array(Bytelist, Size) ->
	bytelist_to_mouse_motion_event_array(Bytelist, Size, []).
bytelist_to_mouse_motion_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_mouse_motion_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_motion_event(Bytelist),
	bytelist_to_mouse_motion_event_array(Rest, Size-1, [Elem|Result]).

parse_mouse_motion_event_array(Bytelist, Size) ->
	parse_mouse_motion_event_array(Bytelist, Size, []).
parse_mouse_motion_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_mouse_motion_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_motion_event(Bytelist),
	parse_mouse_motion_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_mouse_motion_event(Pointer) ->
	Code = int_to_bytelist(319),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_motion_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_motion_event_array(Pointer, Index) ->
	Code = int_to_bytelist(320),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_motion_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_motion_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(321),
	PList = pointer_to_bytelist(Pointer),
	VList = mouse_motion_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_motion_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(322),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = mouse_motion_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_mouse_motion_event() ->
	Code = int_to_bytelist(323),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_motion_event_auto() ->
	Pointer = new_mouse_motion_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_motion_event, P);
		Error -> Error
	end.

new_mouse_motion_event_array(Size) ->
	Code = int_to_bytelist(324),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_motion_event_array_auto(Size) ->
	Pointer = new_mouse_motion_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_motion_event, P);
		Error -> Error
	end.

delete_mouse_motion_event(Pointer) ->
	Code = int_to_bytelist(325),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_mouse_motion_event_array(List) ->
	Size = length(List),
	Pointer = new_mouse_motion_event_array(Size),
	list_to_mouse_motion_event_array(List, Pointer, 0).
list_to_mouse_motion_event_array([], Pointer, _Index) -> Pointer;
list_to_mouse_motion_event_array([Value|List], Pointer, Index) ->
	pointer_deref_mouse_motion_event_array_assign(Pointer, Index, Value),
	list_to_mouse_motion_event_array(List, Pointer, Index+1).

mouse_motion_event_array_to_list(Pointer, Size) ->
	mouse_motion_event_array_to_list(Pointer, Size-1, []).
mouse_motion_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
mouse_motion_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_mouse_motion_event_array(Pointer, Size),
	mouse_motion_event_array_to_list(Pointer, Size-1, [Elem|Result]).

mouse_motion_event_get_type(Pointer) ->
	Code = int_to_bytelist(326),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(327),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(328),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(329),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(330),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(331),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_which(Pointer) ->
	Code = int_to_bytelist(332),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(333),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_state(Pointer) ->
	Code = int_to_bytelist(334),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(335),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_x(Pointer) ->
	Code = int_to_bytelist(336),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(337),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_y(Pointer) ->
	Code = int_to_bytelist(338),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(339),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_xrel(Pointer) ->
	Code = int_to_bytelist(340),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_xrel(Pointer, Attrib) ->
	Code = int_to_bytelist(341),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_get_yrel(Pointer) ->
	Code = int_to_bytelist(342),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_motion_event_set_yrel(Pointer, Attrib) ->
	Code = int_to_bytelist(343),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Y, _} = parse_sint32(R8),
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

mouse_button_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[mouse_button_event_to_bytelist(E) || E<-List].

bytelist_to_mouse_button_event_array(Bytelist, Size) ->
	bytelist_to_mouse_button_event_array(Bytelist, Size, []).
bytelist_to_mouse_button_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_mouse_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_button_event(Bytelist),
	bytelist_to_mouse_button_event_array(Rest, Size-1, [Elem|Result]).

parse_mouse_button_event_array(Bytelist, Size) ->
	parse_mouse_button_event_array(Bytelist, Size, []).
parse_mouse_button_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_mouse_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_button_event(Bytelist),
	parse_mouse_button_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_mouse_button_event(Pointer) ->
	Code = int_to_bytelist(344),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_button_event_array(Pointer, Index) ->
	Code = int_to_bytelist(345),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_button_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(346),
	PList = pointer_to_bytelist(Pointer),
	VList = mouse_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_button_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(347),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = mouse_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_mouse_button_event() ->
	Code = int_to_bytelist(348),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_button_event_auto() ->
	Pointer = new_mouse_button_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_button_event, P);
		Error -> Error
	end.

new_mouse_button_event_array(Size) ->
	Code = int_to_bytelist(349),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_button_event_array_auto(Size) ->
	Pointer = new_mouse_button_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_button_event, P);
		Error -> Error
	end.

delete_mouse_button_event(Pointer) ->
	Code = int_to_bytelist(350),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_mouse_button_event_array(List) ->
	Size = length(List),
	Pointer = new_mouse_button_event_array(Size),
	list_to_mouse_button_event_array(List, Pointer, 0).
list_to_mouse_button_event_array([], Pointer, _Index) -> Pointer;
list_to_mouse_button_event_array([Value|List], Pointer, Index) ->
	pointer_deref_mouse_button_event_array_assign(Pointer, Index, Value),
	list_to_mouse_button_event_array(List, Pointer, Index+1).

mouse_button_event_array_to_list(Pointer, Size) ->
	mouse_button_event_array_to_list(Pointer, Size-1, []).
mouse_button_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
mouse_button_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_mouse_button_event_array(Pointer, Size),
	mouse_button_event_array_to_list(Pointer, Size-1, [Elem|Result]).

mouse_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(351),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(352),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(353),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(354),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(355),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(356),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(357),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(358),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(359),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(360),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(361),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(362),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_clicks(Pointer) ->
	Code = int_to_bytelist(363),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_clicks(Pointer, Attrib) ->
	Code = int_to_bytelist(364),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_x(Pointer) ->
	Code = int_to_bytelist(365),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(366),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_button_event_get_y(Pointer) ->
	Code = int_to_bytelist(367),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_button_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(368),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Direction, _} = parse_uint32(R6),
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

mouse_wheel_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[mouse_wheel_event_to_bytelist(E) || E<-List].

bytelist_to_mouse_wheel_event_array(Bytelist, Size) ->
	bytelist_to_mouse_wheel_event_array(Bytelist, Size, []).
bytelist_to_mouse_wheel_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_mouse_wheel_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_wheel_event(Bytelist),
	bytelist_to_mouse_wheel_event_array(Rest, Size-1, [Elem|Result]).

parse_mouse_wheel_event_array(Bytelist, Size) ->
	parse_mouse_wheel_event_array(Bytelist, Size, []).
parse_mouse_wheel_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_mouse_wheel_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_mouse_wheel_event(Bytelist),
	parse_mouse_wheel_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_mouse_wheel_event(Pointer) ->
	Code = int_to_bytelist(369),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_wheel_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_wheel_event_array(Pointer, Index) ->
	Code = int_to_bytelist(370),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_wheel_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_wheel_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(371),
	PList = pointer_to_bytelist(Pointer),
	VList = mouse_wheel_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_mouse_wheel_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(372),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = mouse_wheel_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_mouse_wheel_event() ->
	Code = int_to_bytelist(373),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_wheel_event_auto() ->
	Pointer = new_mouse_wheel_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_wheel_event, P);
		Error -> Error
	end.

new_mouse_wheel_event_array(Size) ->
	Code = int_to_bytelist(374),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_mouse_wheel_event_array_auto(Size) ->
	Pointer = new_mouse_wheel_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_mouse_wheel_event, P);
		Error -> Error
	end.

delete_mouse_wheel_event(Pointer) ->
	Code = int_to_bytelist(375),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_mouse_wheel_event_array(List) ->
	Size = length(List),
	Pointer = new_mouse_wheel_event_array(Size),
	list_to_mouse_wheel_event_array(List, Pointer, 0).
list_to_mouse_wheel_event_array([], Pointer, _Index) -> Pointer;
list_to_mouse_wheel_event_array([Value|List], Pointer, Index) ->
	pointer_deref_mouse_wheel_event_array_assign(Pointer, Index, Value),
	list_to_mouse_wheel_event_array(List, Pointer, Index+1).

mouse_wheel_event_array_to_list(Pointer, Size) ->
	mouse_wheel_event_array_to_list(Pointer, Size-1, []).
mouse_wheel_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
mouse_wheel_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_mouse_wheel_event_array(Pointer, Size),
	mouse_wheel_event_array_to_list(Pointer, Size-1, [Elem|Result]).

mouse_wheel_event_get_type(Pointer) ->
	Code = int_to_bytelist(376),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(377),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(378),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(379),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(380),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(381),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_which(Pointer) ->
	Code = int_to_bytelist(382),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(383),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_x(Pointer) ->
	Code = int_to_bytelist(384),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(385),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_y(Pointer) ->
	Code = int_to_bytelist(386),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(387),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_get_direction(Pointer) ->
	Code = int_to_bytelist(388),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

mouse_wheel_event_set_direction(Pointer, Attrib) ->
	Code = int_to_bytelist(389),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Value, _} = parse_sint16(R7),
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

joy_axis_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[joy_axis_event_to_bytelist(E) || E<-List].

bytelist_to_joy_axis_event_array(Bytelist, Size) ->
	bytelist_to_joy_axis_event_array(Bytelist, Size, []).
bytelist_to_joy_axis_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_joy_axis_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_axis_event(Bytelist),
	bytelist_to_joy_axis_event_array(Rest, Size-1, [Elem|Result]).

parse_joy_axis_event_array(Bytelist, Size) ->
	parse_joy_axis_event_array(Bytelist, Size, []).
parse_joy_axis_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_joy_axis_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_axis_event(Bytelist),
	parse_joy_axis_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_joy_axis_event(Pointer) ->
	Code = int_to_bytelist(390),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_axis_event_array(Pointer, Index) ->
	Code = int_to_bytelist(391),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_axis_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(392),
	PList = pointer_to_bytelist(Pointer),
	VList = joy_axis_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_axis_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(393),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = joy_axis_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_joy_axis_event() ->
	Code = int_to_bytelist(394),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_axis_event_auto() ->
	Pointer = new_joy_axis_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_axis_event, P);
		Error -> Error
	end.

new_joy_axis_event_array(Size) ->
	Code = int_to_bytelist(395),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_axis_event_array_auto(Size) ->
	Pointer = new_joy_axis_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_axis_event, P);
		Error -> Error
	end.

delete_joy_axis_event(Pointer) ->
	Code = int_to_bytelist(396),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_joy_axis_event_array(List) ->
	Size = length(List),
	Pointer = new_joy_axis_event_array(Size),
	list_to_joy_axis_event_array(List, Pointer, 0).
list_to_joy_axis_event_array([], Pointer, _Index) -> Pointer;
list_to_joy_axis_event_array([Value|List], Pointer, Index) ->
	pointer_deref_joy_axis_event_array_assign(Pointer, Index, Value),
	list_to_joy_axis_event_array(List, Pointer, Index+1).

joy_axis_event_array_to_list(Pointer, Size) ->
	joy_axis_event_array_to_list(Pointer, Size-1, []).
joy_axis_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
joy_axis_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_joy_axis_event_array(Pointer, Size),
	joy_axis_event_array_to_list(Pointer, Size-1, [Elem|Result]).

joy_axis_event_get_type(Pointer) ->
	Code = int_to_bytelist(397),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(398),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(399),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(400),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_which(Pointer) ->
	Code = int_to_bytelist(401),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(402),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_axis(Pointer) ->
	Code = int_to_bytelist(403),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_axis(Pointer, Attrib) ->
	Code = int_to_bytelist(404),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(405),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(406),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(407),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(408),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(409),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(410),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_axis_event_get_value(Pointer) ->
	Code = int_to_bytelist(411),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_axis_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(412),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Yrel, _} = parse_sint16(R8),
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

joy_ball_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[joy_ball_event_to_bytelist(E) || E<-List].

bytelist_to_joy_ball_event_array(Bytelist, Size) ->
	bytelist_to_joy_ball_event_array(Bytelist, Size, []).
bytelist_to_joy_ball_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_joy_ball_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_ball_event(Bytelist),
	bytelist_to_joy_ball_event_array(Rest, Size-1, [Elem|Result]).

parse_joy_ball_event_array(Bytelist, Size) ->
	parse_joy_ball_event_array(Bytelist, Size, []).
parse_joy_ball_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_joy_ball_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_ball_event(Bytelist),
	parse_joy_ball_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_joy_ball_event(Pointer) ->
	Code = int_to_bytelist(413),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_ball_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_ball_event_array(Pointer, Index) ->
	Code = int_to_bytelist(414),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_ball_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_ball_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(415),
	PList = pointer_to_bytelist(Pointer),
	VList = joy_ball_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_ball_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(416),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = joy_ball_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_joy_ball_event() ->
	Code = int_to_bytelist(417),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_ball_event_auto() ->
	Pointer = new_joy_ball_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_ball_event, P);
		Error -> Error
	end.

new_joy_ball_event_array(Size) ->
	Code = int_to_bytelist(418),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_ball_event_array_auto(Size) ->
	Pointer = new_joy_ball_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_ball_event, P);
		Error -> Error
	end.

delete_joy_ball_event(Pointer) ->
	Code = int_to_bytelist(419),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_joy_ball_event_array(List) ->
	Size = length(List),
	Pointer = new_joy_ball_event_array(Size),
	list_to_joy_ball_event_array(List, Pointer, 0).
list_to_joy_ball_event_array([], Pointer, _Index) -> Pointer;
list_to_joy_ball_event_array([Value|List], Pointer, Index) ->
	pointer_deref_joy_ball_event_array_assign(Pointer, Index, Value),
	list_to_joy_ball_event_array(List, Pointer, Index+1).

joy_ball_event_array_to_list(Pointer, Size) ->
	joy_ball_event_array_to_list(Pointer, Size-1, []).
joy_ball_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
joy_ball_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_joy_ball_event_array(Pointer, Size),
	joy_ball_event_array_to_list(Pointer, Size-1, [Elem|Result]).

joy_ball_event_get_type(Pointer) ->
	Code = int_to_bytelist(420),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(421),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(422),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(423),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_which(Pointer) ->
	Code = int_to_bytelist(424),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(425),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_ball(Pointer) ->
	Code = int_to_bytelist(426),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_ball(Pointer, Attrib) ->
	Code = int_to_bytelist(427),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(428),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(429),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(430),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(431),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(432),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(433),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_xrel(Pointer) ->
	Code = int_to_bytelist(434),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_xrel(Pointer, Attrib) ->
	Code = int_to_bytelist(435),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_ball_event_get_yrel(Pointer) ->
	Code = int_to_bytelist(436),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

joy_ball_event_set_yrel(Pointer, Attrib) ->
	Code = int_to_bytelist(437),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding2, _} = parse_uint8(R6),
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

joy_hat_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[joy_hat_event_to_bytelist(E) || E<-List].

bytelist_to_joy_hat_event_array(Bytelist, Size) ->
	bytelist_to_joy_hat_event_array(Bytelist, Size, []).
bytelist_to_joy_hat_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_joy_hat_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_hat_event(Bytelist),
	bytelist_to_joy_hat_event_array(Rest, Size-1, [Elem|Result]).

parse_joy_hat_event_array(Bytelist, Size) ->
	parse_joy_hat_event_array(Bytelist, Size, []).
parse_joy_hat_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_joy_hat_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_hat_event(Bytelist),
	parse_joy_hat_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_joy_hat_event(Pointer) ->
	Code = int_to_bytelist(438),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_hat_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_hat_event_array(Pointer, Index) ->
	Code = int_to_bytelist(439),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_hat_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_hat_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(440),
	PList = pointer_to_bytelist(Pointer),
	VList = joy_hat_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_hat_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(441),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = joy_hat_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_joy_hat_event() ->
	Code = int_to_bytelist(442),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_hat_event_auto() ->
	Pointer = new_joy_hat_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_hat_event, P);
		Error -> Error
	end.

new_joy_hat_event_array(Size) ->
	Code = int_to_bytelist(443),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_hat_event_array_auto(Size) ->
	Pointer = new_joy_hat_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_hat_event, P);
		Error -> Error
	end.

delete_joy_hat_event(Pointer) ->
	Code = int_to_bytelist(444),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_joy_hat_event_array(List) ->
	Size = length(List),
	Pointer = new_joy_hat_event_array(Size),
	list_to_joy_hat_event_array(List, Pointer, 0).
list_to_joy_hat_event_array([], Pointer, _Index) -> Pointer;
list_to_joy_hat_event_array([Value|List], Pointer, Index) ->
	pointer_deref_joy_hat_event_array_assign(Pointer, Index, Value),
	list_to_joy_hat_event_array(List, Pointer, Index+1).

joy_hat_event_array_to_list(Pointer, Size) ->
	joy_hat_event_array_to_list(Pointer, Size-1, []).
joy_hat_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
joy_hat_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_joy_hat_event_array(Pointer, Size),
	joy_hat_event_array_to_list(Pointer, Size-1, [Elem|Result]).

joy_hat_event_get_type(Pointer) ->
	Code = int_to_bytelist(445),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(446),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(447),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(448),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_which(Pointer) ->
	Code = int_to_bytelist(449),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(450),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_hat(Pointer) ->
	Code = int_to_bytelist(451),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_hat(Pointer, Attrib) ->
	Code = int_to_bytelist(452),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_value(Pointer) ->
	Code = int_to_bytelist(453),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(454),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(455),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(456),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_hat_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(457),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_hat_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(458),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding2, _} = parse_uint8(R6),
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

joy_button_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[joy_button_event_to_bytelist(E) || E<-List].

bytelist_to_joy_button_event_array(Bytelist, Size) ->
	bytelist_to_joy_button_event_array(Bytelist, Size, []).
bytelist_to_joy_button_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_joy_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_button_event(Bytelist),
	bytelist_to_joy_button_event_array(Rest, Size-1, [Elem|Result]).

parse_joy_button_event_array(Bytelist, Size) ->
	parse_joy_button_event_array(Bytelist, Size, []).
parse_joy_button_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_joy_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_button_event(Bytelist),
	parse_joy_button_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_joy_button_event(Pointer) ->
	Code = int_to_bytelist(459),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_button_event_array(Pointer, Index) ->
	Code = int_to_bytelist(460),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_button_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(461),
	PList = pointer_to_bytelist(Pointer),
	VList = joy_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_button_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(462),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = joy_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_joy_button_event() ->
	Code = int_to_bytelist(463),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_button_event_auto() ->
	Pointer = new_joy_button_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_button_event, P);
		Error -> Error
	end.

new_joy_button_event_array(Size) ->
	Code = int_to_bytelist(464),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_button_event_array_auto(Size) ->
	Pointer = new_joy_button_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_button_event, P);
		Error -> Error
	end.

delete_joy_button_event(Pointer) ->
	Code = int_to_bytelist(465),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_joy_button_event_array(List) ->
	Size = length(List),
	Pointer = new_joy_button_event_array(Size),
	list_to_joy_button_event_array(List, Pointer, 0).
list_to_joy_button_event_array([], Pointer, _Index) -> Pointer;
list_to_joy_button_event_array([Value|List], Pointer, Index) ->
	pointer_deref_joy_button_event_array_assign(Pointer, Index, Value),
	list_to_joy_button_event_array(List, Pointer, Index+1).

joy_button_event_array_to_list(Pointer, Size) ->
	joy_button_event_array_to_list(Pointer, Size-1, []).
joy_button_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
joy_button_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_joy_button_event_array(Pointer, Size),
	joy_button_event_array_to_list(Pointer, Size-1, [Elem|Result]).

joy_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(466),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(467),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(468),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(469),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(470),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(471),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(472),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(473),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(474),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(475),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(476),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(477),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_button_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(478),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

joy_button_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(479),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Which, _} = parse_sint32(R2),
	#joy_device_event{type=Type, timestamp=Timestamp, which=Which}.

parse_joy_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	{#joy_device_event{type=Type, timestamp=Timestamp, which=Which}, R3}.

joy_device_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[joy_device_event_to_bytelist(E) || E<-List].

bytelist_to_joy_device_event_array(Bytelist, Size) ->
	bytelist_to_joy_device_event_array(Bytelist, Size, []).
bytelist_to_joy_device_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_joy_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_device_event(Bytelist),
	bytelist_to_joy_device_event_array(Rest, Size-1, [Elem|Result]).

parse_joy_device_event_array(Bytelist, Size) ->
	parse_joy_device_event_array(Bytelist, Size, []).
parse_joy_device_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_joy_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_joy_device_event(Bytelist),
	parse_joy_device_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_joy_device_event(Pointer) ->
	Code = int_to_bytelist(480),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_device_event_array(Pointer, Index) ->
	Code = int_to_bytelist(481),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_device_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(482),
	PList = pointer_to_bytelist(Pointer),
	VList = joy_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_joy_device_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(483),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = joy_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_joy_device_event() ->
	Code = int_to_bytelist(484),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_device_event_auto() ->
	Pointer = new_joy_device_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_device_event, P);
		Error -> Error
	end.

new_joy_device_event_array(Size) ->
	Code = int_to_bytelist(485),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_joy_device_event_array_auto(Size) ->
	Pointer = new_joy_device_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_joy_device_event, P);
		Error -> Error
	end.

delete_joy_device_event(Pointer) ->
	Code = int_to_bytelist(486),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_joy_device_event_array(List) ->
	Size = length(List),
	Pointer = new_joy_device_event_array(Size),
	list_to_joy_device_event_array(List, Pointer, 0).
list_to_joy_device_event_array([], Pointer, _Index) -> Pointer;
list_to_joy_device_event_array([Value|List], Pointer, Index) ->
	pointer_deref_joy_device_event_array_assign(Pointer, Index, Value),
	list_to_joy_device_event_array(List, Pointer, Index+1).

joy_device_event_array_to_list(Pointer, Size) ->
	joy_device_event_array_to_list(Pointer, Size-1, []).
joy_device_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
joy_device_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_joy_device_event_array(Pointer, Size),
	joy_device_event_array_to_list(Pointer, Size-1, [Elem|Result]).

joy_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(487),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(488),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(489),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(490),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

joy_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(491),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

joy_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(492),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding4, _} = parse_uint16(R8),
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

controller_axis_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[controller_axis_event_to_bytelist(E) || E<-List].

bytelist_to_controller_axis_event_array(Bytelist, Size) ->
	bytelist_to_controller_axis_event_array(Bytelist, Size, []).
bytelist_to_controller_axis_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_controller_axis_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_axis_event(Bytelist),
	bytelist_to_controller_axis_event_array(Rest, Size-1, [Elem|Result]).

parse_controller_axis_event_array(Bytelist, Size) ->
	parse_controller_axis_event_array(Bytelist, Size, []).
parse_controller_axis_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_controller_axis_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_axis_event(Bytelist),
	parse_controller_axis_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_controller_axis_event(Pointer) ->
	Code = int_to_bytelist(493),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_axis_event_array(Pointer, Index) ->
	Code = int_to_bytelist(494),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_axis_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(495),
	PList = pointer_to_bytelist(Pointer),
	VList = controller_axis_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_axis_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(496),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = controller_axis_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_controller_axis_event() ->
	Code = int_to_bytelist(497),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_axis_event_auto() ->
	Pointer = new_controller_axis_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_axis_event, P);
		Error -> Error
	end.

new_controller_axis_event_array(Size) ->
	Code = int_to_bytelist(498),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_axis_event_array_auto(Size) ->
	Pointer = new_controller_axis_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_axis_event, P);
		Error -> Error
	end.

delete_controller_axis_event(Pointer) ->
	Code = int_to_bytelist(499),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_controller_axis_event_array(List) ->
	Size = length(List),
	Pointer = new_controller_axis_event_array(Size),
	list_to_controller_axis_event_array(List, Pointer, 0).
list_to_controller_axis_event_array([], Pointer, _Index) -> Pointer;
list_to_controller_axis_event_array([Value|List], Pointer, Index) ->
	pointer_deref_controller_axis_event_array_assign(Pointer, Index, Value),
	list_to_controller_axis_event_array(List, Pointer, Index+1).

controller_axis_event_array_to_list(Pointer, Size) ->
	controller_axis_event_array_to_list(Pointer, Size-1, []).
controller_axis_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
controller_axis_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_controller_axis_event_array(Pointer, Size),
	controller_axis_event_array_to_list(Pointer, Size-1, [Elem|Result]).

controller_axis_event_get_type(Pointer) ->
	Code = int_to_bytelist(500),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(501),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(502),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(503),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_which(Pointer) ->
	Code = int_to_bytelist(504),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(505),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_axis(Pointer) ->
	Code = int_to_bytelist(506),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_axis(Pointer, Attrib) ->
	Code = int_to_bytelist(507),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(508),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(509),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(510),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(511),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(512),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(513),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_value(Pointer) ->
	Code = int_to_bytelist(514),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint16(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_value(Pointer, Attrib) ->
	Code = int_to_bytelist(515),
	PList = pointer_to_bytelist(Pointer),
	AList = sint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_axis_event_get_padding4(Pointer) ->
	Code = int_to_bytelist(516),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

controller_axis_event_set_padding4(Pointer, Attrib) ->
	Code = int_to_bytelist(517),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding2, _} = parse_uint8(R6),
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

controller_button_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[controller_button_event_to_bytelist(E) || E<-List].

bytelist_to_controller_button_event_array(Bytelist, Size) ->
	bytelist_to_controller_button_event_array(Bytelist, Size, []).
bytelist_to_controller_button_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_controller_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_button_event(Bytelist),
	bytelist_to_controller_button_event_array(Rest, Size-1, [Elem|Result]).

parse_controller_button_event_array(Bytelist, Size) ->
	parse_controller_button_event_array(Bytelist, Size, []).
parse_controller_button_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_controller_button_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_button_event(Bytelist),
	parse_controller_button_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_controller_button_event(Pointer) ->
	Code = int_to_bytelist(518),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_button_event_array(Pointer, Index) ->
	Code = int_to_bytelist(519),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_button_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(520),
	PList = pointer_to_bytelist(Pointer),
	VList = controller_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_button_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(521),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = controller_button_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_controller_button_event() ->
	Code = int_to_bytelist(522),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_button_event_auto() ->
	Pointer = new_controller_button_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_button_event, P);
		Error -> Error
	end.

new_controller_button_event_array(Size) ->
	Code = int_to_bytelist(523),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_button_event_array_auto(Size) ->
	Pointer = new_controller_button_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_button_event, P);
		Error -> Error
	end.

delete_controller_button_event(Pointer) ->
	Code = int_to_bytelist(524),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_controller_button_event_array(List) ->
	Size = length(List),
	Pointer = new_controller_button_event_array(Size),
	list_to_controller_button_event_array(List, Pointer, 0).
list_to_controller_button_event_array([], Pointer, _Index) -> Pointer;
list_to_controller_button_event_array([Value|List], Pointer, Index) ->
	pointer_deref_controller_button_event_array_assign(Pointer, Index, Value),
	list_to_controller_button_event_array(List, Pointer, Index+1).

controller_button_event_array_to_list(Pointer, Size) ->
	controller_button_event_array_to_list(Pointer, Size-1, []).
controller_button_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
controller_button_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_controller_button_event_array(Pointer, Size),
	controller_button_event_array_to_list(Pointer, Size-1, [Elem|Result]).

controller_button_event_get_type(Pointer) ->
	Code = int_to_bytelist(525),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(526),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(527),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(528),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_which(Pointer) ->
	Code = int_to_bytelist(529),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joystick_id(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(530),
	PList = pointer_to_bytelist(Pointer),
	AList = joystick_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_button(Pointer) ->
	Code = int_to_bytelist(531),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(532),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_state(Pointer) ->
	Code = int_to_bytelist(533),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_state(Pointer, Attrib) ->
	Code = int_to_bytelist(534),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(535),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(536),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_button_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(537),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

controller_button_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(538),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Which, _} = parse_sint32(R2),
	#controller_device_event{type=Type, timestamp=Timestamp, which=Which}.

parse_controller_device_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Which, R3} = parse_sint32(R2),
	{#controller_device_event{type=Type, timestamp=Timestamp, which=Which}, R3}.

controller_device_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[controller_device_event_to_bytelist(E) || E<-List].

bytelist_to_controller_device_event_array(Bytelist, Size) ->
	bytelist_to_controller_device_event_array(Bytelist, Size, []).
bytelist_to_controller_device_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_controller_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_device_event(Bytelist),
	bytelist_to_controller_device_event_array(Rest, Size-1, [Elem|Result]).

parse_controller_device_event_array(Bytelist, Size) ->
	parse_controller_device_event_array(Bytelist, Size, []).
parse_controller_device_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_controller_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_controller_device_event(Bytelist),
	parse_controller_device_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_controller_device_event(Pointer) ->
	Code = int_to_bytelist(539),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_device_event_array(Pointer, Index) ->
	Code = int_to_bytelist(540),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_device_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(541),
	PList = pointer_to_bytelist(Pointer),
	VList = controller_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_controller_device_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(542),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = controller_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_controller_device_event() ->
	Code = int_to_bytelist(543),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_device_event_auto() ->
	Pointer = new_controller_device_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_device_event, P);
		Error -> Error
	end.

new_controller_device_event_array(Size) ->
	Code = int_to_bytelist(544),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_controller_device_event_array_auto(Size) ->
	Pointer = new_controller_device_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_controller_device_event, P);
		Error -> Error
	end.

delete_controller_device_event(Pointer) ->
	Code = int_to_bytelist(545),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_controller_device_event_array(List) ->
	Size = length(List),
	Pointer = new_controller_device_event_array(Size),
	list_to_controller_device_event_array(List, Pointer, 0).
list_to_controller_device_event_array([], Pointer, _Index) -> Pointer;
list_to_controller_device_event_array([Value|List], Pointer, Index) ->
	pointer_deref_controller_device_event_array_assign(Pointer, Index, Value),
	list_to_controller_device_event_array(List, Pointer, Index+1).

controller_device_event_array_to_list(Pointer, Size) ->
	controller_device_event_array_to_list(Pointer, Size-1, []).
controller_device_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
controller_device_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_controller_device_event_array(Pointer, Size),
	controller_device_event_array_to_list(Pointer, Size-1, [Elem|Result]).

controller_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(546),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(547),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(548),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(549),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

controller_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(550),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

controller_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(551),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding3, _} = parse_uint8(R6),
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

audio_device_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[audio_device_event_to_bytelist(E) || E<-List].

bytelist_to_audio_device_event_array(Bytelist, Size) ->
	bytelist_to_audio_device_event_array(Bytelist, Size, []).
bytelist_to_audio_device_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_audio_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_audio_device_event(Bytelist),
	bytelist_to_audio_device_event_array(Rest, Size-1, [Elem|Result]).

parse_audio_device_event_array(Bytelist, Size) ->
	parse_audio_device_event_array(Bytelist, Size, []).
parse_audio_device_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_audio_device_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_audio_device_event(Bytelist),
	parse_audio_device_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_audio_device_event(Pointer) ->
	Code = int_to_bytelist(552),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_audio_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_audio_device_event_array(Pointer, Index) ->
	Code = int_to_bytelist(553),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_audio_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_audio_device_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(554),
	PList = pointer_to_bytelist(Pointer),
	VList = audio_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_audio_device_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(555),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = audio_device_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_audio_device_event() ->
	Code = int_to_bytelist(556),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_audio_device_event_auto() ->
	Pointer = new_audio_device_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_audio_device_event, P);
		Error -> Error
	end.

new_audio_device_event_array(Size) ->
	Code = int_to_bytelist(557),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_audio_device_event_array_auto(Size) ->
	Pointer = new_audio_device_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_audio_device_event, P);
		Error -> Error
	end.

delete_audio_device_event(Pointer) ->
	Code = int_to_bytelist(558),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_audio_device_event_array(List) ->
	Size = length(List),
	Pointer = new_audio_device_event_array(Size),
	list_to_audio_device_event_array(List, Pointer, 0).
list_to_audio_device_event_array([], Pointer, _Index) -> Pointer;
list_to_audio_device_event_array([Value|List], Pointer, Index) ->
	pointer_deref_audio_device_event_array_assign(Pointer, Index, Value),
	list_to_audio_device_event_array(List, Pointer, Index+1).

audio_device_event_array_to_list(Pointer, Size) ->
	audio_device_event_array_to_list(Pointer, Size-1, []).
audio_device_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
audio_device_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_audio_device_event_array(Pointer, Size),
	audio_device_event_array_to_list(Pointer, Size-1, [Elem|Result]).

audio_device_event_get_type(Pointer) ->
	Code = int_to_bytelist(559),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(560),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(561),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(562),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_which(Pointer) ->
	Code = int_to_bytelist(563),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_which(Pointer, Attrib) ->
	Code = int_to_bytelist(564),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_iscapture(Pointer) ->
	Code = int_to_bytelist(565),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_iscapture(Pointer, Attrib) ->
	Code = int_to_bytelist(566),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding1(Pointer) ->
	Code = int_to_bytelist(567),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding1(Pointer, Attrib) ->
	Code = int_to_bytelist(568),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding2(Pointer) ->
	Code = int_to_bytelist(569),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding2(Pointer, Attrib) ->
	Code = int_to_bytelist(570),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

audio_device_event_get_padding3(Pointer) ->
	Code = int_to_bytelist(571),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint8(DataList);
		Msg ->
			{error, Msg}
	end.

audio_device_event_set_padding3(Pointer, Attrib) ->
	Code = int_to_bytelist(572),
	PList = pointer_to_bytelist(Pointer),
	AList = uint8_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Timestamp, _} = parse_uint32(R1),
	#quit_event{type=Type, timestamp=Timestamp}.

parse_quit_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{#quit_event{type=Type, timestamp=Timestamp}, R2}.

quit_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[quit_event_to_bytelist(E) || E<-List].

bytelist_to_quit_event_array(Bytelist, Size) ->
	bytelist_to_quit_event_array(Bytelist, Size, []).
bytelist_to_quit_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_quit_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_quit_event(Bytelist),
	bytelist_to_quit_event_array(Rest, Size-1, [Elem|Result]).

parse_quit_event_array(Bytelist, Size) ->
	parse_quit_event_array(Bytelist, Size, []).
parse_quit_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_quit_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_quit_event(Bytelist),
	parse_quit_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_quit_event(Pointer) ->
	Code = int_to_bytelist(573),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_quit_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_quit_event_array(Pointer, Index) ->
	Code = int_to_bytelist(574),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_quit_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_quit_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(575),
	PList = pointer_to_bytelist(Pointer),
	VList = quit_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_quit_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(576),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = quit_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_quit_event() ->
	Code = int_to_bytelist(577),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_quit_event_auto() ->
	Pointer = new_quit_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_quit_event, P);
		Error -> Error
	end.

new_quit_event_array(Size) ->
	Code = int_to_bytelist(578),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_quit_event_array_auto(Size) ->
	Pointer = new_quit_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_quit_event, P);
		Error -> Error
	end.

delete_quit_event(Pointer) ->
	Code = int_to_bytelist(579),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_quit_event_array(List) ->
	Size = length(List),
	Pointer = new_quit_event_array(Size),
	list_to_quit_event_array(List, Pointer, 0).
list_to_quit_event_array([], Pointer, _Index) -> Pointer;
list_to_quit_event_array([Value|List], Pointer, Index) ->
	pointer_deref_quit_event_array_assign(Pointer, Index, Value),
	list_to_quit_event_array(List, Pointer, Index+1).

quit_event_array_to_list(Pointer, Size) ->
	quit_event_array_to_list(Pointer, Size-1, []).
quit_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
quit_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_quit_event_array(Pointer, Size),
	quit_event_array_to_list(Pointer, Size-1, [Elem|Result]).

quit_event_get_type(Pointer) ->
	Code = int_to_bytelist(580),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

quit_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(581),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

quit_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(582),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

quit_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(583),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Data2, _} = parse_pointer(R5),
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

user_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[user_event_to_bytelist(E) || E<-List].

bytelist_to_user_event_array(Bytelist, Size) ->
	bytelist_to_user_event_array(Bytelist, Size, []).
bytelist_to_user_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_user_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_user_event(Bytelist),
	bytelist_to_user_event_array(Rest, Size-1, [Elem|Result]).

parse_user_event_array(Bytelist, Size) ->
	parse_user_event_array(Bytelist, Size, []).
parse_user_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_user_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_user_event(Bytelist),
	parse_user_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_user_event(Pointer) ->
	Code = int_to_bytelist(584),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_user_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_user_event_array(Pointer, Index) ->
	Code = int_to_bytelist(585),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_user_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_user_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(586),
	PList = pointer_to_bytelist(Pointer),
	VList = user_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_user_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(587),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = user_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_user_event() ->
	Code = int_to_bytelist(588),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_user_event_auto() ->
	Pointer = new_user_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_user_event, P);
		Error -> Error
	end.

new_user_event_array(Size) ->
	Code = int_to_bytelist(589),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_user_event_array_auto(Size) ->
	Pointer = new_user_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_user_event, P);
		Error -> Error
	end.

delete_user_event(Pointer) ->
	Code = int_to_bytelist(590),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_user_event_array(List) ->
	Size = length(List),
	Pointer = new_user_event_array(Size),
	list_to_user_event_array(List, Pointer, 0).
list_to_user_event_array([], Pointer, _Index) -> Pointer;
list_to_user_event_array([Value|List], Pointer, Index) ->
	pointer_deref_user_event_array_assign(Pointer, Index, Value),
	list_to_user_event_array(List, Pointer, Index+1).

user_event_array_to_list(Pointer, Size) ->
	user_event_array_to_list(Pointer, Size-1, []).
user_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
user_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_user_event_array(Pointer, Size),
	user_event_array_to_list(Pointer, Size-1, [Elem|Result]).

user_event_get_type(Pointer) ->
	Code = int_to_bytelist(591),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(592),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(593),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(594),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(595),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(596),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_code(Pointer) ->
	Code = int_to_bytelist(597),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_sint32(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_code(Pointer, Attrib) ->
	Code = int_to_bytelist(598),
	PList = pointer_to_bytelist(Pointer),
	AList = sint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_data1(Pointer) ->
	Code = int_to_bytelist(599),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_data1(Pointer, Attrib) ->
	Code = int_to_bytelist(600),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

user_event_get_data2(Pointer) ->
	Code = int_to_bytelist(601),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

user_event_set_data2(Pointer, Attrib) ->
	Code = int_to_bytelist(602),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Msg, _} = parse_pointer(R2),
	#syswm_event{type=Type, timestamp=Timestamp, msg=Msg}.

parse_syswm_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{Msg, R3} = parse_pointer(R2),
	{#syswm_event{type=Type, timestamp=Timestamp, msg=Msg}, R3}.

syswm_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[syswm_event_to_bytelist(E) || E<-List].

bytelist_to_syswm_event_array(Bytelist, Size) ->
	bytelist_to_syswm_event_array(Bytelist, Size, []).
bytelist_to_syswm_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_syswm_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_syswm_event(Bytelist),
	bytelist_to_syswm_event_array(Rest, Size-1, [Elem|Result]).

parse_syswm_event_array(Bytelist, Size) ->
	parse_syswm_event_array(Bytelist, Size, []).
parse_syswm_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_syswm_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_syswm_event(Bytelist),
	parse_syswm_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_syswm_event(Pointer) ->
	Code = int_to_bytelist(603),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_syswm_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_syswm_event_array(Pointer, Index) ->
	Code = int_to_bytelist(604),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_syswm_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_syswm_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(605),
	PList = pointer_to_bytelist(Pointer),
	VList = syswm_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_syswm_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(606),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = syswm_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_syswm_event() ->
	Code = int_to_bytelist(607),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_syswm_event_auto() ->
	Pointer = new_syswm_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_syswm_event, P);
		Error -> Error
	end.

new_syswm_event_array(Size) ->
	Code = int_to_bytelist(608),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_syswm_event_array_auto(Size) ->
	Pointer = new_syswm_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_syswm_event, P);
		Error -> Error
	end.

delete_syswm_event(Pointer) ->
	Code = int_to_bytelist(609),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_syswm_event_array(List) ->
	Size = length(List),
	Pointer = new_syswm_event_array(Size),
	list_to_syswm_event_array(List, Pointer, 0).
list_to_syswm_event_array([], Pointer, _Index) -> Pointer;
list_to_syswm_event_array([Value|List], Pointer, Index) ->
	pointer_deref_syswm_event_array_assign(Pointer, Index, Value),
	list_to_syswm_event_array(List, Pointer, Index+1).

syswm_event_array_to_list(Pointer, Size) ->
	syswm_event_array_to_list(Pointer, Size-1, []).
syswm_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
syswm_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_syswm_event_array(Pointer, Size),
	syswm_event_array_to_list(Pointer, Size-1, [Elem|Result]).

syswm_event_get_type(Pointer) ->
	Code = int_to_bytelist(610),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(611),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(612),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(613),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

syswm_event_get_msg(Pointer) ->
	Code = int_to_bytelist(614),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

syswm_event_set_msg(Pointer, Attrib) ->
	Code = int_to_bytelist(615),
	PList = pointer_to_bytelist(Pointer),
	AList = pointer_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Pressure, _} = parse_float(R8),
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

touch_finger_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[touch_finger_event_to_bytelist(E) || E<-List].

bytelist_to_touch_finger_event_array(Bytelist, Size) ->
	bytelist_to_touch_finger_event_array(Bytelist, Size, []).
bytelist_to_touch_finger_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_touch_finger_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_touch_finger_event(Bytelist),
	bytelist_to_touch_finger_event_array(Rest, Size-1, [Elem|Result]).

parse_touch_finger_event_array(Bytelist, Size) ->
	parse_touch_finger_event_array(Bytelist, Size, []).
parse_touch_finger_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_touch_finger_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_touch_finger_event(Bytelist),
	parse_touch_finger_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_touch_finger_event(Pointer) ->
	Code = int_to_bytelist(616),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_finger_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_touch_finger_event_array(Pointer, Index) ->
	Code = int_to_bytelist(617),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_finger_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_touch_finger_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(618),
	PList = pointer_to_bytelist(Pointer),
	VList = touch_finger_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_touch_finger_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(619),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = touch_finger_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_touch_finger_event() ->
	Code = int_to_bytelist(620),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_touch_finger_event_auto() ->
	Pointer = new_touch_finger_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_touch_finger_event, P);
		Error -> Error
	end.

new_touch_finger_event_array(Size) ->
	Code = int_to_bytelist(621),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_touch_finger_event_array_auto(Size) ->
	Pointer = new_touch_finger_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_touch_finger_event, P);
		Error -> Error
	end.

delete_touch_finger_event(Pointer) ->
	Code = int_to_bytelist(622),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_touch_finger_event_array(List) ->
	Size = length(List),
	Pointer = new_touch_finger_event_array(Size),
	list_to_touch_finger_event_array(List, Pointer, 0).
list_to_touch_finger_event_array([], Pointer, _Index) -> Pointer;
list_to_touch_finger_event_array([Value|List], Pointer, Index) ->
	pointer_deref_touch_finger_event_array_assign(Pointer, Index, Value),
	list_to_touch_finger_event_array(List, Pointer, Index+1).

touch_finger_event_array_to_list(Pointer, Size) ->
	touch_finger_event_array_to_list(Pointer, Size-1, []).
touch_finger_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
touch_finger_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_touch_finger_event_array(Pointer, Size),
	touch_finger_event_array_to_list(Pointer, Size-1, [Elem|Result]).

touch_finger_event_get_type(Pointer) ->
	Code = int_to_bytelist(623),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(624),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(625),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(626),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(627),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(628),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_fingerId(Pointer) ->
	Code = int_to_bytelist(629),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_finger_id(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_fingerId(Pointer, Attrib) ->
	Code = int_to_bytelist(630),
	PList = pointer_to_bytelist(Pointer),
	AList = finger_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_x(Pointer) ->
	Code = int_to_bytelist(631),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(632),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_y(Pointer) ->
	Code = int_to_bytelist(633),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(634),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_dx(Pointer) ->
	Code = int_to_bytelist(635),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_dx(Pointer, Attrib) ->
	Code = int_to_bytelist(636),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_dy(Pointer) ->
	Code = int_to_bytelist(637),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_dy(Pointer, Attrib) ->
	Code = int_to_bytelist(638),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

touch_finger_event_get_pressure(Pointer) ->
	Code = int_to_bytelist(639),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

touch_finger_event_set_pressure(Pointer, Attrib) ->
	Code = int_to_bytelist(640),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Padding, _} = parse_uint16(R8),
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

multi_gesture_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[multi_gesture_event_to_bytelist(E) || E<-List].

bytelist_to_multi_gesture_event_array(Bytelist, Size) ->
	bytelist_to_multi_gesture_event_array(Bytelist, Size, []).
bytelist_to_multi_gesture_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_multi_gesture_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_multi_gesture_event(Bytelist),
	bytelist_to_multi_gesture_event_array(Rest, Size-1, [Elem|Result]).

parse_multi_gesture_event_array(Bytelist, Size) ->
	parse_multi_gesture_event_array(Bytelist, Size, []).
parse_multi_gesture_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_multi_gesture_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_multi_gesture_event(Bytelist),
	parse_multi_gesture_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_multi_gesture_event(Pointer) ->
	Code = int_to_bytelist(641),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_multi_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_multi_gesture_event_array(Pointer, Index) ->
	Code = int_to_bytelist(642),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_multi_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_multi_gesture_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(643),
	PList = pointer_to_bytelist(Pointer),
	VList = multi_gesture_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_multi_gesture_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(644),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = multi_gesture_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_multi_gesture_event() ->
	Code = int_to_bytelist(645),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_multi_gesture_event_auto() ->
	Pointer = new_multi_gesture_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_multi_gesture_event, P);
		Error -> Error
	end.

new_multi_gesture_event_array(Size) ->
	Code = int_to_bytelist(646),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_multi_gesture_event_array_auto(Size) ->
	Pointer = new_multi_gesture_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_multi_gesture_event, P);
		Error -> Error
	end.

delete_multi_gesture_event(Pointer) ->
	Code = int_to_bytelist(647),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_multi_gesture_event_array(List) ->
	Size = length(List),
	Pointer = new_multi_gesture_event_array(Size),
	list_to_multi_gesture_event_array(List, Pointer, 0).
list_to_multi_gesture_event_array([], Pointer, _Index) -> Pointer;
list_to_multi_gesture_event_array([Value|List], Pointer, Index) ->
	pointer_deref_multi_gesture_event_array_assign(Pointer, Index, Value),
	list_to_multi_gesture_event_array(List, Pointer, Index+1).

multi_gesture_event_array_to_list(Pointer, Size) ->
	multi_gesture_event_array_to_list(Pointer, Size-1, []).
multi_gesture_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
multi_gesture_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_multi_gesture_event_array(Pointer, Size),
	multi_gesture_event_array_to_list(Pointer, Size-1, [Elem|Result]).

multi_gesture_event_get_type(Pointer) ->
	Code = int_to_bytelist(648),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(649),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(650),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(651),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(652),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(653),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_dTheta(Pointer) ->
	Code = int_to_bytelist(654),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_dTheta(Pointer, Attrib) ->
	Code = int_to_bytelist(655),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_dDist(Pointer) ->
	Code = int_to_bytelist(656),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_dDist(Pointer, Attrib) ->
	Code = int_to_bytelist(657),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_x(Pointer) ->
	Code = int_to_bytelist(658),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(659),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_y(Pointer) ->
	Code = int_to_bytelist(660),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(661),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_numFingers(Pointer) ->
	Code = int_to_bytelist(662),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_numFingers(Pointer, Attrib) ->
	Code = int_to_bytelist(663),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_get_padding(Pointer) ->
	Code = int_to_bytelist(664),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint16(DataList);
		Msg ->
			{error, Msg}
	end.

multi_gesture_event_set_padding(Pointer, Attrib) ->
	Code = int_to_bytelist(665),
	PList = pointer_to_bytelist(Pointer),
	AList = uint16_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{Y, _} = parse_float(R7),
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

dollar_gesture_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[dollar_gesture_event_to_bytelist(E) || E<-List].

bytelist_to_dollar_gesture_event_array(Bytelist, Size) ->
	bytelist_to_dollar_gesture_event_array(Bytelist, Size, []).
bytelist_to_dollar_gesture_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_dollar_gesture_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_dollar_gesture_event(Bytelist),
	bytelist_to_dollar_gesture_event_array(Rest, Size-1, [Elem|Result]).

parse_dollar_gesture_event_array(Bytelist, Size) ->
	parse_dollar_gesture_event_array(Bytelist, Size, []).
parse_dollar_gesture_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_dollar_gesture_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_dollar_gesture_event(Bytelist),
	parse_dollar_gesture_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_dollar_gesture_event(Pointer) ->
	Code = int_to_bytelist(666),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_dollar_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_dollar_gesture_event_array(Pointer, Index) ->
	Code = int_to_bytelist(667),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_dollar_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_dollar_gesture_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(668),
	PList = pointer_to_bytelist(Pointer),
	VList = dollar_gesture_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_dollar_gesture_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(669),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = dollar_gesture_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_dollar_gesture_event() ->
	Code = int_to_bytelist(670),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_dollar_gesture_event_auto() ->
	Pointer = new_dollar_gesture_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_dollar_gesture_event, P);
		Error -> Error
	end.

new_dollar_gesture_event_array(Size) ->
	Code = int_to_bytelist(671),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_dollar_gesture_event_array_auto(Size) ->
	Pointer = new_dollar_gesture_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_dollar_gesture_event, P);
		Error -> Error
	end.

delete_dollar_gesture_event(Pointer) ->
	Code = int_to_bytelist(672),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_dollar_gesture_event_array(List) ->
	Size = length(List),
	Pointer = new_dollar_gesture_event_array(Size),
	list_to_dollar_gesture_event_array(List, Pointer, 0).
list_to_dollar_gesture_event_array([], Pointer, _Index) -> Pointer;
list_to_dollar_gesture_event_array([Value|List], Pointer, Index) ->
	pointer_deref_dollar_gesture_event_array_assign(Pointer, Index, Value),
	list_to_dollar_gesture_event_array(List, Pointer, Index+1).

dollar_gesture_event_array_to_list(Pointer, Size) ->
	dollar_gesture_event_array_to_list(Pointer, Size-1, []).
dollar_gesture_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
dollar_gesture_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_dollar_gesture_event_array(Pointer, Size),
	dollar_gesture_event_array_to_list(Pointer, Size-1, [Elem|Result]).

dollar_gesture_event_get_type(Pointer) ->
	Code = int_to_bytelist(673),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(674),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(675),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(676),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_touchId(Pointer) ->
	Code = int_to_bytelist(677),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_id(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_touchId(Pointer, Attrib) ->
	Code = int_to_bytelist(678),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_gestureId(Pointer) ->
	Code = int_to_bytelist(679),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_gesture_id(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_gestureId(Pointer, Attrib) ->
	Code = int_to_bytelist(680),
	PList = pointer_to_bytelist(Pointer),
	AList = gesture_id_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_numFingers(Pointer) ->
	Code = int_to_bytelist(681),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_numFingers(Pointer, Attrib) ->
	Code = int_to_bytelist(682),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_error(Pointer) ->
	Code = int_to_bytelist(683),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_error(Pointer, Attrib) ->
	Code = int_to_bytelist(684),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_x(Pointer) ->
	Code = int_to_bytelist(685),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_x(Pointer, Attrib) ->
	Code = int_to_bytelist(686),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_get_y(Pointer) ->
	Code = int_to_bytelist(687),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_float(DataList);
		Msg ->
			{error, Msg}
	end.

dollar_gesture_event_set_y(Pointer, Attrib) ->
	Code = int_to_bytelist(688),
	PList = pointer_to_bytelist(Pointer),
	AList = float_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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
	{WindowID, _} = parse_uint32(R3),
	#drop_event{type=Type, timestamp=Timestamp, file=File, windowID=WindowID}.

parse_drop_event(Bytelist) ->
	R0 = Bytelist,
	{Type, R1} = parse_uint32(R0),
	{Timestamp, R2} = parse_uint32(R1),
	{File, R3} = parse_string(R2),
	{WindowID, R4} = parse_uint32(R3),
	{#drop_event{type=Type, timestamp=Timestamp, file=File, windowID=WindowID}, R4}.

drop_event_array_to_bytelist(List, Size) when length(List)==Size ->
	[drop_event_to_bytelist(E) || E<-List].

bytelist_to_drop_event_array(Bytelist, Size) ->
	bytelist_to_drop_event_array(Bytelist, Size, []).
bytelist_to_drop_event_array(_Bytelist, 0, Result) ->
	lists:reverse(Result);
bytelist_to_drop_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_drop_event(Bytelist),
	bytelist_to_drop_event_array(Rest, Size-1, [Elem|Result]).

parse_drop_event_array(Bytelist, Size) ->
	parse_drop_event_array(Bytelist, Size, []).
parse_drop_event_array(Bytelist, 0, Result) ->
	{lists:reverse(Result), Bytelist};
parse_drop_event_array(Bytelist, Size, Result) ->
	{Elem, Rest} = parse_drop_event(Bytelist),
	parse_drop_event_array(Rest, Size-1, [Elem|Result]).

pointer_deref_drop_event(Pointer) ->
	Code = int_to_bytelist(689),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_drop_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_drop_event_array(Pointer, Index) ->
	Code = int_to_bytelist(690),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_drop_event(DataList);
		Msg ->
			{error, Msg}
	end.

pointer_deref_drop_event_assign(Pointer, Value) ->
	Code = int_to_bytelist(691),
	PList = pointer_to_bytelist(Pointer),
	VList = drop_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

pointer_deref_drop_event_array_assign(Pointer, Index, Value) ->
	Code = int_to_bytelist(692),
	PList = pointer_to_bytelist(Pointer),
	IList = int_to_bytelist(Index),
	VList = drop_event_to_bytelist(Value),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, IList, VList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

new_drop_event() ->
	Code = int_to_bytelist(693),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_drop_event_auto() ->
	Pointer = new_drop_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_drop_event, P);
		Error -> Error
	end.

new_drop_event_array(Size) ->
	Code = int_to_bytelist(694),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_drop_event_array_auto(Size) ->
	Pointer = new_drop_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_drop_event, P);
		Error -> Error
	end.

delete_drop_event(Pointer) ->
	Code = int_to_bytelist(695),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_drop_event_array(List) ->
	Size = length(List),
	Pointer = new_drop_event_array(Size),
	list_to_drop_event_array(List, Pointer, 0).
list_to_drop_event_array([], Pointer, _Index) -> Pointer;
list_to_drop_event_array([Value|List], Pointer, Index) ->
	pointer_deref_drop_event_array_assign(Pointer, Index, Value),
	list_to_drop_event_array(List, Pointer, Index+1).

drop_event_array_to_list(Pointer, Size) ->
	drop_event_array_to_list(Pointer, Size-1, []).
drop_event_array_to_list(_Pointer, Size, Result) when Size<0 -> Result;
drop_event_array_to_list(Pointer, Size, Result) ->
	Elem = pointer_deref_drop_event_array(Pointer, Size),
	drop_event_array_to_list(Pointer, Size-1, [Elem|Result]).

drop_event_get_type(Pointer) ->
	Code = int_to_bytelist(696),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(697),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_timestamp(Pointer) ->
	Code = int_to_bytelist(698),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_timestamp(Pointer, Attrib) ->
	Code = int_to_bytelist(699),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_file(Pointer) ->
	Code = int_to_bytelist(700),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_string(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_file(Pointer, Attrib) ->
	Code = int_to_bytelist(701),
	PList = pointer_to_bytelist(Pointer),
	AList = string_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

drop_event_get_windowID(Pointer) ->
	Code = int_to_bytelist(702),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

drop_event_set_windowID(Pointer, Attrib) ->
	Code = int_to_bytelist(703),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
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

event_array_to_bytelist(List, Size) when length(List)==Size ->
	pointer_array_to_bytelist(List, Size).

bytelist_to_event_array(Bytelist, Size) ->
	bytelist_to_pointer_array(Bytelist, Size).

parse_event_array(Bytelist, Size) ->
	parse_pointer_array(Bytelist, Size).

new_event() ->
	Code = int_to_bytelist(704),
	ResultCall = call_port_owner(?PORT_NAME, [Code]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_event_auto() ->
	Pointer = new_event(),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_event, P);
		Error -> Error
	end.

new_event_array(Size) ->
	Code = int_to_bytelist(705),
	SList = int_to_bytelist(Size),
	ResultCall = call_port_owner(?PORT_NAME, [Code, SList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_pointer(DataList);
		Msg ->
			{error, Msg}
	end.

new_event_array_auto(Size) ->
	Pointer = new_event_array(Size),
	case Pointer of
		{raw_pointer, P} ->
			erlang_gc:manage_ptr(?MODULE, delete_event, P);
		Error -> Error
	end.

delete_event(Pointer) ->
	Code = int_to_bytelist(706),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

list_to_event_array(List) ->
	list_to_pointer_array(List).

event_array_to_list(Pointer, Size) ->
	pointer_array_to_list(Pointer, Size).

event_get_type(Pointer) ->
	Code = int_to_bytelist(707),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_uint32(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_type(Pointer, Attrib) ->
	Code = int_to_bytelist(708),
	PList = pointer_to_bytelist(Pointer),
	AList = uint32_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_common(Pointer) ->
	Code = int_to_bytelist(709),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_common_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_common(Pointer, Attrib) ->
	Code = int_to_bytelist(710),
	PList = pointer_to_bytelist(Pointer),
	AList = common_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_window(Pointer) ->
	Code = int_to_bytelist(711),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_window_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_window(Pointer, Attrib) ->
	Code = int_to_bytelist(712),
	PList = pointer_to_bytelist(Pointer),
	AList = window_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_key(Pointer) ->
	Code = int_to_bytelist(713),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_keyboard_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_key(Pointer, Attrib) ->
	Code = int_to_bytelist(714),
	PList = pointer_to_bytelist(Pointer),
	AList = keyboard_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_edit(Pointer) ->
	Code = int_to_bytelist(715),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_editing_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_edit(Pointer, Attrib) ->
	Code = int_to_bytelist(716),
	PList = pointer_to_bytelist(Pointer),
	AList = text_editing_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_text(Pointer) ->
	Code = int_to_bytelist(717),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_text_input_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_text(Pointer, Attrib) ->
	Code = int_to_bytelist(718),
	PList = pointer_to_bytelist(Pointer),
	AList = text_input_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_motion(Pointer) ->
	Code = int_to_bytelist(719),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_motion_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_motion(Pointer, Attrib) ->
	Code = int_to_bytelist(720),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_motion_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_button(Pointer) ->
	Code = int_to_bytelist(721),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_button(Pointer, Attrib) ->
	Code = int_to_bytelist(722),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_button_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_wheel(Pointer) ->
	Code = int_to_bytelist(723),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_mouse_wheel_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_wheel(Pointer, Attrib) ->
	Code = int_to_bytelist(724),
	PList = pointer_to_bytelist(Pointer),
	AList = mouse_wheel_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jaxis(Pointer) ->
	Code = int_to_bytelist(725),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jaxis(Pointer, Attrib) ->
	Code = int_to_bytelist(726),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_axis_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jball(Pointer) ->
	Code = int_to_bytelist(727),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_ball_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jball(Pointer, Attrib) ->
	Code = int_to_bytelist(728),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_ball_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jhat(Pointer) ->
	Code = int_to_bytelist(729),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_hat_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jhat(Pointer, Attrib) ->
	Code = int_to_bytelist(730),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_hat_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jbutton(Pointer) ->
	Code = int_to_bytelist(731),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jbutton(Pointer, Attrib) ->
	Code = int_to_bytelist(732),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_button_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_jdevice(Pointer) ->
	Code = int_to_bytelist(733),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_joy_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_jdevice(Pointer, Attrib) ->
	Code = int_to_bytelist(734),
	PList = pointer_to_bytelist(Pointer),
	AList = joy_device_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_caxis(Pointer) ->
	Code = int_to_bytelist(735),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_axis_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_caxis(Pointer, Attrib) ->
	Code = int_to_bytelist(736),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_axis_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_cbutton(Pointer) ->
	Code = int_to_bytelist(737),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_button_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_cbutton(Pointer, Attrib) ->
	Code = int_to_bytelist(738),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_button_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_cdevice(Pointer) ->
	Code = int_to_bytelist(739),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_controller_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_cdevice(Pointer, Attrib) ->
	Code = int_to_bytelist(740),
	PList = pointer_to_bytelist(Pointer),
	AList = controller_device_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_adevice(Pointer) ->
	Code = int_to_bytelist(741),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_audio_device_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_adevice(Pointer, Attrib) ->
	Code = int_to_bytelist(742),
	PList = pointer_to_bytelist(Pointer),
	AList = audio_device_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_quit(Pointer) ->
	Code = int_to_bytelist(743),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_quit_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_quit(Pointer, Attrib) ->
	Code = int_to_bytelist(744),
	PList = pointer_to_bytelist(Pointer),
	AList = quit_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_user(Pointer) ->
	Code = int_to_bytelist(745),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_user_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_user(Pointer, Attrib) ->
	Code = int_to_bytelist(746),
	PList = pointer_to_bytelist(Pointer),
	AList = user_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_syswm(Pointer) ->
	Code = int_to_bytelist(747),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_syswm_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_syswm(Pointer, Attrib) ->
	Code = int_to_bytelist(748),
	PList = pointer_to_bytelist(Pointer),
	AList = syswm_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_tfinger(Pointer) ->
	Code = int_to_bytelist(749),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_touch_finger_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_tfinger(Pointer, Attrib) ->
	Code = int_to_bytelist(750),
	PList = pointer_to_bytelist(Pointer),
	AList = touch_finger_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_mgesture(Pointer) ->
	Code = int_to_bytelist(751),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_multi_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_mgesture(Pointer, Attrib) ->
	Code = int_to_bytelist(752),
	PList = pointer_to_bytelist(Pointer),
	AList = multi_gesture_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_dgesture(Pointer) ->
	Code = int_to_bytelist(753),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_dollar_gesture_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_dgesture(Pointer, Attrib) ->
	Code = int_to_bytelist(754),
	PList = pointer_to_bytelist(Pointer),
	AList = dollar_gesture_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

event_get_drop(Pointer) ->
	Code = int_to_bytelist(755),
	PList = pointer_to_bytelist(Pointer),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList]),
	case ResultCall of
		{datalist, DataList} ->
			bytelist_to_drop_event(DataList);
		Msg ->
			{error, Msg}
	end.

event_set_drop(Pointer, Attrib) ->
	Code = int_to_bytelist(756),
	PList = pointer_to_bytelist(Pointer),
	AList = drop_event_to_bytelist(Attrib),
	ResultCall = call_port_owner(?PORT_NAME, [Code, PList, AList]),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

%--------------------------------------------------------

init(Uint32_1) ->
	Code = int_to_bytelist(757),
	Param1 = uint32_to_bytelist(Uint32_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

quit() ->
	Code = int_to_bytelist(758),

	ResultCall = call_port_owner(?PORT_NAME, [Code, []], []),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

create_window(String_1, Int_2, Int_3, Int_4, Int_5, Uint32_6) ->
	Code = int_to_bytelist(759),
	Param1 = string_to_bytelist(String_1),
	Param2 = int_to_bytelist(Int_2),
	Param3 = int_to_bytelist(Int_3),
	Param4 = int_to_bytelist(Int_4),
	Param5 = int_to_bytelist(Int_5),
	Param6 = uint32_to_bytelist(Uint32_6),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1, Param2, Param3, Param4, Param5, Param6], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_pointer(DataList),
			case RetParamAux of
				{raw_pointer, P} ->
					RetParam1 = erlang_gc:manage_ptr(?MODULE, delete_window, P);
				Error -> RetParam1 = Error
			end,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

get_window_surface(P_Window_1) ->
	Code = int_to_bytelist(760),
	Param1 = pointer_to_bytelist(P_Window_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_pointer(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

load_bmp(String_1) ->
	Code = int_to_bytelist(761),
	Param1 = string_to_bytelist(String_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_pointer(DataList),
			case RetParamAux of
				{raw_pointer, P} ->
					RetParam1 = erlang_gc:manage_ptr(?MODULE, free_surface, P);
				Error -> RetParam1 = Error
			end,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

free_surface(P_Surface_1) ->
	Code = int_to_bytelist(762),
	Param1 = pointer_to_bytelist(P_Surface_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

blit_surface(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Code = int_to_bytelist(763),
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1, Param2, Param3, Param4], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

blit_scaled(P_Surface_1, P_Rect_2, P_Surface_3, P_Rect_4) ->
	Code = int_to_bytelist(764),
	Param1 = pointer_to_bytelist(P_Surface_1),
	Param2 = pointer_to_bytelist(P_Rect_2),
	Param3 = pointer_to_bytelist(P_Surface_3),
	Param4 = pointer_to_bytelist(P_Rect_4),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1, Param2, Param3, Param4], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

update_window_surface(P_Window_1) ->
	Code = int_to_bytelist(765),
	Param1 = pointer_to_bytelist(P_Window_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

destroy_window(P_Window_1) ->
	Code = int_to_bytelist(766),
	Param1 = pointer_to_bytelist(P_Window_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, _DataList} ->
			ok;
		Msg ->
			{error, Msg}
	end.

get_window_size(P_Window_1) ->
	Code = int_to_bytelist(767),
	Param1 = pointer_to_bytelist(P_Window_1),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], []),
	case ResultCall of
		{datalist, DataList} ->
			R0 = DataList,
			{RetParam1, R1} = parse_int(R0),
			{RetParam2, _R2} = parse_int(R1),
			{RetParam1, RetParam2};
		Msg ->
			{error, Msg}
	end.

get_error() ->
	Code = int_to_bytelist(768),

	ResultCall = call_port_owner(?PORT_NAME, [Code, []], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_string(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

poll_event() ->
	Code = int_to_bytelist(769),

	ResultCall = call_port_owner(?PORT_NAME, [Code, []], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			{RetParam2, _R2} = parse_event(R1),
			{RetParam1, RetParam2};
		Msg ->
			{error, Msg}
	end.

maxint(P_Int_1, Int_2) ->
	Code = int_to_bytelist(770),
	Param1 = pointer_to_bytelist(P_Int_1),
	Param2 = int_to_bytelist(Int_2),

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1, Param2], []),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

maxint(List1) ->
	P_Int_1 = list_to_int_array(List1),
	Int_2 = length(List1),
	Value = maxint(P_Int_1, Int_2),
	delete_int(P_Int_1),
	Value.

apply_int(Int_1, Fun1, Fun2) ->
	Code = int_to_bytelist(771),
	Param1 = int_to_bytelist(Int_1),

	Fun1_Wrapper = fun(Buf) ->
		R0 = Buf,
		{P1, _R1} = parse_int(R0),
		Result = Fun1(P1),
		int_to_bytelist(Result)
	end,

	Fun2_Wrapper = fun(Buf) ->
		R0 = Buf,
		{P1, _R1} = parse_int(R0),
		Result = Fun2(P1),
		int_to_bytelist(Result)
	end,

	ResultCall = call_port_owner(?PORT_NAME, [Code, Param1], [Fun1_Wrapper, Fun2_Wrapper]),
	case ResultCall of
		{datalist, DataList} ->
			{RetParamAux, _R1} = parse_int(DataList),
			RetParam1 = RetParamAux,
			RetParam1;
		Msg ->
			{error, Msg}
	end.

