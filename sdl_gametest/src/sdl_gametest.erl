-module(sdl_gametest).

%% API imports
-import(sdl_ports_gen,[
	init_port/0,
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
	event_get_type/1,
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
	apply_int/3,
	new_int/0,
	pointer_deref_int_assign/2,
	pointer_deref_int/1]).

%% API exports
-export([main/1, start/0]).

%% API include
-include("../sdl_generator/generated/sdl_ports_gen.hrl").

-define(SCREEN_WIDTH, 1280).
-define(SCREEN_HEIGHT, 800).

-record(ship, {surface, x, y, vx, vy, vel, w, h, shotSurface, shots}).
-record(shot, {surface, x, y, vx, vy, w, h}).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    start(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

event_handler(Ship) ->
	{Res, Event} = sdl_ports_gen:poll_event(),
	if
		Res =/= 0 ->
			EventType = sdl_ports_gen:event_get_type(Event),
			KeyEvent = sdl_ports_gen:event_get_key(Event),
			if
        (EventType == ?SDL_QUIT) or ((EventType == ?SDL_KEYDOWN) and (KeyEvent#keyboard_event.keysym#keysym.sym == ?SDLK_ESCAPE)) ->
          io:format("ENDING GAME ~n", []), {Ship, 0};
        (EventType == ?SDL_KEYDOWN) and (KeyEvent#keyboard_event.keysym#keysym.sym == ?SDLK_SPACE) ->
        	event_handler(add_shot(Ship));
				true -> event_handler(ship_movement_handler(Event, Ship))
			end;
		true -> {Ship, 1}
	end.

ship_movement_handler(Event, Ship) ->
	Type = sdl_ports_gen:event_get_type(Event),
	KeyEvent = sdl_ports_gen:event_get_key(Event),
	Sym = KeyEvent#keyboard_event.keysym#keysym.sym,
	Rep = KeyEvent#keyboard_event.repeat,
  #ship{vx=Vx, vy=Vy, vel=Vel} = Ship,
  {NVx, NVy} = if
    (Type == ?SDL_KEYDOWN) and (Rep == 0) ->
      if
        Sym == ?SDLK_UP -> {Vx, Vy-Vel};
        Sym == ?SDLK_DOWN -> {Vx, Vy+Vel};
        Sym == ?SDLK_LEFT -> {Vx-Vel, Vy};
        Sym == ?SDLK_RIGHT -> {Vx+Vel, Vy};
        true -> {Vx, Vy}
      end;
    true ->
      if
        (Type == ?SDL_KEYUP) and (Rep == 0) ->
          if
            Sym == ?SDLK_UP -> {Vx, Vy+Vel};
            Sym == ?SDLK_DOWN -> {Vx, Vy-Vel};
            Sym == ?SDLK_LEFT -> {Vx+Vel, Vy};
            Sym == ?SDLK_RIGHT -> {Vx-Vel, Vy};
            true -> {Vx, Vy}
          end;
        true -> {Vx, Vy}
      end
  end,
  Ship#ship{vx=NVx, vy=NVy}.

move_ship(Ship) ->
  #ship{x=X, y=Y, vx=Vx, vy=Vy} = Ship,
  Ship#ship{x=X+Vx, y=Y+Vy}.

add_shot(Ship) ->
  ShotSurface = Ship#ship.shotSurface,
  ShotW = sdl_ports_gen:surface_get_w(ShotSurface),
	ShotH = sdl_ports_gen:surface_get_h(ShotSurface),
  NewW = round(ShotW/3),
  NewH = round(ShotH/5),
  Shot = #shot{surface = ShotSurface,
              x = Ship#ship.x + (Ship#ship.w div 2) - round(NewW/2),
              y = Ship#ship.y,
              vx=0, vy=13,
              w = NewW, h = NewH},
  Ship#ship{shots = [Shot|Ship#ship.shots]}.

draw_shots(WindowSurfacePtr, Shots) ->
  draw_shots(WindowSurfacePtr, Shots, []).
draw_shots(_WindowSurfacePtr, [], NShots) ->
  destroy_shots(NShots);
draw_shots(WindowSurfacePtr, [S|Shots], NShots) ->
  NS = S#shot{x = S#shot.x-S#shot.vx, y = S#shot.y-S#shot.vy},
	SrcRectPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(SrcRectPtr, 0),
	sdl_ports_gen:rect_set_y(SrcRectPtr, 0),
	sdl_ports_gen:rect_set_w(SrcRectPtr, sdl_ports_gen:surface_get_w(NS#shot.surface)),
	sdl_ports_gen:rect_set_h(SrcRectPtr, sdl_ports_gen:surface_get_h(NS#shot.surface)),
	DstRectPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(DstRectPtr, NS#shot.x),
	sdl_ports_gen:rect_set_y(DstRectPtr, NS#shot.y),
	sdl_ports_gen:rect_set_w(DstRectPtr, NS#shot.w),
	sdl_ports_gen:rect_set_h(DstRectPtr, NS#shot.h),
  sdl_ports_gen:blit_scaled(NS#shot.surface, SrcRectPtr, WindowSurfacePtr, DstRectPtr),
  draw_shots(WindowSurfacePtr, Shots, [NS|NShots]).

destroy_shots(Shots) ->
  destroy_shots(Shots, []).
destroy_shots([], NewShots) -> NewShots;
destroy_shots([S|Shots], NewShots) ->
  if
    S#shot.y < -S#shot.h ->
      destroy_shots(Shots, NewShots);
    true ->
      destroy_shots(Shots, [S|NewShots])
  end.


loop(_WindowPtr, _WindowSurfacePtr, _BackgroundPtr, _Ship, 0) -> ok;
loop(WindowPtr, WindowSurfacePtr, BackgroundPtr, Ship, _End) ->
  {NShip, End} = event_handler(Ship),
  MShip = move_ship(NShip),

	SrcRectPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(SrcRectPtr, 0),
	sdl_ports_gen:rect_set_y(SrcRectPtr, 0),
	sdl_ports_gen:rect_set_w(SrcRectPtr, sdl_ports_gen:surface_get_w(BackgroundPtr)),
	sdl_ports_gen:rect_set_h(SrcRectPtr, sdl_ports_gen:surface_get_h(BackgroundPtr)),
	%io:format("rect: ~p~n", [pointer_deref_rect(SrcRectPtr)]),
	DstRectPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(DstRectPtr, 0),
	sdl_ports_gen:rect_set_y(DstRectPtr, 0),
	sdl_ports_gen:rect_set_w(DstRectPtr, ?SCREEN_WIDTH),
	sdl_ports_gen:rect_set_h(DstRectPtr, ?SCREEN_HEIGHT),
	%io:format("rect: ~p~n", [pointer_deref_rect(DstRectPtr)]),
	case sdl_ports_gen:blit_surface(BackgroundPtr, SrcRectPtr, WindowSurfacePtr, DstRectPtr) of
		0 -> ok;
		_ -> io:format("SDL blit_surface error: ~p~n", [sdl_ports_gen:get_error()])
	end,

  OShip = MShip#ship{shots = draw_shots(WindowSurfacePtr, MShip#ship.shots)},
	SrcRectShipPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(SrcRectShipPtr, 0),
	sdl_ports_gen:rect_set_y(SrcRectShipPtr, 0),
	sdl_ports_gen:rect_set_w(SrcRectShipPtr, sdl_ports_gen:surface_get_w(MShip#ship.surface)),
	sdl_ports_gen:rect_set_h(SrcRectShipPtr, sdl_ports_gen:surface_get_h(MShip#ship.surface)),
	DstRectShipPtr = sdl_ports_gen:new_rect(),
	sdl_ports_gen:rect_set_x(DstRectShipPtr, OShip#ship.x),
	sdl_ports_gen:rect_set_y(DstRectShipPtr, OShip#ship.y),
	sdl_ports_gen:rect_set_w(DstRectShipPtr, OShip#ship.w),
	sdl_ports_gen:rect_set_h(DstRectShipPtr, OShip#ship.h),
	case sdl_ports_gen:blit_scaled(OShip#ship.surface, SrcRectShipPtr, WindowSurfacePtr, DstRectShipPtr) of
		0 -> ok;
		_ -> io:format("SDL blit_surface error: ~p~n", [sdl_ports_gen:get_error()])
	end,
	case sdl_ports_gen:update_window_surface(WindowPtr) of
		0 -> ok;
		_ -> io:format("SDL update_window_surface error: ~p~n", [sdl_ports_gen:get_error()])
	end,
	%io:format("Hago todo~n"),

  loop(WindowPtr, WindowSurfacePtr, BackgroundPtr, OShip, End).

start() ->
  sdl_ports_gen:init_port(),
	io:format("Puerto iniciado~n", []),
  case sdl_ports_gen:init(?SDL_INIT_VIDEO) of
		0 -> io:format("SDL iniciado correctamente ~n", []);
		_ -> io:format("SDL init error: ~p~n", [sdl_ports_gen:get_error()])
	end,

  WindowPtr = sdl_ports_gen:create_window("Game SDL", ?SDL_WINDOWPOS_CENTERED, ?SDL_WINDOWPOS_CENTERED, ?SCREEN_WIDTH, ?SCREEN_HEIGHT, ?SDL_WINDOW_SHOWN),
	case WindowPtr of
		0 -> io:format("SDL create_window error: ~p~n", [sdl_ports_gen:get_error()]);
		_ -> io:format("Window creada: ~p, ~p~n", [WindowPtr, sdl_ports_gen:get_window_size(WindowPtr)])
	end,
  WindowSurfacePtr = sdl_ports_gen:get_window_surface(WindowPtr),
	io:format("Window surface obtenida: ~p, (~px~p)~n", [WindowSurfacePtr, sdl_ports_gen:surface_get_w(WindowSurfacePtr), sdl_ports_gen:surface_get_h(WindowSurfacePtr)]),

	BackgroundPath = "resources/img/space_bg.bmp",
  BackgroundPtr = sdl_ports_gen:load_bmp(BackgroundPath),
	case BackgroundPtr of
		0 -> io:format("SDL load_bmp error: ~p~n", [sdl_ports_gen:get_error()]);
		_ -> io:format("BMP cargado (~p): ~p~n", [BackgroundPath, BackgroundPtr])
	end,
	ShipPath = "resources/img/ship1.bmp",
  ShipSurfacePtr = sdl_ports_gen:load_bmp(ShipPath),
	case ShipSurfacePtr of
		0 -> io:format("SDL load_bmp error: ~p~n", [sdl_ports_gen:get_error()]);
		_ -> io:format("BMP cargado (~p): ~p~n", [ShipPath, ShipSurfacePtr])
	end,
	RayPath = "resources/img/ray.bmp",
  RaySurfacePtr = sdl_ports_gen:load_bmp(RayPath),
	case RaySurfacePtr of
		0 -> io:format("SDL load_bmp error: ~p~n", [sdl_ports_gen:get_error()]);
		_ -> io:format("BMP cargado (~p): ~p~n", [RayPath, RaySurfacePtr])
	end,

	ShipSurface = sdl_ports_gen:pointer_deref_surface(ShipSurfacePtr),
	ShipW = ShipSurface#surface.w,
	ShipH = ShipSurface#surface.h,
  Ship = #ship{surface=ShipSurfacePtr,
              x=?SCREEN_WIDTH div 2, y=?SCREEN_HEIGHT div 2,
              vx=0, vy=0, vel=2,
              w=ShipW div 2, h=ShipH div 2,
              shotSurface=RaySurfacePtr, shots=[]},
	io:format("Ship record: ~p~n", [Ship]),

  %{Time, _} = timer:tc(?MODULE, loop, [WindowSurface, Background, Ship, 100, 1, 2000]),
  loop(WindowPtr, WindowSurfacePtr, BackgroundPtr, Ship, 1),

	% Result = sdl_ports_gen:apply_int(3, fun(X) -> X * 2 end, fun(X) -> X + 1 end),
	% io:format("Result apply_int: ~p~n", [Result]),
	%
	% Ptr = sdl_ports_gen:new_int(),
	% io:format("Ptr: ~p~n", [Ptr]),
	% sdl_ports_gen:pointer_deref_int_assign(Ptr, 10),
	% io:format("Deref int: ~p~n", [sdl_ports_gen:pointer_deref_int(Ptr)]),
	% Fun1 = fun(X) ->
	% 					V = sdl_ports_gen:pointer_deref_int(Ptr),
	% 					io:format("Deref int (Fun1):~p~n",[V]),
	% 					V + X
	% 			 end,
	% Fun2 = fun(X) -> X + 1 end,
  % Result2 = sdl_ports_gen:apply_int(3, Fun1, Fun2),
	% io:format("Result apply_int ptr: ~p~n", [Result2]),

  %io:format("Time: ~w ms~n", [Time / 1000]),
  sdl_ports_gen:free_surface(Ship#ship.surface),
  sdl_ports_gen:free_surface(BackgroundPtr),
  sdl_ports_gen:destroy_window(WindowPtr),
  sdl_ports_gen:quit().
