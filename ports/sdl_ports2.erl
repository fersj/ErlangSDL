-module(sdl_ports2).

% -export([start/0, init_port/0, sdl_init/1, sdl_quit/0, sdl_create_window/6, sdl_get_window_surface/1,
%     sdl_load_bmp/1, sdl_blit_surface/4, sdl_update_window_surface/1, sdl_destroy_window/1, sdl_get_window_size/1,
%     sdl_free_surface/1, sdl_blit_scaled/4, sdl_get_surface_size/1, sdl_get_error/0, sdl_poll_event/0,
%     img_load/1, img_get_error/0, event_handler/1, add_shot/1, draw_shots/2, destroy_shots/1, loop/4]).

-export([start/0]).

-import(sdl_ports_gen, [init/1, quit/0, create_window/6, get_window_surface/1, load_bmp/1, free_surface/1, blit_surface/4, blit_scaled/4, update_window_surface/1, destroy_window/1, get_window_size/1, get_error/0, poll_event/0]).

-include("sdl_ports.hrl").

-ifdef(debug).
-define(DEBUG(X, Y), io:format(X, Y)).
-else.
-define(DEBUG(X, Y), ok).
-endif.

-define(SCREEN_WIDTH, 1280).
-define(SCREEN_HEIGHT, 800).

sdl_create_window(Title, X, Y, W, H, Flags) ->
  LTitle = length(Title),
  <<Len3, Len2, Len1, Len0>> = << LTitle : 32 >>,
  <<X3, X2, X1, X0>> = << X : 32 >>,
  <<Y3, Y2, Y1, Y0>> = << Y : 32 >>,
  <<W3, W2, W1, W0>> = << W : 32 >>,
  <<H3, H2, H1, H0>> = << H : 32 >>,
  <<Flags3, Flags2, Flags1, Flags0>> = << Flags : 32 >>,
  sdl_port ! { self(), { command, [?SDL_CREATE_WINDOW_CODE, Len3, Len2, Len1, Len0] ++ Title ++ [X3, X2, X1, X0, Y3, Y2, Y1, Y0, W3, W2, W1, W0, H3, H2, H1, H0, Flags3, Flags2, Flags1, Flags0] }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << NumWindow : 32 >> = << D3, D2, D1, D0 >>,
      NumWindow;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_get_window_surface(Window) ->
  << W3, W2, W1, W0 >> = << Window : 32 >>,
  sdl_port ! { self(), { command, [?SDL_GET_WINDOW_SURFACE_CODE, W3, W2, W1, W0] }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << NumSurface : 32 >> = << D3, D2, D1, D0 >>,
      NumSurface;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_load_bmp(Filename) ->
  LFilename = length(Filename),
  <<Len3, Len2, Len1, Len0>> = << LFilename : 32 >>,
  sdl_port ! { self(), { command, [?SDL_LOAD_BMP_CODE, Len3, Len2, Len1, Len0] ++ Filename }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << NumSurface : 32 >> = << D3, D2, D1, D0 >>,
      NumSurface;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

serialize_rect(nil) -> [0];
serialize_rect(#sdl_rect{x = X, y = Y, w = W, h = H}) ->
  << X3, X2, X1, X0 >> = << X:32 >>,
  << Y3, Y2, Y1, Y0 >> = << Y:32 >>,
  << W3, W2, W1, W0 >> = << W:32 >>,
  << H3, H2, H1, H0 >> = << H:32 >>,
  [1, X3, X2, X1, X0, Y3, Y2, Y1, Y0, W3, W2, W1, W0, H3, H2, H1, H0].

sdl_blit_surface(SrcSurface, SrcRect, DstSurface, DstRect) ->
  << Src3, Src2, Src1, Src0 >> = << SrcSurface:32 >>,
  << Dst3, Dst2, Dst1, Dst0 >> = << DstSurface:32 >>,
  sdl_port ! { self(), { command, [?SDL_BLIT_SURFACE_CODE, Src3, Src2, Src1, Src0] ++ serialize_rect(SrcRect) ++ [Dst3, Dst2, Dst1, Dst0] ++ serialize_rect(DstRect) }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << Result : 32 >> = << D3, D2, D1, D0 >>,
      Result;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_update_window_surface(Window)   ->
  << W3, W2, W1, W0 >> = << Window:32 >>,
  sdl_port ! { self(), { command, [?SDL_UPDATE_WINDOW_SURFACE_CODE, W3, W2, W1, W0] }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << Result : 32 >> = << D3, D2, D1, D0 >>,
      Result;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_destroy_window(Window) ->
  << W3, W2, W1, W0 >> = << Window:32 >>,
  sdl_port ! { self(), { command, [?SDL_DESTROY_WINDOW_CODE, W3, W2, W1, W0] }},
  receive
    { _, { data, [] }} ->
      ?DEBUG("Received: ~w~n", [[]]),
      ok;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_get_window_size(Window) ->
  << W3, W2, W1, W0 >> = << Window:32 >>,
  sdl_port ! { self(), { command, [?SDL_GET_WINDOW_SIZE_CODE, W3, W2, W1, W0] }},
  receive
    { _, { data, [W3, W2, W1, W0, H3, H2, H1, H0] }} ->
      ?DEBUG("Received: ~w~n", [[W3, W2, W1, W0, H3, H2, H1, H0]]),
      << W : 32 >> = << W3, W2, W1, W0 >>,
      << H : 32 >> = << H3, H2, H1, H0 >>,
      {W, H};
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_free_surface(Surface) ->
  << S3, S2, S1, S0 >> = << Surface:32 >>,
  sdl_port ! { self(), { command, [?SDL_FREE_SURFACE_CODE, S3, S2, S1, S0] }},
  receive
    { _, { data, [] }} ->
      ?DEBUG("Received: ~w~n", [[]]),
      ok;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_blit_scaled(SrcSurface, SrcRect, DstSurface, DstRect) ->
  << Src3, Src2, Src1, Src0 >> = << SrcSurface:32 >>,
  << Dst3, Dst2, Dst1, Dst0 >> = << DstSurface:32 >>,
  sdl_port ! { self(), { command, [?SDL_BLIT_SCALED_CODE, Src3, Src2, Src1, Src0] ++ serialize_rect(SrcRect) ++ [Dst3, Dst2, Dst1, Dst0] ++ serialize_rect(DstRect) }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << Result : 32 >> = << D3, D2, D1, D0 >>,
      Result;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_get_surface_size(Surface) ->
  << S3, S2, S1, S0 >> = << Surface:32 >>,
  sdl_port ! { self(), { command, [?SDL_GET_SURFACE_SIZE_CODE, S3, S2, S1, S0] }},
  receive
    { _, { data, [W3, W2, W1, W0, H3, H2, H1, H0] }} ->
      ?DEBUG("Received: ~w~n", [[W3, W2, W1, W0, H3, H2, H1, H0]]),
      << W : 32 >> = << W3, W2, W1, W0 >>,
      << H : 32 >> = << H3, H2, H1, H0 >>,
      {W, H};
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

sdl_poll_event() ->
  sdl_port ! {self(), { command, [?SDL_POLL_EVENT_CODE] }},
  receive
    { _, { data, [Res3, Res2, Res1, Res0, T3, T2, T1, T0, S3, S2, S1, S0, Rep3, Rep2, Rep1, Rep0] }} ->
      ?DEBUG("Received: ~w~n", [[Res3, Res2, Res1, Res0, T3, T2, T1, T0, S3, S2, S1, S0, Rep3, Rep2, Rep1, Rep0]]),
      << Result : 32 >> = << Res3, Res2, Res1, Res0 >>,
      << Type : 32 >> = << T3, T2, T1, T0 >>,
      << Sym : 32 >> = << S3, S2, S1, S0 >>,
      << Repeat : 32 >> = << Rep3, Rep2, Rep1, Rep0 >>,
      {Result, #sdl_event{type = Type, sym = Sym, repeat = Repeat}};
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

img_load(Filename) ->
  LFilename = length(Filename),
  <<Len3, Len2, Len1, Len0>> = << LFilename : 32 >>,
  sdl_port ! { self(), { command, [?IMG_LOAD_CODE, Len3, Len2, Len1, Len0] ++ Filename }},
  receive
    { _, { data, [D3, D2, D1, D0] }} ->
      ?DEBUG("Received: ~w~n", [[D3, D2, D1, D0]]),
      << NumSurface : 32 >> = << D3, D2, D1, D0 >>,
      NumSurface;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      {error, Msg}
  end.

img_get_error() ->
  sdl_port ! {self(), { command, [?IMG_GET_ERROR_CODE] }},
  receive
    { _, { data, Error }} ->
      ?DEBUG("Received: ~w~n", [Error]),
      Error;
    Msg ->
      ?DEBUG("Unknown message: ~w~n", [Msg]),
      { error, Msg }
  end.

event_handler(Ship) ->
	{Res, Event} = sdl_poll_event(),
	if
		Res =/= 0 ->
			EventType = Event#sdl_event.type,
			if
        (EventType == ?SDL_QUIT) or ((EventType == ?SDL_KEYDOWN) and (Event#sdl_event.sym == ?SDLK_ESCAPE)) ->
          io:format("ENDING GAME ~n", []), {Ship, 0};
        (EventType == ?SDL_KEYDOWN) and (Event#sdl_event.sym == ?SDLK_SPACE) ->
          event_handler(add_shot(Ship));
				true -> event_handler(ship_movement_handler(Event, Ship))
			end;
      %event_handler(ship_movement_handler(Event, Ship));
		true -> {Ship, 1}
	end.

ship_movement_handler(#sdl_event{type=Type, sym=Sym, repeat=Rep}, Ship) ->
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
  {ShotW, ShotH} = sdl_get_surface_size(ShotSurface),
  NewW = round(ShotW/3),
  NewH = round(ShotH/5),
  Shot = #shot{surface = ShotSurface,
              x = Ship#ship.x + (Ship#ship.w div 2) - round(NewW/2),
              y = Ship#ship.y,
              vx=0, vy=13,
              w = NewW, h = NewH},
  Ship#ship{shots = [Shot|Ship#ship.shots]}.

draw_shots(WindowSurface, Shots) ->
  draw_shots(WindowSurface, Shots, []).
draw_shots(_WindowSurface, [], NShots) ->
  destroy_shots(NShots);
draw_shots(WindowSurface, [S|Shots], NShots) ->
  NS = S#shot{x = S#shot.x-S#shot.vx, y = S#shot.y-S#shot.vy},
  Rect = #sdl_rect{x=NS#shot.x, y=NS#shot.y, w=NS#shot.w, h=NS#shot.h},
  sdl_blit_scaled(NS#shot.surface, nil, WindowSurface, Rect),
  draw_shots(WindowSurface, Shots, [NS|NShots]).

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


loop(_WindowSurface, _Background, _Ship, 0) -> ok;
loop(WindowSurface, Background, Ship, _End) ->
  % NPosX = PosX + DirX,
  % NDirX = case (NPosX > 1080 orelse NPosX < 100)  of
  %            true -> -DirX;
  %            false -> DirX
  %         end,
  {NShip, End} = event_handler(Ship),
  MShip = move_ship(NShip),
  sdl_blit_surface(Background, nil, WindowSurface, nil),
  OShip = MShip#ship{shots = draw_shots(WindowSurface, MShip#ship.shots)},
  Rect = #sdl_rect{x = OShip#ship.x, y = OShip#ship.y, w = OShip#ship.w, h = OShip#ship.h},
  sdl_blit_scaled(OShip#ship.surface, nil, WindowSurface, Rect),
  sdl_update_window_surface(WindowSurface),

  loop(WindowSurface, Background, OShip, End).

start() ->
  sdl_ports_gen:init_port(),
  sdl_ports_gen:init(?SDL_INIT_VIDEO),
  Window = sdl_create_window("Game SDL", ?SDL_WINDOWPOS_CENTERED, ?SDL_WINDOWPOS_CENTERED, ?SCREEN_WIDTH, ?SCREEN_HEIGHT, ?SDL_WINDOW_SHOWN),
  WindowSurface = sdl_get_window_surface(Window),
  Background = sdl_load_bmp("resources/img/space_bg.bmp"),
  ShipSurface = sdl_load_bmp("resources/img/ship1.bmp"),
  RaySurface = img_load("resources/img/ray.png"),

  {ShipW, ShipH} = sdl_get_surface_size(ShipSurface),
  Ship = #ship{surface=ShipSurface,
              x=?SCREEN_WIDTH div 2, y=?SCREEN_HEIGHT div 2,
              vx=0, vy=0, vel=2,
              w=ShipW div 2, h=ShipH div 2,
              shotSurface=RaySurface, shots=[]},

  %{Time, _} = timer:tc(?MODULE, loop, [WindowSurface, Background, Ship, 100, 1, 2000]),
  loop(WindowSurface, Background, Ship, 1),

  %io:format("Time: ~w ms~n", [Time / 1000]),
  sdl_free_surface(Ship#ship.surface),
  sdl_free_surface(Background),
  sdl_destroy_window(Window),
  sdl_ports_gen:quit(),
  init:stop().
