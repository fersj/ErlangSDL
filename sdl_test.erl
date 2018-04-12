-module(sdl_test).

-import(sdl,
			[init/1,
			quit/0,
			createWindow/6,
			getWindowSurface/1,
			getWindowSize/1,
			destroyWindow/1,
			updateWindowSurface/1,
			freeSurface/1,
			blitSurface/4,
			blitScaled/4,
			getSurfaceSize/1,
			getError/0,
			pollEvent/0,
			getEventType/1,
			imgLoad/1,
			imgGetError/0]).

-compile(export_all).
-include("sdl.hrl").

-define(SCREEN_WIDTH, 1280).
-define(SCREEN_HEIGHT, 720).

init () ->
	% Init SDL, Window and Screen surface
	Init = sdl:init(?SDL_INIT_VIDEO),
	if
		Init < 0 ->
			%io:format("SDL could not initialize! SDL_Error: ~p~n", [sdl:getError()]),
			exit(string:concat("SDL could not initialize! SDL_Error: ", sdl:getError()));
		true -> ok
	end,
	Window = sdl:createWindow("Space TFM", ?SCREEN_WIDTH, ?SCREEN_HEIGHT, 1280, 720, ?SDL_WINDOW_SHOWN),
	if
		Window == null ->
			%io:format("Window could not be created! SDL_Error: ~p~n", [sdl:getError()]);
			exit(string:concat("Window could not be created! SDL_Error: ", sdl:getError()));
		true -> ok
	end,	
	Screen = sdl:getWindowSurface(Window),
	% ScreenSize = sdl:getSurfaceSize(Screen),
	% io:format("Screen size: ~p~n", [ScreenSize]),

	% Load media
	BackgroundPath = "resources/img/space_bg.png",
	Background = sdl:imgLoad(BackgroundPath),
	if
		Background == null -> 
			io:format("Unable to load image ~p! SDL_Image_Error: ~p~n", [BackgroundPath, sdl:imgGetError()]);
		true -> ok
	end,
	ShipPath = "resources/img/ship1.png",
	Ship = sdl:imgLoad(ShipPath),
	if
		Ship == null -> 
			io:format("Unable to load image ~p! SDL_Image_Error: ~p~n", [ShipPath, sdl:imgGetError()]);
		true -> ok
	end,
	LaserPath = "resources/img/ray.png",
	Laser = sdl:imgLoad(LaserPath),
	if
		Laser == null -> 
			io:format("Unable to load image ~p! SDL_Image_Error: ~p~n", [LaserPath, sdl:imgGetError()]);
		true -> ok
	end,
	{WX, WY} = sdl:getWindowSize(Window),

	% Main loop
	{ShipW, ShipH} = sdl:getSurfaceSize(Ship),
	ShipRec = #surface{surface = Ship,
							 x = WX div 2,
							 y = WY div 2,
							 vx = 0,
							 vy = 0,
							 vel = 2,
							 w = ShipW div 2,
							 h = ShipH div 2},
	{LaserW, LaserH} = sdl:getSurfaceSize(Laser),
	LaserRec = #surface{surface = Laser,
							 vx = 0,
							 vy = 13,
							 w = LaserW div 10,
							 h = LaserH div 10},
	mainLoop(Window, Screen, Background, ShipRec, LaserRec),
	io:format("Salgo del bucle principal~n", []),

	% Free surfaces and quit SDL
	sdl:freeSurface(Background),
	sdl:freeSurface(Screen),
	sdl:destroyWindow(Window),
	sdl:quit(),
	io:format("SDL Finished", []).


mainLoop (Window, Screen, Background, Ship, Laser) ->
	mainLoop(Window, Screen, Background, Ship, Laser, false).
mainLoop (_Window, _Screen, _Background, _Ship, _Laser, true) ->
	ok;
mainLoop (Window, Screen, Background, Ship, Laser, _Exit) ->
	%Exit = handleEvent(),
	%io:format("Poll Event: ~p~n", [sdl:pollEvent()]),
	sdl:blitSurface(Background, null, Screen, null),
	sdl:blitScaled(Ship#surface.surface, null, Screen, {Ship#surface.x, Ship#surface.y, Ship#surface.w, Ship#surface.h}),
	sdl:updateWindowSurface(Window),
	mainLoop(Window, Screen, Background, Ship, Laser, false).

handleEvent () ->
	{Code, Event} = sdl:pollEvent(),
	if
		Code =/= 0 ->
			EventType = sdl:getEventType(Event),
			SDL_QUIT = ?SDL_QUIT,
			if
				EventType == SDL_QUIT -> true;
				true -> handleEvent()
			end;
		true -> false
	end.
	

