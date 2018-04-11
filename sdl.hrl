%% SDL.h - Init flags
-define(SDL_INIT_TIMER, list_to_integer("00000001",16)).
-define(SDL_INIT_AUDIO, list_to_integer("00000010",16)).
-define(SDL_INIT_VIDEO, list_to_integer("00000020",16)).
-define(SDL_INIT_JOYSTICK, list_to_integer("00000200",16)).
-define(SDL_INIT_HAPTIC, list_to_integer("00001000",16)).
-define(SDL_INIT_GAMECONTROLLER, list_to_integer("00002000",16)).
-define(SDL_INIT_EVENTS, list_to_integer("00004000",16)).
-define(SDL_INIT_NOPARACHUTE, list_to_integer("00100000",16)).
-define(SDL_INIT_EVERYTHING, ?SDL_INIT_TIMER bor ?SDL_INIT_AUDIO bor ?SDL_INIT_VIDEO bor ?SDL_INIT_JOYSTICK bor
									  ?SDL_INIT_HAPTIC bor ?SDL_INIT_GAMECONTROLLER bor ?SDL_INIT_EVENTS bor ?SDL_INIT_NOPARACHUTE).

%% SDL_video.h - Window flags
-define(SDL_WINDOW_FULLSCREEN, list_to_integer("00000001", 16)).
-define(SDL_WINDOW_OPENGL, list_to_integer("00000002", 16)).
-define(SDL_WINDOW_SHOWN, list_to_integer("00000004", 16)).
-define(SDL_WINDOW_HIDDEN, list_to_integer("00000008", 16)).
-define(SDL_WINDOW_BORDERLESS, list_to_integer("00000010", 16)).
-define(SDL_WINDOW_RESIZABLE, list_to_integer("00000020", 16)).
-define(SDL_WINDOW_MINIMIZED, list_to_integer("00000040", 16)).
-define(SDL_WINDOW_MAXIMIZED, list_to_integer("00000080", 16)).
-define(SDL_WINDOW_INPUT_GRABBED, list_to_integer("00000100", 16)).
-define(SDL_WINDOW_INPUT_FOCUS, list_to_integer("00000200", 16)).
-define(SDL_WINDOW_MOUSE_FOCUS, list_to_integer("00000400", 16)).
-define(SDL_WINDOW_FULLSCREEN_DESKTOP, ?SDL_WINDOW_FULLSCREEN bor list_to_integer("00001000", 16)).
-define(SDL_WINDOW_FOREIGN, list_to_integer("00000800", 16)).
-define(SDL_WINDOW_ALLOW_HIGHDPI, list_to_integer("00002000", 16)).
-define(SDL_WINDOW_MOUSE_CAPTURE, list_to_integer("00004000", 16)).
-define(SDL_WINDOW_ALWAYS_ON_TOP, list_to_integer("00008000", 16)).
-define(SDL_WINDOW_SKIP_TASKBAR, list_to_integer("00010000", 16)).
-define(SDL_WINDOW_UTILITY, list_to_integer("00020000", 16)).
-define(SDL_WINDOW_TOOLTIP, list_to_integer("00040000", 16)).
-define(SDL_WINDOW_POPUP_MENU, list_to_integer("00080000", 16)).
-define(SDL_WINDOW_VULKAN, list_to_integer("10000000", 16)).

%% SDL_events.h - Event types
-define(SDL_QUIT, list_to_integer("100", 16)).
-define(SDL_KEYDOWN, list_to_integer("300", 16)).
-define(SDL_KEYUP, list_to_integer("301", 16)).

-define(SDLK_SCANCODE_MASK, list_to_integer("40000000", 16)).
-define(SDLK_SPACE, " ").
-define(SDLK_RIGHT, 79 bor ?SDLK_SCANCODE_MASK).
-define(SDLK_LEFT, 80 bor ?SDLK_SCANCODE_MASK).
-define(SDLK_DOWN, 81 bor ?SDLK_SCANCODE_MASK).
-define(SDLK_UP, 82 bor ?SDLK_SCANCODE_MASK).

%% Records
-record(surface, {surface, x, y, vx, vy, vel, w, h}).