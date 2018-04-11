-module(sdl).

-export([init/1,
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

-on_load(init/0).

init () ->
	ok = erlang:load_nif("./erlangSDL", 0).
	
init (_) ->
	error(library_not_loaded).

quit () ->
	error(library_not_loaded).

createWindow (_,_,_,_,_,_) ->
	error(library_not_loaded).

getWindowSurface (_) ->
	error(library_not_loaded).

getWindowSize (_) ->
	error(library_not_loaded).

destroyWindow (_) ->
	error(library_not_loaded).

updateWindowSurface (_) ->
	error(library_not_loaded).

freeSurface (_) ->
	error(library_not_loaded).

blitSurface (_,_,_,_) ->
	error(library_not_loaded).

blitScaled (_,_,_,_) ->
	error(library_not_loaded).

getSurfaceSize (_) ->
	error(library_not_loaded).

getError () ->
	error(library_not_loaded).

pollEvent () ->
	error(library_not_loaded).

getEventType (_) ->
	error(library_not_loaded).

imgLoad (_) ->
	error(library_not_loaded).

imgGetError () ->
	error(library_not_loaded).
