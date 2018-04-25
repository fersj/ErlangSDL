//
//  erlangSDL.c
//  erlangSDL
//
//  Created by Fernando Suárez Jiménez on 20/3/18.
//  Copyright © 2018 Fernando Suárez Jiménez. All rights reserved.
//

#include <erl_nif.h>
#include <SDL2/SDL.h>
#include <SDL2_image/SDL_image.h>
#include <assert.h>

const int BUFFER_SIZE = 256;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_null;

static ErlNifResourceType *surface_type;
static ErlNifResourceType *window_type;
static ErlNifResourceType *event_type;

static void surface_dtor (ErlNifEnv *env, void *surface) {
    SDL_Surface *s = (SDL_Surface *) surface;
    SDL_FreeSurface(s);
}

static void window_dtor (ErlNifEnv *env, void *window) {
    SDL_Window **w = (SDL_Window **) window;
    SDL_DestroyWindow(*w);
}

static void event_dtor (ErlNifEnv *env, void *event) {
    SDL_Event *e = (SDL_Event *) event;
    SDL_free(e);
}

static int on_load (ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_null = enif_make_atom(env, "null");
    surface_type = enif_open_resource_type(env, "sdl", "surface", surface_dtor, ERL_NIF_RT_CREATE, NULL);
    window_type = enif_open_resource_type(env, "sdl", "window", window_dtor, ERL_NIF_RT_CREATE, NULL);
    event_type = enif_open_resource_type(env, "sdl", "event", event_dtor, ERL_NIF_RT_CREATE, NULL);
    
    return 0;
}

static ERL_NIF_TERM Init (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    Uint32 flags;
    int result;
    if (!enif_get_uint(env, argv[0], &flags)) {
        return enif_make_badarg(env);
    }
    
    result = SDL_Init(flags);
    
    return enif_make_int(env, result);
}

static ERL_NIF_TERM Quit (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 0);
    
    SDL_Quit();
    
    return atom_ok;
}

static ERL_NIF_TERM CreateWindow (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 6);
    
    char title[BUFFER_SIZE];
    int x, y, w, h;
    Uint32 flags;
    if (!enif_get_string(env, argv[0], title, BUFFER_SIZE, ERL_NIF_LATIN1) ||
        !enif_get_int(env, argv[1], &x) ||
        !enif_get_int(env, argv[2], &y) ||
        !enif_get_int(env, argv[3], &w) ||
        !enif_get_int(env, argv[4], &h) ||
        !enif_get_uint(env, argv[5], &flags)) {
        return enif_make_badarg(env);
    }
    
    SDL_Window **window = (SDL_Window **) enif_alloc_resource(window_type, sizeof(SDL_Window *));
    if (window == NULL) return atom_null;
    *window = SDL_CreateWindow(title, x, y, w, h, flags);
    
    if (window == NULL) {
        enif_release_resource(window);
        return atom_null;
    }
    ERL_NIF_TERM result = enif_make_resource(env, window);
    enif_release_resource(window);

    return result;
}

static ERL_NIF_TERM GetWindowSurface (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], window_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Window **window = (SDL_Window **) resource;
    
    SDL_Surface *surface = (SDL_Surface *) enif_alloc_resource(surface_type, sizeof(SDL_Surface));
    if (surface == NULL) return atom_null;
    
    SDL_Surface *aux = SDL_GetWindowSurface(*window);
    if (aux == NULL) {
        enif_release_resource(surface);
        return atom_null;
    }
    *surface = *aux;
    ERL_NIF_TERM result = enif_make_resource(env, surface);
    enif_release_resource(surface);
    
    return result;
}

static ERL_NIF_TERM GetWindowSize (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], window_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Window **window = (SDL_Window **) resource;
    int w, h;
    
    SDL_GetWindowSize(*window, &w, &h);
    
    return enif_make_tuple(env, 2, enif_make_int(env, w), enif_make_int(env, h));
}

static ERL_NIF_TERM DestroyWindow (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], surface_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Window **window = (SDL_Window **) resource;
    
    SDL_DestroyWindow(*window);
    
    return atom_ok;
}

static ERL_NIF_TERM UpdateWindowSurface (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], window_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Window **window = (SDL_Window **) resource;
    
    int result = SDL_UpdateWindowSurface(*window);
    
    return enif_make_int(env, result);
}

static ERL_NIF_TERM FreeSurface (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], surface_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Surface *surface = (SDL_Surface *) resource;
    
    SDL_FreeSurface(surface);
    
    return atom_ok;
}

static ERL_NIF_TERM BlitSurface (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 4);
    
    void *src_res, *dst_res;
    if (!enif_get_resource(env, argv[0], surface_type, &src_res) ||
        !enif_get_resource(env, argv[2], surface_type, &dst_res)) {
        return enif_make_badarg(env);
    }
    SDL_Surface *src = (SDL_Surface *) src_res;
    SDL_Surface *dst = (SDL_Surface *) dst_res;

    char srcrect_atom[BUFFER_SIZE];
    const ERL_NIF_TERM *srcrect_term = NULL;
    int srcrect_arity;
    if (!enif_get_atom(env, argv[1], srcrect_atom, BUFFER_SIZE, ERL_NIF_LATIN1))
        if (!enif_get_tuple(env, argv[1], &srcrect_arity, &srcrect_term))
            return enif_make_badarg(env);
    SDL_Rect srcrect;
    if (srcrect_term != NULL)
        if(!enif_get_int(env, srcrect_term[0], &srcrect.x) ||
           !enif_get_int(env, srcrect_term[1], &srcrect.y) ||
           !enif_get_int(env, srcrect_term[2], &srcrect.w) ||
           !enif_get_int(env, srcrect_term[3], &srcrect.h))
            return enif_make_badarg(env);

    char dstrect_atom[BUFFER_SIZE];
    const ERL_NIF_TERM *dstrect_term = NULL;
    int dstrect_arity;
    if (!enif_get_atom(env, argv[3], dstrect_atom, BUFFER_SIZE, ERL_NIF_LATIN1))
        if (!enif_get_tuple(env, argv[3], &dstrect_arity, &dstrect_term))
            return enif_make_badarg(env);
    SDL_Rect dstrect;
    if (dstrect_term != NULL)
        if(!enif_get_int(env, dstrect_term[0], &dstrect.x) ||
           !enif_get_int(env, dstrect_term[1], &dstrect.y) ||
           !enif_get_int(env, dstrect_term[2], &dstrect.w) ||
           !enif_get_int(env, dstrect_term[3], &dstrect.h))
            return enif_make_badarg(env);

    int result;
    if (srcrect_term==NULL && dstrect_term==NULL)
        result = SDL_BlitSurface(src, NULL, dst, NULL);
    else if (srcrect_term==NULL && dstrect_term!=NULL)
        result = SDL_BlitSurface(src, NULL, dst, &dstrect);
    else if (srcrect_term!=NULL && dstrect_term==NULL)
        result = SDL_BlitSurface(src, &srcrect, dst, NULL);
    else
        result = SDL_BlitSurface(src, &srcrect, dst, &dstrect);
    
    return enif_make_int(env, result);
}

static ERL_NIF_TERM BlitScaled (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 4);
    
    void *src_res, *dst_res;
    if (!enif_get_resource(env, argv[0], surface_type, &src_res) ||
        !enif_get_resource(env, argv[2], surface_type, &dst_res)) {
        return enif_make_badarg(env);
    }
    SDL_Surface *src = (SDL_Surface *) src_res;
    SDL_Surface *dst = (SDL_Surface *) dst_res;
    
    char srcrect_atom[BUFFER_SIZE];
    const ERL_NIF_TERM *srcrect_term = NULL;
    int srcrect_arity;
    if (!enif_get_atom(env, argv[1], srcrect_atom, BUFFER_SIZE, ERL_NIF_LATIN1))
        if (!enif_get_tuple(env, argv[1], &srcrect_arity, &srcrect_term))
            return enif_make_badarg(env);
    SDL_Rect srcrect;
    if (srcrect_term != NULL)
        if(!enif_get_int(env, srcrect_term[0], &srcrect.x) ||
           !enif_get_int(env, srcrect_term[1], &srcrect.y) ||
           !enif_get_int(env, srcrect_term[2], &srcrect.w) ||
           !enif_get_int(env, srcrect_term[3], &srcrect.h))
            return enif_make_badarg(env);

    char dstrect_atom[BUFFER_SIZE];
    const ERL_NIF_TERM *dstrect_term = NULL;
    int dstrect_arity;
    if (!enif_get_atom(env, argv[3], dstrect_atom, BUFFER_SIZE, ERL_NIF_LATIN1))
        if (!enif_get_tuple(env, argv[3], &dstrect_arity, &dstrect_term))
            return enif_make_badarg(env);
    SDL_Rect dstrect;
    if (dstrect_term != NULL)
        if(!enif_get_int(env, dstrect_term[0], &dstrect.x) ||
           !enif_get_int(env, dstrect_term[1], &dstrect.y) ||
           !enif_get_int(env, dstrect_term[2], &dstrect.w) ||
           !enif_get_int(env, dstrect_term[3], &dstrect.h))
            return enif_make_badarg(env);

    int result;
    if (srcrect_term==NULL && dstrect_term==NULL)
        result = SDL_BlitScaled(src, NULL, dst, NULL);
    else if (srcrect_term==NULL && dstrect_term!=NULL)
        result = SDL_BlitScaled(src, NULL, dst, &dstrect);
    else if (srcrect_term!=NULL && dstrect_term==NULL)
        result = SDL_BlitScaled(src, &srcrect, dst, NULL);
    else
        result = SDL_BlitScaled(src, &srcrect, dst, &dstrect);
    
    return enif_make_int(env, result);
}

static ERL_NIF_TERM GetSurfaceSize (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], surface_type, &resource)) {
        return enif_make_badarg(env);
    }
    SDL_Surface *surface = (SDL_Surface *) resource;
    
    return enif_make_tuple2(env, enif_make_int(env, surface->w), enif_make_int(env, surface->h));
}

static ERL_NIF_TERM GetError (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 0);
    
    return enif_make_string(env, SDL_GetError(), ERL_NIF_LATIN1);
}

static ERL_NIF_TERM PollEvent (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 0);
    
    SDL_Event *event = (SDL_Event *) enif_alloc_resource(event_type, sizeof(SDL_Event));
    if (event == NULL) return atom_null;
    
    SDL_Event e;
    int result = SDL_PollEvent(&e);
    *event = e;
    ERL_NIF_TERM event_result = enif_make_resource(env, event);
    enif_release_resource(event);
    
    // return {result, event}
    return enif_make_tuple(env, 2, enif_make_int(env, result), event_result);
}

static ERL_NIF_TERM GetEventType (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    void *resource;
    if (!enif_get_resource(env, argv[0], event_type, &resource))
        return enif_make_badarg(env);
    SDL_Event *event = (SDL_Event *) resource;

    return enif_make_uint(env, event->type);
}

static ERL_NIF_TERM ImgLoad (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 1);
    
    char path[BUFFER_SIZE];
    if (!enif_get_string(env, argv[0], path, BUFFER_SIZE, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    SDL_Surface *surface = (SDL_Surface *) enif_alloc_resource(surface_type, sizeof(SDL_Surface));
    if (surface == NULL) return atom_null;
    
    SDL_Surface *aux = IMG_Load(path);
    if (aux == NULL) {
        enif_release_resource(surface);
        return atom_null;
    }
    *surface = *aux;
    ERL_NIF_TERM result = enif_make_resource(env, surface);
    enif_release_resource(surface);
    
    return result;
}

static ERL_NIF_TERM ImgGetError (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    assert(argc == 0);
    
    return enif_make_string(env, IMG_GetError(), ERL_NIF_LATIN1);
}

static ErlNifFunc funcs[] = {
    {"init", 1, Init},
    {"quit", 0, Quit},
    {"createWindow", 6, CreateWindow},
    {"getWindowSurface", 1, GetWindowSurface},
    {"getWindowSize", 1, GetWindowSize},
    {"destroyWindow", 1, DestroyWindow},
    {"updateWindowSurface", 1, UpdateWindowSurface},
    {"freeSurface", 1, FreeSurface},
    {"blitSurface", 4, BlitSurface},
    {"blitScaled", 4, BlitScaled},
    {"getSurfaceSize", 1, GetSurfaceSize},
    {"getError", 0, GetError},
    {"pollEvent", 0, PollEvent},
    {"getEventType", 1, GetEventType},
    {"imgLoad", 1, ImgLoad},
    {"imgGetError", 0, ImgGetError}
    // TODO get event.key.keysym.sym...
    //      SDL_ConvertSurface, getScreenFormat
};

ERL_NIF_INIT(sdl, funcs, on_load, NULL, NULL, NULL)
