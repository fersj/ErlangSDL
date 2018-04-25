#include <stdio.h>
#include <time.h>

#include <SDL2/SDL.h>

#ifndef NUM_ITERS
#define NUM_ITERS 2000
#endif

int main(void) {
  SDL_Init(SDL_INIT_VIDEO);
  
  SDL_Window *window = SDL_CreateWindow("Hola, mundo", 
                   SDL_WINDOWPOS_CENTERED, 
                   SDL_WINDOWPOS_CENTERED,
                   1280, 800, 0);
  
  SDL_Surface *screen = SDL_GetWindowSurface(window);
  SDL_Surface *background = SDL_LoadBMP("resources/img/space_bg.bmp");
  SDL_Surface *ship = SDL_LoadBMP("resources/img/ship1.bmp");
  
  
  int posX = 100;
  int dirX = 1;
  for (int i = 0; i < NUM_ITERS; i++) {
    SDL_BlitSurface(background, NULL, screen, NULL);
    
    SDL_Rect rect;
    rect.x = posX;
    rect.y = 100;
    rect.w = ship->w;
    rect.h = ship->h;
    
    SDL_BlitSurface(ship, NULL, screen, &rect);
    SDL_UpdateWindowSurface(window);

    posX += dirX;
    if (posX > 1080 || posX < 100) dirX = -dirX;
  }
  
  SDL_DestroyWindow(window);
  SDL_Quit();
}