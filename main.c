#include <stdio.h>
#include "SDL.h"
#include <windows.h>
#include <mmdeviceapi.h>
#include <endpointvolume.h>

DWORD WINAPI audioThread() {
  printf("hello audio\n");
  return 0;
}

void audioInit() {
  
}

int main(int argc, char* argv[]) {
  if (SDL_Init(SDL_INIT_VIDEO)) {
    printf("SDL INIT FAILED: %s\n", SDL_GetError());
    return 1;
  }
  
  int width = 600;
  int height = 600;
  
  SDL_Window* window = SDL_CreateWindow("c-spectrum", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
  if (window == NULL) {
    printf("Window creation failed: %s\n", SDL_GetError());
    return 1;
  }
  
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
  if (renderer == NULL) {
    printf("Renderer creation failed: %s\n", SDL_GetError());
    return 1;
  }
  
  HANDLE aThread = CreateThread(
                                NULL, //security type
                                0, //initial stack size
                                audioThread,
                                NULL, //args to func
                                0, //or CreateSuspended
                                NULL //pointer to store thread id
  );
  
  int desired_delta = 1000 / 30;
  SDL_Event event;
  while (1) {
    int start = SDL_GetTicks();

    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
        goto cleanup;
      }
    }

    int delta = SDL_GetTicks() - start;
    if (delta < desired_delta) {
      SDL_Delay(desired_delta - delta);
    }
  }
  
  printf("hello sdl\n");

 cleanup:
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
