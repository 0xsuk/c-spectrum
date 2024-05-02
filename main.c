#include <Windows.h>
#include <stdio.h>
#include "portaudio.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

static int patestCallback( const void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData ) {

  float sum = 0.0;
  for (int i = 0; i<framesPerBuffer; i++) {
    sum += ((float *)inputBuffer)[i];
  }
  
  printf("sum: %lf\n", sum);
  
  return 0;
}

void audioInit() {
  PaStream* stream;
  PaError err = Pa_Initialize();

  if (err != paNoError) goto error;

  err = Pa_OpenDefaultStream(&stream, 2, 2, paFloat32, 44100, 256, patestCallback, NULL);
  if (err != paNoError) goto error;
 
  err = Pa_StartStream( stream );
  if (err != paNoError) goto error;

  /* Sleep for several seconds. */
  Pa_Sleep(4*1000); //mili
  printf("done\n");

  /* err = Pa_StopStream(stream); */
  /* if (err != paNoError) goto error; */
  /* err = Pa_CloseStream( stream ); */
  /* if (err != paNoError) goto error; */
  /* Pa_Terminate(); */
  /* printf("Test finished.\n"); */
  return;
 
 error:
  Pa_Terminate();
  printf("failed to initialize portaudio: %s\n", Pa_GetErrorText(err));
}

DWORD WINAPI audioThread() {
  printf("hello audio\n");
  audioInit();
  return 0;
}

int main(int argc, char* argv[]) {
  if (SDL_Init(SDL_INIT_VIDEO)) {
    printf("SDL INIT FAILED: %s\n", SDL_GetError());
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
  
  SDL_Surface* image = IMG_Load("dmc.jpg");
  if (!image) {
    printf("bad\n");
  }
  
  
  
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
