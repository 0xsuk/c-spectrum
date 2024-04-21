(in-package :sdl2)

(cffi:define-foreign-library libsdl2
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2.so.0.2" "libSDL2"))
  )

(cffi:use-foreign-library libsdl2)

(defparameter INIT-VIDEO #x20)

(cffi:defcfun (init "SDL_Init") :int
  (flags :uint32))

(cffi:defcfun (create-window "SDL_CreateWindow") :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags :uint32))

(cffi:defcfun (create-renderer "SDL_CreateRenderer") :pointer
  (window :pointer)
  (index :int)
  (flags :uint32))

(cffi:defcfun (create-texture "SDL_CreateTexture") :pointer
  (renderer :pointer)
  (format :uint32)
  (access :int)
  (w :int)
  (h :int))

(cffi:defcfun (get-ticks "SDL_GetTicks") :int)

(cffi:defcfun (poll-event "SDL_PollEvent") :int
  (event :pointer))

(cffi:defcfun (set-render-draw-color "SDL_SetRenderDrawColor") :int
  (renderer :pointer)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8)
  )

(cffi:defcfun (render-present "SDL_RenderPresent") :void
  (renderer :pointer))

(cffi:defcfun (delay "SDL_Delay") :void
  (ms :uint32))

(cffi:defcfun (destroy-renderer "SDL_DestroyRenderer") :void
  (renderer :pointer))

(cffi:defcfun (destroy-window "SDL_DestroyWindow") :void
  (window :pointer))

(cffi:defcfun (quit "SDL_Quit") :void)

(cffi:defcfun (render-copy "SDL_RenderCopy") :int
  (renderer :pointer)
  (texture :pointer)
  (srcrect :pointer)
  (dstrect :pointer))

(cffi:defcfun (set-render-target "SDL_SetRenderTarget") :int
  (renderer :pointer)
  (texture :pointer))

(cffi:defcfun (render-clear "SDL_RenderClear") :int
  (renderer :pointer))

(cffi:defcfun (render-fill-rect "SDL_RenderFillRect") :int
  (renderer :pointer)
  (rect :pointer))

(defcstruct rect
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defcfun (render-draw-line "SDL_RenderDrawLine") :int
  (renderer :pointer)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))
