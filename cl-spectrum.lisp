(in-package :cl-spectrum)

(declaim (optimize (speed 3) (safety 0)))
(declaim (inline fill-frame fill-buffer))

(defparameter *period-size* 64)
(defparameter *period-size-2* (* 2 *period-size*))
(defparameter *sample-rate* 44100)

(defparameter handle&-ref nil)

(defun alsa-setup (handle& buffer-size& period-size&)
  (cffi:with-foreign-objects ((hw-params& :pointer)
                              (hw-params&& :pointer))
    (alsa:snd-pcm-hw-params-malloc hw-params&&)
    (setf hw-params& (cffi:mem-ref hw-params&& :pointer))
    (alsa:snd-pcm-hw-params-any handle& hw-params&)
    (alsa:snd-pcm-hw-params-set-access handle& hw-params& :snd-pcm-access-rw-interleaved)
    (alsa:snd-pcm-hw-params-set-format handle& hw-params& 2)
    (alsa:snd-pcm-hw-params-set-rate handle& hw-params& *sample-rate* 0)
    (alsa:snd-pcm-hw-params-set-channels handle& hw-params& 2)
    (alsa:snd-pcm-hw-params-set-period-size-near handle& hw-params& period-size& (cffi:null-pointer))
    (alsa:snd-pcm-hw-params-set-buffer-size-near handle& hw-params& buffer-size&)
    (alsa:snd-pcm-hw-params handle& hw-params&)
    (alsa:snd-pcm-get-params handle& buffer-size& period-size&)
    (alsa:snd-pcm-hw-params-free hw-params&)))

(defconstant +dphase+ (/ (* 2 pi 440) 44100))
(defconstant +2pi+ (* 2 pi))
(defparameter *phase* 0)

(defun fill-frame (buffer& i)
  (let ((sample (round (* 32767 (sin *phase*)))))
    (setf (cffi:mem-aref buffer& :short (* 2 i))
          sample)
    (setf (cffi:mem-aref buffer& :short (1+ (* i 2)))
          sample)
    )
  (incf *phase* +dphase+)
  (if (>= *phase* +2pi+)
      (decf *phase* +2pi+))
  )

(defun fill-buffer (buffer&)
  (loop for i below *period-size*
        do (fill-frame buffer& i)))

(defun alsa-loop (handle&)
  (cffi:with-foreign-object (buffer& :short *period-size-2*)
    (loop
      do
         (sb-ext:gc)
         (fill-buffer buffer&)
         (let ((err (alsa:snd-pcm-writei handle& buffer& *period-size*)))
           (if (= err -32)
               (progn
                 ;; (princ "underrun")
                 (alsa:snd-pcm-prepare handle&))
               (when (< err 0)
                 (alsa:snd-pcm-recover handle& err 0)
                 (error (format nil "Serious error: ~A" (alsa:snd-strerror err))))))
      ))
  )

(defun alsa-start (device)
  (let ((err 0))
    (cffi:with-foreign-objects ((handle& :pointer)
                                (handle&& :pointer))
      (setf handle&-ref handle&&)
      (unwind-protect
           (progn
             (setf err (alsa:snd-pcm-open handle&&
                                     device
                                     0
                                     0))
             (when (< err 0)
               (error (alsa:snd-strerror err)))

             (setf handle& (cffi:mem-ref handle&& :pointer))
             
             (cffi:with-foreign-objects ((buffer-size& :int)
                                         (period-size& :int))
               (setf (cffi:mem-ref period-size& :int) *period-size*)
               (setf (cffi:mem-ref buffer-size& :int) (* *period-size* 2))
               (format t "want: ~A and ~A~%" (cffi:mem-ref buffer-size& :int) (cffi:mem-ref period-size& :int))
               (alsa-setup handle& buffer-size& period-size&)
               (setf *period-size* (cffi:mem-ref period-size& :int))
               (setf *period-size-2* (* *period-size* 2))
               
               (format t "got: ~A and ~A~%" (cffi:mem-ref buffer-size& :int) *period-size*))
             
             (alsa-loop handle&)
             )
        
        (format t "closing")
        (unwind-protect 
             (setf err (alsa:snd-pcm-drain handle&))
          (format  t "tried"))
        (when (< err 0)
          (error (format nil "drain failed: ~A" (alsa:snd-strerror err))))
        (alsa:snd-pcm-close handle&)
        (format t "closed")))))


(defparameter *width* 900)
(defparameter *height* 600)
(defparameter *running* t)
(defparameter *fps* 30)
(defparameter *desired-delta* (/ 1000 *fps*))

(defun draw (event renderer texture)
  (let ((start-time (sdl2:get-ticks)))
    
    (loop while (= 1 (sdl2:poll-event event)) do
      (if (= (cffi:mem-ref event :int32) #x100) ; =quit
          (setf *running* nil)))
    
    (sdl2:set-render-target renderer texture)

    (sdl2:set-render-draw-color renderer 100 255 255 255)
    (sdl2:render-draw-line renderer 0 0 *width* *height*)
    (sdl2:render-draw-line renderer 0 *height* *width* 0)
    (sdl2:set-render-target renderer (cffi:null-pointer))
    
    (cffi:with-foreign-object (dst 'sdl2:rect)
      (setf (cffi:foreign-slot-value dst 'sdl2:rect 'sdl2::x) 0
            (cffi:foreign-slot-value dst 'sdl2:rect 'sdl2::y) 0
            (cffi:foreign-slot-value dst 'sdl2:rect 'sdl2::w) *width*
            (cffi:foreign-slot-value dst 'sdl2:rect 'sdl2::h) *height*)
      (sdl2:render-copy renderer texture (cffi:null-pointer) dst))
    
    (let ((delta (- (sdl2:get-ticks) start-time)))
      (when (< delta *desired-delta*)
        (sdl2:delay (ceiling (- *desired-delta* delta)))))
    )
  )

(defun interface-loop (renderer)
  (let ((texture (sdl2:create-texture renderer 3373694468 2 *width* *height*))) ; RGBA=, TARGEt=2
    (sdl2:set-texture-blend-mode texture 0)
    (cffi:with-foreign-object (event :int32)
      (loop while *running* do
        (draw event renderer texture)
        (sdl2:render-present renderer))))
  )

(defparameter ref nil)

(defun run-interface ()
  (sdl2:init sdl2:init-video)
  (let ((window (sdl2:create-window "cl-spectrum" 100 100 *width* *height* #x4)))
    (when (cffi:null-pointer-p window)
      (error "window creation failed"))

    (let ((renderer (sdl2:create-renderer window -1 1)))
      (when (cffi:null-pointer-p renderer)
        (error "renderer creation failed"))
      (setf ref renderer)
      
      (unwind-protect
           (interface-loop renderer)
        (sdl2:destroy-renderer renderer)
        (sdl2:destroy-window window)
        (sdl2:quit)
        (format t "quit")
        (setf *running* t)
        ))) ; render software = 1
  )


(defun main ()
  (bt:make-thread 'run-interface))
