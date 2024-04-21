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


(defun main ()
  (sdl2:with-window (window :title "cl-spectrum"
                     :w width
                     :h height)
    (sdl2:with-renderer (renderer window :index -1 :flags '(sdl2-ffi:+sdl-renderer-software+))
      
      (let ((texture (sdl2:create-texture renderer sdl2:+pixelformat-rgba8888+ SDL2-FFI:+SDL-TEXTUREACCESS-TARGET+ *width* *height*)))
        
        ))))
