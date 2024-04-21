(defpackage :cl-spectrum
  (:use
   :cl))

(defpackage :alsa
  (:use
   :cl
   :cffi)
  (:export
   #:snd-pcm-stream
   #:snd-pcm-format
   #:snd-pcm-access
   #:snd-pcm-open
   #:snd-strerror
   #:snd-pcm-hw-params-malloc
   #:snd-pcm-hw-params-any
   #:snd-pcm-hw-params-set-access
   #:snd-pcm-hw-params-set-format
   #:snd-pcm-hw-params-set-rate-near
   #:snd-pcm-hw-params-set-rate
   #:snd-pcm-hw-params-set-channels
   #:snd-pcm-hw-params-set-period-size-near
   #:snd-pcm-hw-params-set-buffer-size-near
   #:snd-pcm-hw-params
   #:snd-pcm-hw-params-free
   #:snd-pcm-writei
   #:snd-pcm-set-params
   #:snd-pcm-get-params
   #:snd-pcm-prepare
   #:snd-pcm-drain
   #:snd-pcm-close
   #:snd-pcm-recover))