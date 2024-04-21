(asdf:defsystem :cl-spectrum
  :name "cl-spectrum"
  :author "0xsuk"
  :depends-on (:cffi
               :bt-semaphore)
  :serial t
  :components (
               (:file "package")
               (:file "alsa")
               (:file "cl-spectrum"))
  )
