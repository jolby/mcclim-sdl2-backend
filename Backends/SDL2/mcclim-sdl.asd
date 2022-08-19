
(defsystem #:mcclim-sdl
  :depends-on ("mcclim-backend-common" "mcclim-fonts" "mcclim/extensions" "sdl2" "cl-cairo2" "log4cl")
  :components ((:file "package")
               (:file "basic" :depends-on ("package"))
               (:file "port" :depends-on ("basic"))
               (:file "medium" :depends-on ("port" "package"))
               (:file "graft" :depends-on ("port" "package"))
               (:file "frame-manager" :depends-on ("medium" "port" "package"))))
