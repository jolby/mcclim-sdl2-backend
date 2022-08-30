(cl:in-package #:asdf-user)

(defsystem #:mcclim-sdl2
  :depends-on ("mcclim-backend-common" "mcclim-fonts" "mcclim/extensions"
                                       "sdl2" "log4cl" "trivial-main-thread" "trivial-backtrace")
  :serial t
  :components ((:module "jackdaniel-spec"
                :pathname "src"
                :components ((:file "package")
                             (:file "datatype-defs" :depends-on ("package"))
                             (:file "basic" :depends-on ("package" "datatype-defs"))
                             (:file "port" :depends-on ("package" "datatype-defs" "basic"))
                             (:file "mirror" :depends-on ("package" "datatype-defs" "basic" "port"))
                             (:file "frame-manager" :depends-on ("package" "datatype-defs" "basic" "port"))
                             ;; (:file "medium" :depends-on ("port" "package"))
                             ;; (:file "graft" :depends-on ("port" "package"))
                             ))))

(defsystem #:mcclim-sdl2/demo
  :depends-on (#:mcclim-sdl2)
  :serial t
  :components ((:module "basic-demos"
                :pathname "demo"
                :components ((:file "packages")
                             (:file "001-port-event-loop")))))

;; REPL helpers
;; 1. Open this file in emacs with sly/slime
;; 2. Load this file (C-c C-l)
;; 3. Then, you can place your cursor at the end of these following
;; forms to execute them with (C-x C-e)
#+nil (asdf:load-system :mcclim-sdl2)

;; Demos
#+nil (asdf:load-system :mcclim-sdl2/demo)
#+nil (clim-sdl2-demo/001-port-event-loop:run)
