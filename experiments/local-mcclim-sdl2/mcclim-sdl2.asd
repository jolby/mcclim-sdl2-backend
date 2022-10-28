(in-package #:asdf-user)

(defsystem "mcclim-sdl2"
  :author "Daniel Kochmański"
  :description "SDL2 backend"
  :depends-on ("mcclim" "sdl2" "log4cl" "lparallel")
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "input")
               (:file "port")
               (:file "mirror")
               (:file "medium-soft-render")))
