(in-package #:asdf-user)

(defsystem "mcclim-sdl2-skia"
  :author "Daniel Kochmański (McCLIM, SDL backend). Pavel Korlev (claw, skia bindings). Joel Boehland (medium-skia hacks)."
  :description "SDL2 backend"
  :depends-on ("mcclim" "sdl2" "mcclim-sdl2" "log4cl" "lparallel" "cl-opengl" "aw-skia")
  :serial t
  :components ((:file "skia-utilities")
               (:file "skia-context")
               (:file "mirror-skia")
               (:file "medium-skia")))
