(in-package #:asdf-user)

(defsystem "mcclim-sdl2-skia"
  :author "Daniel Kochmański (McCLIM, SDL backend). Pavel Korlev (claw, skia bindings). Joel Boehland (mcclim-skia hacks)."
  :description "SDL2 backend"
  :depends-on ("mcclim" "sdl2" "mcclim-sdl2" "mcclim-sdl2-skia/blob"
                        "log4cl" "lparallel" "cl-opengl" "aw-skia" "static-vectors")
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "skia-types")
               (:file "skia-canvas")
               (:file "skia-context")
               (:file "mirror-opengl")
               (:file "skia-sheet")
               (:file "mirror-skia")
               (:file "medium-skia")
               ))

(asdf:defsystem #:mcclim-sdl2-skia/blob
  :description "skia runtime blob for linux"
  :author "Pavel Korlev (claw, skia bindings). Joel Boehland (mcclim/skia hacks)"
  :mailto "jboehland@gmail.com"
  :license "MIT"
  :defsystem-depends-on (#:bodge-blobs-support)
  :class :bodge-blob-system
  :libraries ((
               (:linux :x86-64) "libskia.clawed.so" "native-libs/x86_64-pc-linux-gnu/")
              ))
