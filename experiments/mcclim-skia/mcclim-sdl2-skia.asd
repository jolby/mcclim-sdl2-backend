(in-package #:asdf-user)

(defsystem #:mcclim-sdl2-skia
  :author "Daniel Kochmański (McCLIM, SDL backend). Pavel Korlev (claw, skia bindings). Joel Boehland (mcclim-skia hacks)."
  :description "SDL2 backend"
  :depends-on ("mcclim" "mcclim-fonts" "mcclim-fonts/truetype"
                        "sdl2" "mcclim-sdl2" "mcclim-sdl2-skia/blob"
                        "log4cl" "lparallel" "cl-opengl" "aw-skia" "static-vectors"
                        "cl-muth")
  :serial t
  :components ((:file "packages")
               (:file "clim-patches")
               (:file "utilities")
               (:file "skia-types")
               (:file "skia-canvas")
               (:file "skia-context")
               (:file "mcclim-skia-types")
               (:file "mirror-opengl")
               (:file "mirror-skia")
               (:file "draw-command-queue")
               (:file "mcclim-skia-state")
               (:file "medium-skia")
               (:file "skia-sheet")
               (:file "clx-sheet")
               ))

(defsystem #:mcclim-sdl2-skia/tests
  :depends-on ("mcclim" "mcclim-sdl2-skia" "fiveam")
  :components ((:module "tests"
                        :serial t
                        :components ((:file "package")))))

(defsystem #:mcclim-sdl2-skia/blob
  :description "skia runtime blob for linux"
  :author "Pavel Korlev (claw, skia bindings). Joel Boehland (mcclim/skia hacks)"
  :mailto "jboehland@gmail.com"
  :license "MIT"
  :defsystem-depends-on (#:bodge-blobs-support)
  :class :bodge-blob-system
  :libraries ((
               ;; (:linux :x86-64) "libskia.clawed.so" "native-libs/x86_64-pc-linux-gnu/"
               (:linux :x86-64) "libskia.clawed.37.so" "native-libs/x86_64-pc-linux-gnu/"
              )))
