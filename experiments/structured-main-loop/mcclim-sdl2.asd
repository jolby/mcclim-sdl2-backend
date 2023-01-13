(in-package #:asdf-user)

(defsystem "mcclim-sdl2"
  :author "Daniel Kochmański"
  :description "SDL2 backend"
  :depends-on ("mcclim" "sdl2" "log4cl"
                        #+sbcl "sb-concurrency"
                        "lparallel" "trivial-main-thread" "cl-muth" "stmx" "timer-wheel"
                        "journal" "nhooks")
  :serial t
  :components ((:file "packages")
               (:file "meter")
               (:file "atomic-domain-counters")
               (:file "utilities")      ; sdl2 glue
               (:file "resources")      ; sdl2 memory
               ;; (file  "original-sdl-loop") ;jackdaniel's original inner loop logic
               (:file "time-utilities")
               (:file "scheduler")
               (:file "structured-main-loop") ;experimental frame-clock loop
               (:file "port")           ; sdl2 <-> clim
               (:file "pointer")        ; touch, mouse, cursors
               (:file "keyboard")       ; keybaord, input methods
               (:file "window")         ; mirrors
               (:file "plain-sheet")))  ; testing

;; (asdf:load-system :mcclim-sdl2 :force t)
