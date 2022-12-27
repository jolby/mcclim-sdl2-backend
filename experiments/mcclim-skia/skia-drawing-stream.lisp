(in-package #:mcclim-skia)

(defvar *last-drawing-stream* nil)
(defvar *last-top-level* nil)

(defun %draw-commands (stream)
  (surrounding-output-with-border
      (stream :background +white+ :filled t)
    (log:info "DRAW-STREAM current-thread: ~a" (bt:current-thread))

    (draw-rectangle* stream 100 100 100 100 :ink +blue+)
    (draw-rectangle* stream 25 25 75 75 :ink +red+)
    (draw-circle* stream 0 0 100 :ink
                  (compose-in +green+ (make-opacity .5)))
    (terpri stream)
    (log:info "Done drawing...")
    (setf *last-drawing-stream* stream)))

(mcclim-sdl2::define-sdl2-request do-draw-commands (stream)
  (log:info "DO-DRAW-COMMANDS current-thread: ~a" (bt:current-thread))
  (%draw-commands stream))

(defun draw-stream ()
  (setf *last-drawing-stream* nil
        *last-top-level* nil)

  (let ((new-top-level
          (with-output-to-drawing-stream (stream :sdl2-skia-ttf nil
                                                 :width 800
                                                 :height 800
                                                 ;; :orientation :graphics
                                                 ;; :units :device
                                                 ;; :scale-to-fit t
                                                 )
            (do-draw-commands stream :synchronize t)
            )))
    (setf *last-top-level* new-top-level)
    ))

(mcclim-sdl2::define-sdl2-request do-draw-stream ()
  (log:info "DO-DRAW-STREAM current-thread: ~a" (bt:current-thread))
  (draw-stream))

;; (draw-stream)
;; (do-draw-stream :synchronize t)
;; (find-port :server-path :sdl2-skia-ttf)
;; (destroy-port (find-port :server-path :sdl2-skia-ttf))
;; (mcclim-sdl2::%init-sdl2)
;; (mcclim-sdl2::%quit-sdl2)
