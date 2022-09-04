(in-package #:mcclim-skia)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

(defvar *skia-port* nil)
(defvar *skia-window* nil)
(defvar *skia-top-level-sheet* nil)
(defvar *skia-medium* nil)

(defun toggle-sheet ()
  (if *skia-top-level-sheet*
      (progn
        (destroy-mirror (port *skia-top-level-sheet*) *skia-top-level-sheet*)
        (setf *skia-top-level-sheet* nil))
      (let* ((port (find-port :server-path :sdl2))
             (sk-tls (make-instance 'sdl2-skia-top-level-sheet
                                   :port port
                                   :region (make-bounding-rectangle 100 100 900 700)
                                   :pretty-name "My Skia Top Level Sheet")))
        (realize-mirror port sk-tls)
        (setf *skia-top-level-sheet* sk-tls))))

(defun make-test-window ()
  (let* ((bbox (make-bounding-rectangle 1280 720  1680 1120))
         (port (find-port :server-path :sdl2))
         (win (make-instance 'sdl2-skia-window
                               :region bbox :port port
                               :icon climi::*default-icon-large*)))
    (realize-mirror port win)
    win))

(defun do-test-skia-window ()
  (port-set-mirror-name (port *skia-window*) *skia-window* "Hello world - Skia Window")
  (port-set-mirror-geometry
   (port *skia-window*) *skia-window*
   (make-rectangle* 100 100 600 600)))

(defun create-surface (sheet width height)
  (declare (ignore sheet))
  (sdl2-ffi.functions:sdl-create-rgb-surface
   0 width height 32 #x00ff0000 #x0000ff00 #x000000ff #xff000000))

(defun destroy-surface (surface)
  (sdl2-ffi.functions:sdl-free-surface surface))

(defun blit-surface (source target)
  (log:info "blitting brrt")
  (sdl2-ffi.functions:sdl-upper-blit
   source nil target (sdl2:make-rect 0 0 100 100)))

(defun update-surface (surface pattern)
  (let ((color (sdl2:map-rgb
                (sdl2:surface-format surface) #x44 #x44 #x88)))
    (sdl2:fill-rect surface nil color)
    (sdl2:update-window window)))


(defun sdl-color (surface ink)
  (multiple-value-bind (r g b) (color-rgb ink)
    (sdl2:map-rgb (sdl2:surface-format surface)
                  (truncate (* r 255))
                  (truncate (* g 255))
                  (truncate (* b 255)))))

(defun sdl-draw-rect (sheet ink x1 y1 x2 y2)
  (alx:when-let ((window (sdl2-window (sheet-direct-mirror sheet))))
    (let* ((surface (sdl2:get-window-surface window))
           (color (sdl-color surface ink))
           (rect (sdl2:make-rect x1 y1 (- x2 x1) (- y2 y1))))
      (sdl2:fill-rect surface rect color)
      (sdl2:update-window window))))


(comment
  (defvar *skia-port* (find-port :server-path :sdl2))

  (defvar *skia-medium*
    (make-medium *skia-port* *xxx*))


  (sdl-draw-rect *xxx* +deep-pink+ 10 10 90 90)
  (medium-draw-rectangle* *skia-medium* 10 10 90 90 t)
  (restart-port (find-port :server-path :sdl2))

  (setf *skia-window* (make-test-window))
  (setf (sheet-pretty-name *skia-window*) "close request")
  (destroy-mirror (port *skia-window*) *skia-window*)


  (do-test-skia-window)
  )
