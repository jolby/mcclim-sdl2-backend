(in-package #:mcclim-sdl2)

;;; https://skryabiin.wordpress.com/2015/04/25/hello-world/
;;; http://osdl.sourceforge.net/main/documentation/rendering/SDL-openGL.html#twoDim

;;; http://osdl.sourceforge.net/main/documentation/rendering/SDL-threads.html

(defclass sdl2-medium (basic-medium) ())

(defmethod make-medium ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (make-instance 'sdl2-medium))


;;; https://stackoverflow.com/questions/6172020/opengl-rendering-in-a-secondary-thread
;;;
;;; (sdl2:gl-make-current window gl-context) and (sdl2:gl-swap-window window)
;;; are not thread safe.

(defclass sdl2-window (mirrored-sheet-mixin top-level-sheet-mixin basic-pane) ())

(defvar *sheet* nil)

(defun toggle-sheet ()
  (if *sheet*
      (progn
        (destroy-mirror (port *sheet*) *sheet*)
        (setf *sheet* nil))
      (let* ((port (find-port :server-path :sdl2))
             (sheet (make-instance 'sdl2-window
                                   :port port
                                   :region (make-bounding-rectangle 100 100 900 700)
                                   :pretty-name "My window")))
        (realize-mirror port sheet)
        (setf *sheet* sheet))))

(defclass sdl2-mirror ()
  ((window
    :initarg :window
    :reader window)
   (gl-context
    :initarg :gl-context
    :reader gl-context)))

(defmethod realize-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (window (sdl2:create-window
                    :title title :flags '(:shown :opengl) :x x :y y :w w :h h))
           (gl-context (sdl2:gl-create-context window)))
      (sdl2:gl-make-current window gl-context)
      (make-instance 'sdl2-mirror :window window :gl-context gl-context))))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-direct-mirror sheet)))
    (sdl2:gl-delete-context (gl-context mirror))
    (sdl2:destroy-window (window mirror))))




(defclass test-top-level-sheet
    (top-level-sheet-mixin mirrored-sheet-mixin basic-sheet)
  ())

(defun test-window ()
  (let* ((bbox (make-bounding-rectangle 1280 720  1680 1120))
         (port (find-port :server-path :sdl2))
         (sheet (make-instance 'test-top-level-sheet
                               :region bbox :port port
                               :icon climi::*default-icon-large*)))
    (realize-mirror port sheet)
    sheet))

(defparameter *sheet* (test-window))
(setf (sheet-pretty-name *sheet*) "close request")
(destroy-mirror (port *sheet*) *sheet*)
(defun do-it ()
  (port-set-mirror-name (port *sheet*) *sheet* "Hello world")
  (port-set-mirror-geometry
   (port *sheet*) *sheet*
   (make-rectangle* 100 100 600 600)))


(do-it)

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


;; (defvar *my-port* (find-port :server-path :sdl2))

;; (defvar *my-medium*
;;  (make-medium *my-port* *xxx*))

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

;; (sdl-draw-rect *xxx* +deep-pink+ 10 10 90 90)

;; (medium-draw-rectangle* *my-medium* 10 10 90 90 t)
;; (restart-port (find-port :server-path :sdl2))

