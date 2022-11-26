(in-package #:mcclim-skia)

(defclass sdl2-skia-port (mcclim-sdl2::sdl2-port) ())

(defclass sdl2-skia-ttf-port (mcclim-truetype:ttf-port-mixin sdl2-skia-port)
  ((%fontpath->skia-typeface :initform (make-hash-table :test #'equal) :accessor fontpath->skia-typeface)))

(defmethod find-port-type ((port (eql :sdl2-skia)))
  (values 'sdl2-skia-port (constantly nil)))

(defmethod find-port-type ((port (eql :sdl2-skia-ttf)))
  (values 'sdl2-skia-ttf-port (constantly nil)))

;;;;
;;;; SDL2/OpenGL Mirrors/Sheets/Windows
;;;;
(defclass sdl2-opengl-mirror (mcclim-sdl2::mirror-with-sheet-mixin
                              mcclim-sdl2::sdl2-window-handle-mixin)
  ((window :initarg :window :accessor window)
   (gl-context :initarg :gl-context :accessor gl-context)))

(defclass opengl-mirrored-sheet-mixin (mirrored-sheet-mixin) ())

(defclass sdl2-opengl-top-level-sheet
    (top-level-sheet-mixin opengl-mirrored-sheet-mixin basic-sheet)
  ())

(defclass sdl2-opengl-window (sdl2-opengl-top-level-sheet basic-pane) ())


;;;;
;;;; Skia Mirrors/Sheets/Windows
;;;;
(defclass sdl2-opengl-skia-mirror (sdl2-opengl-mirror)
  ((skia-context
    :initarg :skia-context
    :accessor skia-context)))

(defclass skia-mirrored-sheet-mixin (mirrored-sheet-mixin) ())

(defclass sdl2-skia-top-level-sheet
    (top-level-sheet-mixin skia-mirrored-sheet-mixin basic-sheet)
  ())

(defclass sdl2-skia-window (sdl2-skia-top-level-sheet basic-pane) ())


;;;;
;;;;
;;;; Skia Medium
;;;;
(defclass skia-opengl-medium (basic-medium mcclim-truetype::ttf-medium-mixin)
  ;;XXX do we want skia canvas as a slot or as a special dynamic var? The
  ;;skia-canvas will need to be invalidated on screen resizes and maybe other
  ;;situations as well.
  ((%canvas :initarg :skia-canvas :accessor medium-skia-canvas)
   (%paint :initform nil :initarg :skia-paint :accessor medium-skia-paint)
   (%font :initform nil :initarg :skia-font :accessor medium-skia-font)
   (%font-paint :initform nil :initarg :skia-font :accessor medium-skia-font-paint)
      ))
