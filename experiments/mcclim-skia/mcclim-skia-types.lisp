(in-package #:mcclim-skia)


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
(defclass skia-opengl-medium (basic-medium)
  ;;XXX do we want skia canvas as a slot or as a special dynamic var? The
  ;;skia-canvas will need to be invalidated on screen resizes and maybe other
  ;;situations as well.
  ((skia-canvas :initarg :skia-canvas :accessor skia-canvas)
   (%paint-stack :initarg nil
                :initform (make-array 1 :fill-pointer 0 :adjustable t)
                :accessor medium-paint-stack)
   (%font-stack :initarg nil
                :initform (make-array 1 :fill-pointer 0 :adjustable t)
                :accessor medium-font-stack)
   ;; Maybe make this a deferred-drawing-mixin???
   (%deferred-command-queue :initarg nil
                           :initform (make-array 128 :fill-pointer 0 :adjustable t :initial-element nil)
     :accessor medium-deferred-command-queue)
   ))
