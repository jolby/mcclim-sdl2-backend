(in-package #:mcclim-skia)

(defclass sdl2-medium (basic-medium) ())

(defmethod make-medium ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (make-instance 'sdl2-medium))
