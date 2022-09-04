(in-package #:mcclim-skia)

(defclass skia-medium (basic-medium) ())

(defmethod make-medium ((port mcclim-sdl2::sdl2-port) (sheet skia-mirrored-sheet-mixin))
  (make-instance 'skia-medium))
