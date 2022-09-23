(in-package #:clim-internals)

(defclass color-with-alpha (clim-internals::standard-color)
  ((alpha :initarg :alpha :initform 1 :type (real 0 1))))

(defmethod color-rgba ((color color-with-alpha))
  (with-slots (red green blue alpha) color
      (values red green blue alpha)))

(defun make-rgba-color (red green blue &optional (alpha 1.0))
  (make-instance 'color-with-alpha :red red :green green :blue blue :alpha alpha))
