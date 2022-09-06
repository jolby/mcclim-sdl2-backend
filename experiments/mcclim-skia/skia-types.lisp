(in-package #:skia-core)

(define-enumval-extractor color-type-enum %skia:sk-color-type)
(define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(define-enumval-extractor clip-op-enum %skia:sk-clip-op)
(define-enumval-extractor paint-style-enum %skia:sk-paint+style)

(defun set-color4f (color4f r g b a)
  (iffi:with-intricate-slots
      %skia:sk-color4f ((cr %skia:f-r) (cg %skia:f-g)
                        (cb %skia:f-b) (ca %skia:f-a))
      color4f
    (setf cr (float r 0f0) cg (float g 0f0)
          cb (float b 0f0) ca (float a 0f0)))
  color4f)

(defun make-color4f (r g b a)
  (let ((color (iffi:make-intricate-instance '%skia:sk-color4f)))
    (set-color4f color r g b a)))

(defun destroy-color4f (color4f)
  (iffi:destroy-intricate-instance '%skia:sk-color4f color4f))

(defmacro with-color4f ((color-sym r g b a) &body body)
  `(iffi:with-intricate-instance (,color-sym %skia:sk-color4f)
     (set-color4f ,color-sym ,r ,g ,b ,a)
     ,@body))

(defun make-paint ()
  (iffi:make-intricate-instance '%skia:sk-paint))

(defun destroy-paint (paint)
  (iffi:destroy-intricate-instance '%skia:sk-paint paint))

(defmacro with-paint ((paint-sym) &body body)
  `(iffi:with-intricate-instance (,paint-sym '%skia:sk-paint)
    ,@body))

(defun set-paint-color4f (paint r g b a)
  (with-color4f (c4f r g b a)
    (%skia:set-color
     '(claw-utils:claw-pointer %skia:sk-paint) paint
     '(claw-utils:claw-pointer %skia:sk-color4f) c4f
     '(claw-utils:claw-pointer %skia:sk-color-space) (cffi:null-pointer))))

(defun set-paint-color32argb (paint c)
  (%skia:set-color '(:pointer %skia::sk-paint) paint
                   '%skia::sk-color c))

(defun set-rectangle-xywh (rect x y w h)
  (%skia:set-xywh
   '(:pointer %skia:sk-rect) rect
   '%skia:sk-scalar (float x 0f0) '%skia:sk-scalar (float y 0f0)
   '%skia:sk-scalar (float w 0f0) '%skia:sk-scalar (float h 0f0)))

(defun make-rectangle (x y w h)
  (let ((rect (iffi:make-intricate-instance '%skia:sk-rect)))
    (%skia:sk-rect+make-xywh
     '(:pointer %skia:sk-rect) rect
     '%skia:sk-scalar (float x 0f0) '%skia:sk-scalar (float y 0f0)
     '%skia:sk-scalar (float w 0f0) '%skia:sk-scalar (float h 0f0))))

(defun destroy-rectangle (rect)
  (iffi:destroy-intricate-instance '%skia:sk-rect rect))

(defmacro with-rectangle ((rect-sym x y w h) &body body)
  `(let ((,rect-sym (make-rectangle ,x ,y ,w ,h)))
     (unwind-protect
          (progn ,@body)
       (destroy-rectangle ,rect-sym))))

(defun set-i-rectangle-xywh (irect x y w h)
  (%skia:set-xywh
   '(:pointer %skia:sk-i-rect) irect
   '%skia:int32-t (floor x) '%skia:int32-t (floor y)
   '%skia:int32-t (floor w) '%skia:int32-t (floor h)))

(defun make-i-rectangle (x y w h)
  (let ((irect (iffi:make-intricate-instance '%skia:sk-i-rect)))
    (%skia:sk-i-rect+make-xywh
     '(:pointer %skia:sk-i-rect) irect
     '%skia:int32-t (floor x) '%skia:int32-t (floor y)
     '%skia:int32-t (floor w) '%skia:int32-t (floor h) )))

(defun destroy-i-rectangle (irect)
  (iffi:destroy-intricate-instance '%skia:sk-i-rect irect))

(defmacro with-i-rectangle ((irect-sym x y w h) &body body)
  `(let ((,irect-sym (make-i-rectangle ,x ,y ,w ,h)))
     (unwind-protect
          (progn ,@body)
       (destroy-i-rectangle ,irect-sym))))

(defun make-rounded-rectangle (x y w h rad)
  (with-rectangle (rect x y w h)
    (let ((rrect (iffi:make-intricate-instance '%skia:sk-r-rect))))))
