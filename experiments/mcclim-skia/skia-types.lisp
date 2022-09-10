(in-package #:skia-core)

(define-enumval-extractor color-type-enum %skia:sk-color-type)
(define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(define-enumval-extractor clip-op-enum %skia:sk-clip-op)
;; :fill-style :stroke-style :stroke-and-fill-style
(define-enumval-extractor paint-style-enum %skia:sk-paint+style)
;; :miter-join :round-join :bevel-join :last-join :default-join
(define-enumval-extractor paint-join-enum %skia:sk-paint+join)
;; :butt-cap :round-cap :square-cap :last-cap :default-cap
(define-enumval-extractor paint-join-cap %skia:sk-paint+cap)
(define-enumval-extractor path-add-mode-enum %skia::sk-path+add-mode)
(define-enumval-extractor path-arc-size-mode-enum %skia::sk-path+arc-size)
(define-enumval-extractor path-segment-mask-num %skia::sk-path-segment-mask)
;; :cw :ccw
(define-enumval-extractor path-direction-enum %skia::sk-path-direction)
(define-enumval-extractor path-first-direction-enum %skia::sk-path-first-direction)
(define-enumval-extractor path-fill-type-enum %skia::sk-path-fill-type)
(define-enumval-extractor path-convexity-enum %skia::sk-path-convexity)
(define-enumval-extractor path-verb-enum %skia::sk-path-verb)

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

;; Need to make an ergonomic make-XXX function that accepts:
;; 3 or 4 8bit uints (r g b (a))
;; 3 or 4 floats (r g b (a))
;; string, keyword or symbols 4 - 9 chars, starting with '#'
;; for hex-encoded 32argb representation
;; ... later also accept keywords for different colorspaces HSL etc...
;; (defun make-color (&rest args))

(defun make-paint ()
  (iffi:make-intricate-instance '%skia:sk-paint))

(defun destroy-paint (paint)
  (iffi:destroy-intricate-instance '%skia:sk-paint paint))

(defmacro with-paint ((paint-sym) &body body)
  `(iffi:with-intricate-instance (,paint-sym '%skia:sk-paint)
    ,@body))

(defun get-paint-color4f (paint)
  (iffi:with-intricate-instance (color4f %skia:sk-color4f)
    (iffi:with-intricate-slots
        %skia:sk-color4f ((cr %skia:f-r) (cg %skia:f-g)
                          (cb %skia:f-b) (ca %skia:f-a))
        (%skia:get-color4f :const
                           '(:pointer %skia:sk-color4f) color4f
                           '(:pointer %skia:sk-paint) paint)
      (values cr cg cb ca))))

(defun set-paint-color4f (paint r g b a)
  (with-color4f (c4f r g b a)
    (%skia:set-color
     '(claw-utils:claw-pointer %skia:sk-paint) paint
     '(claw-utils:claw-pointer %skia:sk-color4f) c4f
     '(claw-utils:claw-pointer %skia:sk-color-space) (cffi:null-pointer))))

(defun get-paint-color32argb (paint)
  (%skia:get-color :const '(:pointer %skia:sk-paint) paint))

(defun set-paint-color32argb (paint c)
  (%skia:set-color '(:pointer %skia::sk-paint) paint
                   '%skia::sk-color c))


(defun set-paint-anti-alias (paint &optional (antialias t))
  (%skia:set-anti-alias '(:pointer %skia::sk-paint) paint
                        :bool antialias))

(defun set-paint-stroke (paint &optional (stroke t))
  (%skia:set-stroke '(:pointer %skia::sk-paint) paint
                    :bool stroke))

(defun set-paint-stroke-width (paint width)
  (%skia:set-stroke-width '(:pointer %skia::sk-paint) paint
                          '%skia:sk-scalar (float width 1f0)))

(defun set-paint-style (paint style)
  (%skia:set-style '(:pointer %skia::sk-paint) paint
                   '%skia:sk-paint+style style))

;;
;; Geometric objects
;;
;;
(defun set-point-xy (point x y)
  (iffi:with-intricate-slots
      %skia:sk-point
      ((px %skia:f-x)
       (py %skia:f-y))
      point
    (setf px (float x 0f0)
          py (float y 0f0))))

(defun make-point (x y)
  (let ((p (iffi:make-intricate-instance '%skia:sk-point)))
    (set-point-xy p x y)))

(defun destroy-point (p)
  (iffi:destroy-intricate-instance '%skia:sk-point p))

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

(defun make-oval (x y w h)
  (with-rectangle (rect x y w h)
    (let ((rrect (iffi:make-intricate-instance '%skia:sk-r-rect)))
      (%skia:sk-r-rect+make-oval
       '(:pointer %skia:sk-r-rect) rrect
       '(:pointer %skia:sk-rect) rect)
      rrect)))

(defun make-circle (x y diameter)
  (make-oval x y diameter diameter))

(defun destroy-rectangle (rect)
  (iffi:destroy-intricate-instance '%skia:sk-rect rect))

(defmacro with-rectangle ((rect-sym x y w h) &body body)
  `(let ((,rect-sym (make-rectangle ,x ,y ,w ,h)))
     (unwind-protect
          (progn ,@body)
       (destroy-rectangle ,rect-sym))))

(defmacro with-oval ((oval-sym x y w h) &body body)
  `(let ((,oval-sym (make-oval ,x ,y ,w ,h)))
     (unwind-protect
          (progn ,@body)
       (destroy-rectangle ,oval-sym))))

(defmacro with-circle ((circle-sym x y diameter) &body body)
  `(let ((,circle-sym (make-oval ,x ,y ,diameter)))
     (unwind-protect
          (progn ,@body)
       (destroy-rectangle ,circle-sym))))

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

(defun make-rounded-rectangle (x y w h x-rad y-rad)
  (with-rectangle (rect x y w h)
    (let ((rrect (iffi:make-intricate-instance '%skia:sk-r-rect)))
      (%skia:sk-r-rect+make-rect-xy
       '(:pointer %skia:sk-r-rect) rrect
       '(:pointer %skia:sk-rect) rect
       '%skia:sk-scalar (float x-rad 0f0)
       '%skia:sk-scalar (float y-rad 0f0))
      rrect)))

;;XXX maybe just use rr with zero xy rads?
(defun make-rounded-rectangle-zero-radius (x y w h)
  (with-rectangle (rect x y w h)
    (let ((rrect (iffi:make-intricate-instance '%skia:sk-r-rect)))
      (%skia:sk-r-rect+make-rect
       '(:pointer %skia:sk-r-rect) rrect
       '(:pointer %skia:sk-rect) rect)
      rrect)))

(defun destroy-rounded-rectangle (rrect)
  (iffi:destroy-intricate-instance '%skia:sk-r-rect rrect))

(defmacro with-rounded-rectangle ((rrect-sym x y w h x-rad y-rad) &body body)
  `(let ((,rrect-sym (make-rounded-rectangle ,x ,y ,w ,h ,x-rad ,y-rad)))
     (unwind-protect
          (progn ,@body)
       (destroy-rounded-rectangle ,rrect-sym))))

(defun make-path ()
  (iffi:make-intricate-instance '%skia:sk-path))

(defun destroy-path (path)
 (iffi:destroy-intricate-instance '%skia:sk-path path))

(defmacro with-path ((path-sym) &body body)
  `(let ((,path-sym (make-path)))
     (unwind-protect
          (progn ,@body)
       (destroy-path ,path-sym))))

(defun path-move-to (path x y)
  (%skia:move-to '(:pointer %skia:sk-path) path
                 '%skia:sk-scalar (float x 0f0)
                 '%skia:sk-scalar (float y 0f0)))

(defun path-line-to (path x1 y1)
  (%skia:line-to '(:pointer %skia:sk-path) path
                 '%skia:sk-scalar (float x1 0f0)
                 '%skia:sk-scalar (float y1 0f0)))

(defun path-circle (center-x center-y radius
                    &key (path-direction :cw) (append-to nil))
  (%skia:sk-path+circle
   '(:pointer %skia:sk-path) (or append-to (make-path))
   '%skia:sk-scalar (float center-x 0f0)
   '%skia:sk-scalar (float center-y 0f0)
   '%skia:sk-scalar (float radius 0f0)
   '%skia:sk-path-direction path-direction))

(defun path-close (path)
  (%skia:close '(:pointer %skia:sk-path) path))
