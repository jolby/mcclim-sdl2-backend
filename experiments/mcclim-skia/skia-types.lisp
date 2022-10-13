(in-package #:skia-core)

(define-enumval-extractor color-type-enum %skia:sk-color-type)
(define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(define-enumval-extractor clip-op-enum %skia:sk-clip-op)
;; :fill-style :stroke-style :stroke-and-fill-style
(define-enumval-extractor paint-style-enum %skia:sk-paint+style)
;; :miter-join :round-join :bevel-join :last-join :default-join
(define-enumval-extractor paint-join-enum %skia:sk-paint+join)
;; :butt-cap :round-cap :square-cap :last-cap :default-cap
(define-enumval-extractor paint-cap-enum %skia:sk-paint+cap)
(define-enumval-extractor path-add-mode-enum %skia::sk-path+add-mode)
(define-enumval-extractor path-arc-size-mode-enum %skia::sk-path+arc-size)
(define-enumval-extractor path-segment-mask-num %skia::sk-path-segment-mask)
;; :cw :ccw
(define-enumval-extractor path-direction-enum %skia::sk-path-direction)
(define-enumval-extractor path-first-direction-enum %skia::sk-path-first-direction)
(define-enumval-extractor path-fill-type-enum %skia::sk-path-fill-type)
(define-enumval-extractor path-convexity-enum %skia::sk-path-convexity)
(define-enumval-extractor path-verb-enum %skia::sk-path-verb)

(define-enumval-extractor skia-shadow-enum %skia::sk-shadow-flags)

(defun %unit-float->u8 (f-comp)
  (logand (truncate (* f-comp 255)) 255))

(defun color32-u8-values (color32)
  (values (logand 255 (ash color32 -24))
          (logand 255 (ash color32 -16))
          (logand 255 (ash color32 -8))
          (logand 255 color32)))

(defun color32-float-values (color32)
  (values (/ (logand 255 (ash color32 -24)) 255.0)
          (/ (logand 255 (ash color32 -16)) 255.0)
          (/ (logand 255 (ash color32 -8)) 255.0)
          (/ (logand 255 color32) 255.0)))

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

(defun make-color4f-from-color32rgba (color32rgba)
  (multiple-value-bind (red green blue alpha) (color32-float-values color32rgba)
    (make-color4f red green blue alpha)))

(defun destroy-color4f (color4f)
  (iffi:destroy-intricate-instance '%skia:sk-color4f color4f))

(defmacro with-color4f ((color-sym make-form) &body body)
  (alx:once-only (make-form)
    `(let ((,color-sym ,make-form))
       (unwind-protect
            (progn
              ,@body)
         (destroy-color4f ,color-sym)))))

(defun color4f-rgba-values (color4f)
  (iffi:with-intricate-slots
      %skia:sk-color4f ((cr %skia:f-r) (cg %skia:f-g)
                        (cb %skia:f-b) (ca %skia:f-a))
      color4f
    (values cr cg cb ca)))

(defun color4f-rgba-u8-values (color4f)
  (iffi:with-intricate-slots
      %skia:sk-color4f ((cr %skia:f-r) (cg %skia:f-g)
                        (cb %skia:f-b) (ca %skia:f-a))
      color4f
    (values (%unit-float->u8 cr) (%unit-float->u8 cg)
            (%unit-float->u8 cb) (%unit-float->u8 ca))))

(defun color4f->color32argb (color4f)
  (multiple-value-bind (red green blue alpha)
      (color4f-rgba-values color4f)
    (logior (ash (%unit-float->u8 alpha) 24)
            (ash (%unit-float->u8 red) 16)
            (ash (%unit-float->u8 green) 8)
            (ash (%unit-float->u8 blue) 0))))

(defun color4f->color32rgba (color4f)
  (multiple-value-bind (red green blue alpha)
      (color4f-rgba-values color4f)
    (logior (ash (%unit-float->u8 red) 24)
            (ash (%unit-float->u8 green) 16)
            (ash (%unit-float->u8 blue) 8)
            (ash (%unit-float->u8 alpha) 0))))

(comment
  (color32-u8-values #xef00ff80)
  (color32-float-values #xefccff80)
  (with-color4f (rc (make-color4f 1 0 0 .5))
    (color4f-rgba-values rc))
  (with-color4f (rc (make-color4f 1 0 0 .5))
    (color4f-rgba-u8-values rc))
  (with-color4f (rc (make-color4f 1 0 0 .5))
    (color4f->color32rgba rc))
  (with-color4f (rc (make-color4f 1 0 0 .5))
    (color4f->color32argb rc))
  (with-color4f (rc (make-color4f-from-color32rgba #xef00ff80))
    (color4f->color32rgba rc))
  )
;; Need to make an ergonomic make-XXX function that accepts:
;; 3 or 4 8bit uints (r g b (a))
;; 3 or 4 floats (r g b (a))
;; string, keyword or symbols 4 - 9 chars, starting with '#'
;; for hex-encoded 32argb representation
;; ... later also accept keywords for different colorspaces HSL etc...
;; (defun make-color (&rest args))
(defun %make-paint ()
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
  (with-color4f (color4f (make-color4f r g b a))
    (%skia:set-color
     '(claw-utils:claw-pointer %skia:sk-paint) paint
     '(claw-utils:claw-pointer %skia:sk-color4f) color4f
     '(claw-utils:claw-pointer %skia:sk-color-space) (cffi:null-pointer))))

(defun get-paint-color32argb (paint)
  (%skia:get-color :const '(:pointer %skia:sk-paint) paint))

(defun set-paint-color32argb (paint c)
  (%skia:set-color '(:pointer %skia::sk-paint) paint
                   '%skia::sk-color c))

(defun paint-anti-alias-p (paint)
  (%skia:is-anti-alias :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-anti-alias (paint &optional (antialias t))
  (%skia:set-anti-alias '(:pointer %skia::sk-paint) paint
                        :bool antialias))

(defun get-paint-style (paint)
  (%skia:get-style :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-style (paint style)
  (%skia:set-style '(:pointer %skia::sk-paint) paint
                   '%skia:sk-paint+style style))

;;XXX maybe get rid of this and just use set-paint-style
(defun set-paint-stroke (paint &optional (stroke t))
  (%skia:set-stroke '(:pointer %skia::sk-paint) paint
                    :bool stroke))

(defun paint-stroke-p (paint)
  (let ((style (get-paint-style paint)))
    (member style '(:stroke-style :stroke-and-fill-style) :test 'eq)))

(defun paint-fill-p (paint)
  (let ((style (get-paint-style paint)))
    (member style '(:fill-style :stroke-and-fill-style) :test 'eq)))

(defun paint-stroke-and-fill-p (paint)
  (eq :stroke-and-fill-style (get-paint-style paint)))

(defun get-paint-stroke-width (paint)
  (%skia:get-stroke-width :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-stroke-width (paint width)
  (%skia:set-stroke-width '(:pointer %skia::sk-paint) paint
                          '%skia:sk-scalar (float width 1f0)))

(defun get-paint-stroke-miter (paint)
  (%skia:get-stroke-miter :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-stroke-miter (paint miter)
  (%skia:set-stroke-miter '(:pointer %skia::sk-paint) paint
                          '%skia:sk-scalar (float miter 1f0)))

(defun get-paint-stroke-cap (paint)
  (%skia:get-stroke-cap :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-stroke-cap (paint stroke-cap)
  (%skia:set-stroke-cap '(:pointer %skia::sk-paint) paint
                   '%skia:sk-paint+cap stroke-cap))

(defun get-paint-stroke-join (paint)
  (%skia:get-stroke-join :const '(:pointer %skia::sk-paint) paint))

(defun set-paint-stroke-join (paint stroke-join)
  (%skia:set-stroke-join '(:pointer %skia::sk-paint) paint
                   '%skia:sk-paint+join stroke-join))

(defun paint-to-string (paint)
  (format nil "SkPaint (~s) color: #~x, style: ~s anti-alias: ~a, ~%width: ~a, miter: ~a, cap: ~s, join: ~s"
          paint (get-paint-color32argb paint) (get-paint-style paint) (paint-anti-alias-p paint)
          (get-paint-stroke-width paint) (get-paint-stroke-miter paint)
          (get-paint-stroke-cap paint) (get-paint-stroke-join paint)))

(defun make-paint (&key (color #xFF000000) (stroke-width 1) (style :fill-style))
  (let ((paint (iffi:make-intricate-instance '%skia:sk-paint)))
    (set-paint-color32argb paint color)
    (set-paint-stroke-width paint stroke-width)
    (set-paint-style paint style)
    paint))

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

(defun set-point3-xyz (point3 x y z)
  (iffi:with-intricate-slots
      %skia:sk-point3
      ((px %skia:f-x)
       (py %skia:f-y)
       (pz %skia:f-z))
      point3
    (setf px (float x 0f0)
          py (float y 0f0)
          pz (float z 0f0))))

(defun make-point3 (x y z)
  (let ((p (iffi:make-intricate-instance '%skia:sk-point3)))
    (set-point3-xyz p x y z)))

(defun destroy-point3 (p)
  (iffi:destroy-intricate-instance '%skia:sk-point3 p))

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

(defun make-typeface (font-data-ub8-array)
  (assert (or (subtypep (array-element-type font-data-ub8-array) '(unsigned-byte 8))
              (subtypep (array-element-type font-data-ub8-array) '(signed-byte 8))))
  (let ((typeface (iffi:intricate-alloc '%skia:sk-sp<sk-typeface>)))
    (skia-core::with-pinned-array-pointer (font-data-ptr font-data-ub8-array)
      (iffi:with-intricate-alloc (data %skia:sk-sp<sk-data>)
        (%skia:sk-data+make-with-copy
         '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data
         '(claw-utils:claw-pointer :void) font-data-ptr
         '%skia:size-t (length font-data-ub8-array))
        (unwind-protect
             (progn
               (%skia:sk-typeface+make-from-data
                '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface
                '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data
                :int 0))
          (%skia:~sk-sp
           '(claw-utils:claw-pointer %skia:sk-sp<sk-data>) data))))
    typeface))

(defun destroy-typeface (typeface)
  (%skia:~sk-sp
   '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface)
  (iffi:intricate-free typeface)
  (values))

(defun %typeface-family-name (typeface-ptr)
  (unless (cffi:null-pointer-p typeface-ptr)
    (iffi:with-intricate-instance (sk-str %skia:sk-string)
      (%skia:get-family-name
       :const
       '(claw-utils:claw-pointer %skia:sk-typeface) typeface-ptr
       '(claw-utils:claw-pointer %skia:sk-string) sk-str)
      (cffi:foreign-string-to-lisp
       (%skia:c-str :const '(claw-utils:claw-pointer %skia:sk-string) sk-str)
       :encoding :utf-8))))

(defun typeface-family-name (typeface)
  (%typeface-family-name (%skia:get
                          :const
                          '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface)))

(defun make-default-font ()
  (iffi:make-intricate-instance '%skia:sk-font))

(defun make-font (typeface)
  (iffi:make-intricate-instance
   '%skia:sk-font
   '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface))

(defun destroy-font (font)
  (iffi:destroy-intricate-instance '%skia:sk-font font))

(defun font-family-name (font)
  (%typeface-family-name
   (%skia:get-typeface-or-default
    :const
    '(claw-utils:claw-pointer %skia:sk-font) font)))

(defun font-size (font)
  (%skia:get-size
   :const
   '(claw-utils:claw-pointer %skia:sk-font) font))

(defun set-font-size (font size)
  (%skia:set-size
   '(claw-utils:claw-pointer %skia:sk-font) font
   '%skia:sk-scalar (float size 0f0)) )
