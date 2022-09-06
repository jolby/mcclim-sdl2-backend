(in-package #:skia-canvas)

;; Note: almost all of this is pulled straight from the alien-works project
;; created by Pavel Korlev (MIT license)
(declaim (special *canvas*
                  *paint*
                  *font*))


;;;
;;; Canvas maintenance
;;;
(defun clear-canvas (&optional (canvas *canvas*))
  (%skia:clear '(:pointer %skia:sk-canvas) (%canvas-handle canvas)
               '%skia:sk-color %skia:+sk-color-transparent+))

(defun discard-canvas (&optional (canvas *canvas*))
  (%skia:discard '(:pointer %skia:sk-canvas) (%canvas-handle canvas)))

(defun flush-canvas (&optional (canvas *canvas*))
  (%skia:flush '(:pointer %skia:sk-canvas) (%canvas-handle canvas)))

(defun update-canvas-clip (canvas x y width height &key (mode :intersect))
  (iffi:with-intricate-instance (irect %skia:sk-i-rect)
    (%skia:set-xywh
     '(claw-utils:claw-pointer %skia:sk-i-rect) irect
     '%skia:int32-t (floor x)
     '%skia:int32-t (floor y)
     '%skia:int32-t (floor width)
     '%skia:int32-t (floor height))
    (%skia:clip-i-rect
     '(claw-utils:claw-pointer %skia:sk-canvas) canvas
     '(claw-utils:claw-pointer %skia:sk-i-rect) irect
     '%skia:sk-clip-op mode))
  canvas)

;;;
;;; DRAWING
;;;

(defun paint-color (r g b a)
  (set-paint-color *paint* r g b a))



(defun rectangle (x y width height)
  (iffi:with-intricate-alloc (rect %skia:sk-rect)
    (%skia:sk-rect+make-xywh
     '(:pointer %skia:sk-rect) rect
     '%skia:sk-scalar (float x 0f0)
     '%skia:sk-scalar (float y 0f0)
     '%skia:sk-scalar (float width 0f0)
     '%skia:sk-scalar (float height 0f0))
    (%skia:draw-rect
     '(:pointer %skia:sk-canvas) (%canvas-handle *canvas*)
     '(:pointer %skia:sk-rect) rect
     '(:pointer %skia:sk-paint) *paint*)))

(defun circle (x y radius)
  (%skia:draw-circle
   '(:pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)
   '%skia:sk-scalar (float radius 0f0)
   '(:pointer %skia:sk-paint) *paint*))

(defun simple-text (text x y)
  (when *font*
    (cffi:with-foreign-string ((ftext byte-size) text :encoding :utf-8)
      (%skia:draw-simple-text
       '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
       '(claw-utils:claw-pointer :void) ftext
       '%skia:size-t (1- byte-size)
       '%skia:sk-text-encoding :utf8
       '%skia:sk-scalar (float x 0f0)
       '%skia:sk-scalar (float y 0f0)
       '(claw-utils:claw-pointer %skia:sk-font) *font*
       '(claw-utils:claw-pointer %skia:sk-paint) *paint*))))


;;;
;;; Transforms
;;;
(defun save-transform ()
  (%skia:save '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)))

(defun restore-transform ()
  (%skia:restore '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)))

(defun reset-transform ()
  (%skia:restore-to-count
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   :int 1))

(defun translate (x y)
  (%skia:translate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

(defun rotate (degrees)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float degrees 0f0)))

(defun rotate-around (x y degrees)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float degrees 0f0)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

(defun scale (x y)
  (%skia:scale
   '(claw-utils:claw-pointer %skia:sk-canvas) (%canvas-handle *canvas*)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))


;;;
;;; FONTS
;;;
(defun font-size (size)
  (when *font*
    (%skia:set-size
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     '%skia:sk-scalar (float size 0f0))))

(defun font-baseline-snap (snapped)
  (when *font*
    (%skia:set-baseline-snap
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     :bool (and snapped t))))

(defun font-edging (mode)
  (when *font*
    (%skia:set-edging
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     '%skia::sk-font+edging mode)))

(defun font-subpixel (subpixeled)
  (when *font*
    (%skia:set-subpixel
     '(claw-utils:claw-pointer %skia:sk-font) *font*
     :bool (and subpixeled t))))

(defun make-typeface (font-data-ub8-array)
  (assert (or (subtypep (array-element-type font-data-ub8-array) '(unsigned-byte 8))
              (subtypep (array-element-type font-data-ub8-array) '(signed-byte 8))))
  (let ((typeface (iffi:intricate-alloc '%skia:sk-sp<sk-typeface>)))
    (u:with-pinned-array-pointer (font-data-ptr font-data-ub8-array)
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
  (iffi:make-intricate-instance '%skia:sk-font
                                '(claw-utils:claw-pointer %skia:sk-sp<sk-typeface>) typeface))


(defun destroy-font (font)
  (iffi:destroy-intricate-instance '%skia:sk-font font))


(defun font-family-name (font)
  (%typeface-family-name
   (%skia:get-typeface-or-default :const '(claw-utils:claw-pointer %skia:sk-font) font)))
