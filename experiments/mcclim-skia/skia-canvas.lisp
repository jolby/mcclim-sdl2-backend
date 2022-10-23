(in-package #:skia-canvas)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

;; Note: A lot of this code is from the alien-works project
;; created by Pavel Korlev (MIT license)
(declaim (special *canvas*
                  *paint*
                  *font*))

(defvar *default-typeface* nil)

;;;
;;; Canvas maintenance
;;;
(defun clear-canvas (&optional (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:clear '(:pointer %skia:sk-canvas) canvas
               '%skia:sk-color %skia:+sk-color-transparent+))

(defun clear-canvas-to-white (&optional (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:clear '(:pointer %skia:sk-canvas) canvas
               '%skia:sk-color %skia:+sk-color-white+))

(defun discard-canvas (&optional (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:discard '(:pointer %skia:sk-canvas) canvas))

(defun flush-canvas (&optional (canvas *canvas*))
  (%skia:flush '(:pointer %skia:sk-canvas) canvas))

(defun update-canvas-clip (canvas x y width height &key (mode :intersect))
  (check-nil-or-nullptr canvas)
  (skia-core::with-i-rectangle (irect x y width height)
    (%skia:clip-i-rect
     '(claw-utils:claw-pointer %skia:sk-canvas) canvas
     '(claw-utils:claw-pointer %skia:sk-i-rect) irect
     '%skia:sk-clip-op mode))
  canvas)

;;;
;;; DRAWING
;;;
(defun paint-color (r g b a
                    &key (paint *paint*))
  (check-nil-or-nullptr paint)
  (skia-core::set-paint-color4f paint r g b a))

(defun paint-color32argb (c
                          &key (paint *paint*))
  (check-nil-or-nullptr paint)
  (skia-core::set-paint-color32argb paint c))

(defun point (x y
              &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (%skia:draw-point
   '(:pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)
   '(:pointer %skia:sk-paint) paint))

(defun line (x1 y1 x2 y2
             &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (%skia:draw-line
   '(:pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float x1 0f0)
   '%skia:sk-scalar (float y1 0f0)
   '%skia:sk-scalar (float x2 0f0)
   '%skia:sk-scalar (float y2 0f0)
   '(:pointer %skia:sk-paint) paint))

(defun arc (x y width height start-angle sweep-angle use-center
            &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (skia-core::with-rectangle (rect x y width height)
    (%skia:draw-arc
     '(:pointer %skia:sk-canvas) canvas
     '(:pointer %skia:sk-rect) rect
     '%skia:sk-scalar (float start-angle 0f0)
     '%skia:sk-scalar (float sweep-angle 0f0)
     :bool use-center
     '(:pointer %skia:sk-paint) paint)))

(defun rectangle (x y width height
                  &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (skia-core::with-rectangle (rect x y width height)
    (%skia:draw-rect
     '(:pointer %skia:sk-canvas) canvas
     '(:pointer %skia:sk-rect) rect
     '(:pointer %skia:sk-paint) paint)))

(defun rounded-rectangle (x y width height radx rady
                          &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (skia-core::with-rectangle (rect x y width height)
    (%skia:draw-round-rect
     '(:pointer %skia:sk-canvas) canvas
     '(:pointer %skia:sk-rect) rect
     '%skia:sk-scalar (float radx 0f0)
     '%skia:sk-scalar (float rady 0f0)
     '(:pointer %skia:sk-paint) paint)))

(defun circle (x y radius
               &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (%skia:draw-circle
   '(:pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)
   '%skia:sk-scalar (float radius 0f0)
   '(:pointer %skia:sk-paint) paint))

(defun oval (x y width height
             &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (skia-core::with-rectangle (rect x y width height)
    (%skia:draw-oval
     '(:pointer %skia:sk-canvas) canvas
     '(:pointer %skia:sk-rect) rect
     '(:pointer %skia:sk-paint) paint)))

(defun path (path
             &key (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  ;; (log:info "canvas: ~a, path: ~a, paint: ~a" canvas path (skia-core::paint-to-string paint))
  (%skia:draw-path
   '(:pointer %skia:sk-canvas) canvas
   '(:pointer %skia:sk-path) path
   '(:pointer %skia:sk-paint) paint))

(defun polygon (coord-seq
                &key (closed t) (filled t)
                  (canvas *canvas*) (paint *paint*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (skia-core::with-path (poly-path)
    (let* ((x (first coord-seq))
           (y (second coord-seq)))
      (skia-core::path-move-to poly-path x y)
      (loop :for (x y) :on (cddr coord-seq) :by #'cddr
            :do (skia-core::path-line-to poly-path x y))
      (when closed (skia-core::path-close poly-path))
      (when filled (skia-core::set-paint-style paint :fill-style))
      (path poly-path :canvas canvas :paint paint))))

(defun simple-text (text x y
                    &key (canvas *canvas*) (paint *paint*) (font *font*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (check-nil-or-nullptr font)
    (cffi:with-foreign-string ((ftext byte-size) text :encoding :utf-8)
      (%skia:draw-simple-text
       '(claw-utils:claw-pointer %skia:sk-canvas) canvas
       '(claw-utils:claw-pointer :void) ftext
       '%skia:size-t (1- byte-size)
       '%skia:sk-text-encoding :utf8
       '%skia:sk-scalar (float x 0f0)
       '%skia:sk-scalar (float y 0f0)
       '(claw-utils:claw-pointer %skia:sk-font) font
       '(claw-utils:claw-pointer %skia:sk-paint) paint)))

;;;
;;; Transforms
;;;
(defun save-transform (&key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:save '(claw-utils:claw-pointer %skia:sk-canvas) canvas))

(defun restore-transform (&key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:restore '(claw-utils:claw-pointer %skia:sk-canvas) canvas))

(defun reset-transform (&key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:restore-to-count
   '(claw-utils:claw-pointer %skia:sk-canvas) canvas
   :int 1))

(defun translate (x y
                  &key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:translate
   '(claw-utils:claw-pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

(defun rotate (degrees
               &key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float degrees 0f0)))

(defun rotate-around (x y degrees
                      &key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:rotate
   '(claw-utils:claw-pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float degrees 0f0)
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

(defun scale (x y
              &key (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (%skia:scale
   '(claw-utils:claw-pointer %skia:sk-canvas) canvas
   '%skia:sk-scalar (float x 0f0)
   '%skia:sk-scalar (float y 0f0)))

;;;
;;; FONTS
;;;
(defun get-typeface (family face)
  (declare (ignorable family face))
  ;;XXX TODO really do typeface lookup
  *default-typeface*
  )

(defun font-size (size &key (font *font*))
  (check-nil-or-nullptr font)
  (%skia:set-size
   '(claw-utils:claw-pointer %skia:sk-font) font
   '%skia:sk-scalar (float size 0f0)))

(defun font-baseline-snap (snapped &key (font *font*))
  (check-nil-or-nullptr font)
    (%skia:set-baseline-snap
     '(claw-utils:claw-pointer %skia:sk-font) font
     :bool (and snapped t)))

(defun font-edging (mode &key (font *font*))
  (check-nil-or-nullptr font)
  (when *font*
    (%skia:set-edging
     '(claw-utils:claw-pointer %skia:sk-font) font
     '%skia::sk-font+edging mode)))

(defun font-subpixel (subpixeled &key (font *font*))
  (check-nil-or-nullptr font)
    (%skia:set-subpixel
     '(claw-utils:claw-pointer %skia:sk-font) font
     :bool (and subpixeled t)))

;;;
;;; Material Ops
;;;
(defun draw-shadowed-path (path paint z-plane
                           &key
                             ;;Maybe don't take light-pos param? Could be source of leak
                             ;;if caller provides and doesn't clean up
                             (light-pos (skia-core::make-point3 0 -700 700) light-pos-p)
                             (light-radius 1.1)
                             (ambient-alpha 0.05)
                             (spot-alpha 0.35)
                             (shadow-flags 4) ;;directional light
                             (canvas *canvas*))
  (check-nil-or-nullptr canvas)
  (check-nil-or-nullptr paint)
  (check-nil-or-nullptr path)
  (let ((z-plane3 (skia-core::make-point3 0 0 z-plane))
        (light-pos3 (if light-pos-p light-pos (skia-core::make-point3 0 -700 700)))
        (ambient-color (skia-core::with-color4f (amb-c (skia-core::make-color4f 0 0 0 ambient-alpha))
                         (skia-core::color4f->color32argb amb-c)))
        (spot-color (skia-core::with-color4f (spot-c (skia-core::make-color4f 0 0 0 spot-alpha))
                         (skia-core::color4f->color32argb spot-c))))
    (unwind-protect
         (progn
           (%skia::sk-shadow-utils+draw-shadow '(claw-utils:claw-pointer %skia::sk-canvas) canvas
                                               '(claw-utils:claw-pointer %skia::sk-path) path
                                               '(claw-utils:claw-pointer %skia::sk-point3) z-plane3
                                               '(claw-utils:claw-pointer %skia::sk-point3) light-pos3
                                               '%skia::sk-scalar light-radius
                                               '%skia::sk-color ambient-color
                                               '%skia::sk-color spot-color
                                               '%skia::uint32-t shadow-flags)
           (path path :paint paint :canvas canvas))
      (skia-core::destroy-point3 z-plane3)
      (unless light-pos-p (skia-core::destroy-point3 light-pos3)))))

