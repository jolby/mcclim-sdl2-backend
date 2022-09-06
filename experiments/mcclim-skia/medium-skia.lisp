(in-package #:mcclim-skia)

(defun round-coordinate (x)
  (floor (+ x .5)))

(defclass skia-opengl-medium (basic-medium)
  ;;XXX do we want skia canvas as a slot or as a special dynamic var? The
  ;;skia-canvas will need to be invalidated on screen resizes and maybe other
  ;;situations as well.
  ((skia-canvas :initarg :skia-canvas :accessor skia-canvas)))

(defmethod make-medium ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  ;; XXX this doesn't work. The mirror hasn't been realized yet
  ;; (let* ((mirror (sheet-mirrored-ancestor sheet))
  ;;        (skia-canvas (canvas (skia-context mirror))))
  ;;   (log:info "skia-canvas: ~a" skia-canvas)
  ;;   (make-instance 'skia-opengl-medium :skia-canvas skia-canvas))
  (make-instance 'skia-opengl-medium :skia-canvas nil))

(defun %lookup-skia-canvas (medium)
  (or (skia-canvas medium)
      (let* ((sheet (sheet-mirrored-ancestor (medium-sheet medium)))
             (mirror (sheet-mirror sheet))
             (skia-canvas (skia-core::canvas (skia-context mirror))))
        (if skia-canvas
            (progn
              (setf (skia-canvas medium) skia-canvas)
              skia-canvas)
            (error "Unable to lookup skia-canvas from medium: ~a" medium)))))

(defmacro with-skia-canvas ((medium) &body body)
  (alx:once-only (medium)
    `(let ((canvas::*canvas* (%lookup-skia-canvas ,medium)))
       (progn ,@body))))

(comment
  (with-skia-canvas ((mirror-medium mirror))
    (log:info "Got canvas: ~a" *canvas*))
  )

(defmethod medium-draw-line* ((medium skia-opengl-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (with-skia-canvas (medium)
            )
          )))))


(defmethod medium-draw-polygon* ((medium skia-opengl-medium) coord-seq closed filled)
  ;; (declare (ignore coord-seq closed filled))
  ;; (log:info "NYI: first of coord-seq: ~a of length: ~a | closed?: ~a, filled?: ~a"
  ;;           (first coord-seq) (length coord-seq) closed filled)
  nil)

(defun test-skia-drawing (medium mirror window)
  (log:info "lookup-skia-canvas: ~a" (%lookup-skia-canvas medium))
  (with-skia-canvas (medium)
    (canvas::clear-canvas)
    (let* ((time (/ (get-internal-real-time) internal-time-units-per-second))
           (rect-x (float (floor (+ 300 (* 100 (cos (* 10 time))))) 0f0))
           (rect-y (float (floor (+ 300 (* 100 (sin (* 10 time))))) 0f0))
           (circle-x (float (floor (+ 400 (* 150 (sin (* 5 time))))) 0f0))
           (circle-y (float (floor (+ 600 (* 150 (cos (* 1 time))))) 0f0)))
      (iffi:with-intricate-instances ((paint %skia:sk-paint))
        (let ((canvas::*paint* paint))
          (skia-core::set-paint-color32argb canvas::*paint* %skia:+sk-color-yellow+)
          (canvas::rectangle rect-x rect-y 500 500)
          (skia-core::set-paint-color32argb canvas::*paint* %skia:+sk-color-red+)
          (canvas::circle circle-x circle-y 300)) ))
    (canvas::flush-canvas)
    (sdl2::gl-swap-window window) )
  (sleep .1))

(defun %mirror-force-output (medium mirror)
  ;; (declare (optimize speed))
  ;; (log:info "finalizing output")
  (let* ((window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror)))
         (surface (sdl2:get-window-surface window))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    ;; (log:info "updating the surface ~s ~s" width height)
    (test-skia-drawing medium mirror window)
    ))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))
