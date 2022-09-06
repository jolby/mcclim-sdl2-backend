(in-package #:mcclim-skia)

(declaim (special *canvas*
                  *paint*
                  *font*))

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
            (skia-canvas (skia-canvas sheet)))
        (if skia-canvas
            (progn
              (setf (skia-canvas medium) skia-canvas)
              skia-canvas)
            (error "Unable to lookup skia-canvas from medium: ~a" medium)))))

(defmacro with-skia-canvas ((medium) &body body)
  `(alx:once-only (medium)
      (let ((*canvas* (%lookup-skia-canvas ,medium)))
          (progn ,@body))))

(defmethod medium-draw-line* ((medium skia-opengl-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (with-skia-canvas (medium)
            (log:info "NIY: x1: ~a, y1: ~a, x2: ~a, y2: ~a" x1 y1 x2 y2)
            ;; (update-attrs medium)
            ;; (skia:move-to x1 y1)
            ;; (skia:line-to x2 y2)
            ;; (skia:stroke)
            ))))))

(defmethod medium-draw-polygon* ((medium skia-opengl-medium) coord-seq closed filled)
  ;; (declare (ignore coord-seq closed filled))
  (log:info "NYI: coord-seq: ~a closed?: ~a, filled?: ~a" coord-seq closed filled)
  nil)

(defun %mirror-force-output (mirror)
  ;; (declare (optimize speed))
  (log:info "finalizing output")
  (let* ((window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror)))
         (surface (sdl2:get-window-surface window))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (log:info "updating the surface ~s ~s" width height)))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output mirror)))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output mirror)))
