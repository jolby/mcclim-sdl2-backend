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
  (make-instance 'skia-opengl-medium :port port :skia-canvas nil))

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

(defun test-drawing-star (medium)
  ;; https://fiddle.skia.org/c/@skcanvas_star

  (with-skia-canvas (medium)
    (let* ((path (skia-core::make-path))
           (canvas::*paint* (skia-core::make-paint))
           ;; (canvas::*paint* %skia:+sk-color-yellow+)
           ;; (scale 256.0)
           (scale 25.6)
           ;; (r (* 45.0 scale))
           ;; (r (* 25.0 scale))
           (r (* 15.0 scale))
           (y-start 0f0)
           ;; (y-start -300f0)
           (tau 6.2831853))
      (skia-core::move-to path r y-start)
      ;; (log:info "move-to: ~a,~a" r y)
      ;; (skia-core::move-to path r 400.0)
      ;; (skia-core::move-to path 800 800)
      ;; (log:info "move-to: ~a,~a" 500 500)
      (loop for i :below 7
            :for theta := (* 3 i (/ tau 7)) :then (* 3 i (/ tau 7))
            :for x := (* r (cos theta)) :then (* r (cos theta))
            :for y := (* r (sin theta)) :then (* r (sin theta))
            :do (progn
                  (skia-core::line-to path x y)
                  (log:info "line-to: ~a,~a theta: ~a, tau: ~a" x y theta tau)
                  ))
      (skia-core::path-close path)

      (skia-core::set-paint-anti-alias canvas::*paint* 1)
      (skia-core::set-paint-color32argb canvas::*paint*
                                        %skia:+sk-color-black+)
      (skia-core::set-paint-stroke-width canvas::*paint* 10)
      ;; (skia-core::set-paint-style canvas::*paint* :stroke-style)
      (skia-core::set-paint-style canvas::*paint* :fill-style)
      (canvas::clear-canvas-to-white)
      ;; (canvas::translate (* -0.5 scale) (* -0.5 scale))
      (canvas::translate (* 10.0 scale) (* 10.0 scale))
      (canvas::path path)
      (canvas::flush-canvas))))

(defun test-drawing-oscillating-shapes (medium)
  (with-skia-canvas (medium)
    (canvas::clear-canvas-to-white)
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
          (canvas::circle circle-x circle-y 100)) ))
    (canvas::flush-canvas)))

(defun test-skia-drawing (medium)
  ;; Pick amongst these to test skia drawing
  ;; (test-drawing-oscillating-shapes medium)

  (test-drawing-star medium)
  )

(defun %mirror-force-output (medium mirror)
  ;; (declare (optimize speed))
  ;; (log:info "finalizing output")
  (let* ((window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror)))
         (surface (sdl2:get-window-surface window))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    ;; (log:info "updating the surface ~s ~s" width height)
    (test-skia-drawing medium)
    (sdl2::gl-swap-window window)
    (sleep .1)
    ))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))
