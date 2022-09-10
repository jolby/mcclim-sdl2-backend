(in-package #:mcclim-skia)

(defun round-coordinate (x)
  (floor (+ x .5)))

(defclass skia-opengl-medium (basic-medium)
  ;;XXX do we want skia canvas as a slot or as a special dynamic var? The
  ;;skia-canvas will need to be invalidated on screen resizes and maybe other
  ;;situations as well.
  ((skia-canvas :initarg :skia-canvas :accessor skia-canvas)
   (paint-stack :initarg nil
                :initform (make-array 1 :fill-pointer 0 :adjustable t)
                :accessor medium-paint-stack)))

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

(defun %push-paint (medium)
  (let ((paint (skia-core::make-paint)))
    (vector-push-extend paint (medium-paint-stack medium))
    (setf canvas::*paint* paint)))

(defun %pop-paint (medium)
  (let ((paint-stack (medium-paint-stack medium)))
    (skia-core::destroy-paint (vector-pop paint-stack))
    (setf canvas::*paint* (when (> (length paint-stack) 0)
                            (aref paint-stack (1- (length paint-stack)))))))

(defmacro with-paint ((medium) &body body)
  `(progn
     (%push-paint medium)
     (unwind-protect
          (progn ,@body)
       (%pop-paint medium))))

(defun %sync-skia-paint-with-medium (medium)
  (let ((line-style (medium-line-style medium))
        (ink (medium-ink medium)))
    (skia-core::set-paint-stroke-width canvas::*paint*
                                       (line-style-thickness line-style))
    ;;xxx do we want the anti-alias setting?
    (skia-core::set-paint-anti-alias canvas::*paint* t)
    (skia-core::set-paint-stroke canvas::*paint* t)
    (multiple-value-bind (red green blue alpha)
        (clime::color-rgba ink)
      (skia-core::set-paint-color4f canvas::*paint* red green blue alpha))))

(defmacro with-skia-canvas ((medium) &body body)
  (alx:once-only (medium)
    `(let ((canvas::*canvas* (%lookup-skia-canvas ,medium)))
       (with-paint (medium)
         (%sync-skia-paint-with-medium medium)
       (progn ,@body)))))

(defmethod medium-draw-line* ((medium skia-opengl-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (with-skia-canvas (medium)
            (skia-core::with-path (path)
              (skia-core::move-to path x1 y1)
              (skia-core::line-to path x2 y2)
              (skia-core::path-close path)
              (canvas::path path)
              ;;XXX Do this here or just in medium-finish/force-output??
              (canvas::flush-canvas))))))))


(defmethod medium-draw-polygon* ((medium skia-opengl-medium) coord-seq closed filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-skia-canvas (medium)
        (skia-core::with-path (path)
          (multiple-value-bind (x y)
              (climi::transform-position tr (first coord-seq) (second coord-seq))
            (skia-core::move-to path x y))
          (loop :for (x y) :on (cddr coord-seq) :by #'cddr
                :do (multiple-value-bind (nx ny)
                        (climi::transform-position tr x y)
                      (skia-core::line-to path nx ny)))
          (when closed (skia-core::path-close path))
          (when filled (skia-core::set-paint-style canvas::*paint* :fill-style))
          (log:info "path: ~a, is-valid: ~a" path (%skia:is-valid :const '(:pointer %skia:sk-path) path))
          (canvas::path path)
          ;;XXX Do this here or just in medium-finish/force-output??
          (canvas::flush-canvas)))))

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
         ;; (width (sdl2:surface-width surface))
         ;; (height (sdl2:surface-height surface))
         )
    ;; (log:info "updating the surface ~s ~s" width height)
    ;; (test-skia-drawing medium)
    (sdl2::gl-swap-window window)
    (sleep .1)
    ))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output medium mirror)))
