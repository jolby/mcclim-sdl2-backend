(in-package #:mcclim-skia)

(defun round-coordinate (x)
  (floor (+ x .5)))

(defun ensure-string-value (v)
  (etypecase v
    (string v)
    (character (string v))))

(defclass skia-opengl-medium (basic-medium)
  ;;XXX do we want skia canvas as a slot or as a special dynamic var? The
  ;;skia-canvas will need to be invalidated on screen resizes and maybe other
  ;;situations as well.
  ((skia-canvas :initarg :skia-canvas :accessor skia-canvas)
   (%paint-stack :initarg nil
                :initform (make-array 1 :fill-pointer 0 :adjustable t)
                :accessor medium-paint-stack)
   (%font-stack :initarg nil
                :initform (make-array 1 :fill-pointer 0 :adjustable t)
                :accessor medium-font-stack)
   ;; Maybe make this a deferred-drawing-mixin???
   (%deferred-command-queue :initarg nil
                           :initform (make-array 128 :fill-pointer 0 :adjustable t :initial-element nil)
     :accessor medium-deferred-command-queue)

   ))

(defmethod make-medium ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
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
     (%push-paint ,medium)
     (unwind-protect
          (progn ,@body)
       (%pop-paint ,medium))))

(defun %push-font (medium &optional typeface)
  (let ((font (if typeface
                  (skia-core::make-font typeface)
                  (skia-core::make-default-font))))
    (vector-push-extend font (medium-font-stack medium))
    (setf canvas::*font* font)))

(defun %pop-font (medium)
  (let ((font-stack (medium-font-stack medium)))
    (skia-core::destroy-font (vector-pop font-stack))
    (setf canvas::*font* (when (> (length font-stack) 0)
                            (aref font-stack (1- (length font-stack)))))))

(defmacro with-font ((medium typeface) &body body)
  `(progn
     (%push-font ,medium ,typeface)
     (unwind-protect
          (progn ,@body)
       (%pop-font ,medium))))

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
       (with-font (medium canvas::*default-typeface*)
         ;;XXX TODO pull from medium
         (canvas::font-size 64)
         (with-paint (medium)
           (%sync-skia-paint-with-medium medium)
           (progn ,@body))))))
;;;
;;; DRAW COMMAND
;;;
(defstruct (draw-command
            (:constructor make-draw-command ())
            (:conc-name %draw-command-))
  (name nil)
  (arguments nil))

(defun update-draw-command (command name &rest arguments)
  (setf (%draw-command-name command) name
        (%draw-command-arguments command) arguments))

(defun invoke-draw-command (command)
  (log:info "CMD: ~a" command)
  (apply (%draw-command-name command) (%draw-command-arguments command)))

;;;
;;; COMMAND QUEUE
;;;
(defun push-command (medium name &rest args)
  (let* ((queue (medium-deferred-command-queue medium))
         (fp (fill-pointer queue)))
    (if (= fp (array-total-size queue))
        (vector-push-extend nil queue 128)
        (setf (fill-pointer queue) (1+ fp)))

    (let ((next-command (alx:if-let ((next-command (aref queue fp)))
                          next-command
                          (setf (aref queue fp) (make-draw-command)))))
      (apply #'update-draw-command next-command name args)))
  (values))

(defun drain-draw-commands (medium)
  (let ((array (medium-deferred-command-queue medium)))
    (unwind-protect
         (loop for command across array
               do (invoke-draw-command command)
                  (update-draw-command command nil))
      (setf (fill-pointer array) 0))))

(defun discard-command-queue (medium)
  (setf (fill-pointer (medium-deferred-command-queue medium)) 0)
  (values))

(defmethod medium-draw-line* ((medium skia-opengl-medium) x1 y1 x2 y2)
  (with-skia-canvas (medium)
    (skia-core::with-path (path)
      (skia-core::path-move-to path x1 y1)
      (skia-core::path-line-to path x2 y2)
      (skia-core::path-close path)
      ;; (canvas::path path)
      (push-command medium #'canvas::path path)

      ;;XXX Do this here or just in medium-finish/force-output??
      ;; (canvas::flush-canvas)
      )) )

(defmethod medium-draw-polygon* ((medium skia-opengl-medium) coord-seq closed filled)
  (with-skia-canvas (medium)
    (skia-core::with-path (path)
      (let* ((x (first coord-seq))
             (y (second coord-seq)))
        (skia-core::path-move-to path x y)
        (loop :for (x y) :on (cddr coord-seq) :by #'cddr
              :do (skia-core::path-line-to path x y))
        (when closed (skia-core::path-close path))
        (when filled (skia-core::set-paint-style canvas::*paint* :fill-style))
        ;; (log:info "path: ~a, is-valid: ~a" path (%skia:is-valid :const '(:pointer %skia:sk-path) path))
        ;; (canvas::path path)
        (push-command medium #'canvas::path path)
        ))))

(defmethod medium-draw-rectangle* ((medium basic-medium) x1 y1 x2 y2 filled)
  (let ((width (abs (- x2 x1)))
        (height (abs (- y2 y1))))
    (with-skia-canvas (medium)
      (when filled (skia-core::set-paint-style canvas::*paint* :fill-style))
      ;; (canvas::rectangle x1 y1 width height)
      (push-command medium #'canvas::rectangle x1 y1 width height)

      )))

;; (defmethod medium-draw-circle* ((medium basic-medium) cx cy radius eta1 eta2 filled)
;;   )

(defmethod medium-draw-text* ((medium skia-opengl-medium) text x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let ((text (ensure-string-value text)))
    (when (alx:emptyp text)
      (return-from medium-draw-text*))
    (let ((fixed-text (subseq text (or start 0) (or end (length text)))))
      (log:info "Drawing string ~s at (~s,~s)"
                fixed-text x y)
      (with-skia-canvas (medium)
        (skia-core::set-paint-style canvas::*paint* :stroke-and-fill-style)
        ;; (canvas::simple-text fixed-text x y)
        (push-command medium #'canvas::simple-text fixed-text x y)
        ) )))

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
      (skia-core::path-move-to path r y-start)
      ;; (log:info "move-to: ~a,~a" r y)
      ;; (skia-core::move-to path r 400.0)
      ;; (skia-core::move-to path 800 800)
      ;; (log:info "move-to: ~a,~a" 500 500)
      (loop for i :below 7
            :for theta := (* 3 i (/ tau 7)) :then (* 3 i (/ tau 7))
            :for x := (* r (cos theta)) :then (* r (cos theta))
            :for y := (* r (sin theta)) :then (* r (sin theta))
            :do (progn
                  (skia-core::path-line-to path x y)
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
  (let* ((window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror))))
    (with-skia-canvas (medium)
      (drain-draw-commands medium)
      (canvas::flush-canvas))
    (sdl2::gl-swap-window window)))

;;XXX this won't work until we create a draw-command queue
;;all drawing requests must happen on the main/render thread or
;;you just get a black screen
(mcclim-sdl2::define-sdl2-request do-mirror-force-output (medium mirror)
  (log:info "do-force-output on thread: ~a" (bt:current-thread))
  (%mirror-force-output medium mirror))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (do-mirror-force-output medium mirror)))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (do-mirror-force-output medium mirror)))
