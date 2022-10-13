(in-package #:mcclim-skia)

(defmethod make-medium ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (make-instance 'skia-opengl-medium :port port :skia-canvas nil))

(defun %medium-mirror (medium)
  (sheet-mirror (sheet-mirrored-ancestor (medium-sheet medium))))

(defmethod medium-drawable ((medium skia-opengl-medium))
  (%medium-mirror medium))

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

(defun %medium-has-pending-draw-ops-p (medium)
  (bt:with-lock-held (*command-queue-lock*)
    (let ((array (medium-deferred-command-queue medium)))
      (log:info "fill pointer: ~a" (fill-pointer array))
      (> (fill-pointer array) 0))))

(defun %invoke-with-skia-drawing-state-synced-with-medium (medium continuation)
  (declare (ignorable medium continuation))
  (funcall continuation))
;;
;; Draw OP playback
;;
(defmacro with-skia-canvas ((medium) &body body)
  (alx:once-only (medium)
    (alx:with-gensyms (gskia-draw-op)
      `(let ((canvas::*canvas* (%lookup-skia-canvas ,medium))
             ;;XXX TODO remove need for these
             ;;(canvas::*paint* (if (boundp 'canvas::*paint*) canvas::*paint* (skia-core::make-paint)))
             )
         (with-paint (medium)
           (with-font (medium canvas::*default-typeface*)
             ;; (log:info "XXX paint: ~a" canvas::*paint*)
             ;; (log:info "XXX font: ~a" canvas::*font*)
             (flet ((,gskia-draw-op ()
                      ,@body))
               (declare (dynamic-extent #',gskia-draw-op))
               (%invoke-with-skia-drawing-state-synced-with-medium
                ,medium #',gskia-draw-op)
               )
             ))
         ))))

(defmethod medium-draw-line* ((medium skia-opengl-medium) x1 y1 x2 y2)
  (with-skia-canvas (medium)
      (push-command medium #'canvas::line x1 y1 x2 y2)
    ))

(defmethod medium-draw-polygon* ((medium skia-opengl-medium) coord-seq closed filled)
  (with-skia-canvas (medium)
    (push-command medium #'canvas::polygon coord-seq :closed closed :filled filled)))

(defmethod medium-draw-rectangle* ((medium basic-medium) x1 y1 x2 y2 filled)
  (let ((width (abs (- x2 x1)))
        (height (abs (- y2 y1))))
    (with-skia-canvas (medium)
      ;; (when filled (skia-core::set-paint-style canvas::*paint* :fill-style))
      ;; (canvas::rectangle x1 y1 width height)
      (push-command medium #'canvas::rectangle x1 y1 width height))))

;; (defmethod medium-draw-circle* ((medium basic-medium) cx cy radius eta1 eta2 filled)
;;   )

(defmethod medium-draw-text* ((medium skia-opengl-medium) text x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (let ((text (skia-core::ensure-string-value text)))
    (when (alx:emptyp text)
      (return-from medium-draw-text*))
    (let ((fixed-text (subseq text (or start 0) (or end (length text)))))
      (with-skia-canvas (medium)
        (log:info "Drawing string ~s at (~s,~s)"
                  fixed-text x y)

        (skia-core::set-paint-style canvas::*paint* :stroke-and-fill-style)
        ;; (canvas::simple-text fixed-text x y)
        (push-command medium #'canvas::simple-text fixed-text x y)
        ) )))


(defmethod medium-clear-area ((medium skia-opengl-medium) left top right bottom)
  (with-skia-canvas (medium)
    ;; (push-command medium #'canvas::rectangle x1 y1 width height)
    ;; (draw-rectangle* medium left top right bottom
    ;;                  :ink (compose-over (medium-background medium) +black+))
    (log:info "clear-area... ")
    (push-command medium #'canvas::clear-canvas)
    ))

(defun %swap-window-buffers (medium)
  (alx:when-let ((mirror (medium-drawable medium)))
    (sdl2::gl-swap-window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror)))
    ))

(defun %do-skia-output (medium)
  ;; (break)
  (unless (%medium-has-pending-draw-ops-p medium)
    (return-from %do-skia-output))
  (handler-case
      (let ((canvas::*paint* nil)
            (canvas::*font* nil))
        (%push-paint medium)
        (unwind-protect
             (with-skia-canvas (medium)
               (drain-draw-commands medium)
               (canvas::flush-canvas)
               (let* ((mirror (medium-drawable medium))
                      (window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror))))
                 (sdl2::gl-swap-window window)))
          (%pop-paint medium)))
    (error (c)
      (log:error "~a" c))) )

(mcclim-sdl2::define-sdl2-request do-medium-skia-output (medium)
  (log:info "do-medium-skia-output on thread: ~a" (bt:current-thread))
  (%do-skia-output medium))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (log:info "FINISH OUTPUT")
    (when (%medium-has-pending-draw-ops-p medium)
      (do-medium-skia-output medium))))

(defmethod medium-force-output :before ((medium skia-opengl-medium))
  ;; (break)
  (log:info "FORCE OUTPUT. ")
  ;; (do-mirror-force-output medium mirror)

  ;; This is called immediately after finish-output
  ;; in an unwind-protect (invoke-with-output-buffered) Core/drawing/medium.lisp

  ;; Note below may not be current (still investigating repaint protocol):
  ;; This was causing spurious push-paint draw commands to be inserted
  ;; in the command queue. This resulted in no actual drawing commands being
  ;; queued to opengl, and then when calling swap-windows it presented a black
  ;; screen.

  (alx:when-let ((mirror (medium-drawable medium)))
    (log:info "FORCE OUTPUT. gl-swap-window...")
    (sdl2::gl-swap-window (mcclim-sdl2::sdl2-window (mcclim-sdl2::window-id mirror)))))
