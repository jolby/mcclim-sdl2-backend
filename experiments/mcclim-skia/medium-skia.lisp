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

(defun %ensure-medium-skia-paint (medium)
  (if-let ((skia-paint (medium-skia-paint medium)))
    skia-paint
    (setf (medium-skia-paint medium) (skia-core::make-paint))))

(defun %ensure-medium-skia-font-paint (medium)
  (if-let ((skia-font-paint (medium-skia-font-paint medium)))
    skia-font-paint
    (setf (medium-skia-font-paint medium) (skia-core::make-paint))))

;;; secondary methods for changing text styles and line styles and syncing with
;;; corresponding skia paint/font styles
(defun %translate-cap-shape (clim-cap-shape)
  (case clim-cap-shape
    (:butt    :butt-cap)
    (:square  :square-cap)
    (:round   :round-cap)
    (otherwise :butt-cap)))

(defun %translate-join-shape (clim-join-shape)
  (case clim-join-shape
    (:miter  :miter-join)
    (:none   :miter-join)
    (:bevel  :bevel-join)
    (:round  :round-join)
    (otherwise :miter-join)))

(defun %update-dash-pattern (medium new-dashes)
  (log:warn "Not implemented"))

(defun %update-skia-paint-from-ink (medium ink)
  (let ((skia-paint (%ensure-medium-skia-paint medium))
        (skia-font-paint (%ensure-medium-skia-font-paint medium)))
    (multiple-value-bind (red green blue alpha) (clime::color-rgba ink)
      (skia-core::set-paint-color4f skia-paint red green blue alpha)
      (skia-core::set-paint-color4f skia-font-paint red green blue alpha))))

(defmethod (setf medium-ink) :before (ink (medium skia-opengl-medium))
  (let ((old-ink (medium-ink medium)))
    (unless (eq ink old-ink)
      (%update-skia-paint-from-ink medium ink))))

(defun %update-skia-paint-from-line-style (medium line-style)
  (let* ((skia-paint (%ensure-medium-skia-paint medium))
         (line-style (medium-line-style medium)))
    (skia-core::set-paint-width skia-paint
                                (round (line-style-effective-thickness line-style medium)))
    (skia-core::set-paint-stroke-cap skia-paint
                                     (%translate-cap-shape (line-style-cap-shape line-style)))
    (skia-core::set-paint-stroke-join skia-paint
                                      (%translate-join-shape (line-style-joint-shape line-style)))
    (%update-dash-pattern medium (line-style-dashes line-style))))

(defmethod (setf medium-line-style) :before (new-value (medium skia-opengl-medium))
  (let* ((skia-paint (%ensure-medium-skia-paint medium))
         (old-line-style (medium-line-style medium))
         (old-unit (line-style-unit old-line-style))
         (new-unit (line-style-unit new-value))
         (new-cap-shape (line-style-cap-shape new-value))
         (new-joint-shape (line-style-joint-shape new-value)))
    (unless (and (eq new-unit old-unit)
                 (eql (line-style-thickness new-value)
                      (line-style-thickness old-line-style)))
      (skia-core::set-paint-width skia-paint
            (round (line-style-effective-thickness new-value medium))))
    (unless (eq new-cap-shape (line-style-cap-shape old-line-style))
      (skia-core::set-paint-stroke-cap skia-paint
            (%translate-cap-shape new-cap-shape)))
    (unless (eq new-joint-shape (line-style-joint-shape old-line-style))
      (skia-core::set-paint-stroke-join skia-paint
            (%translate-join-shape new-joint-shape)))
    (unless (and new-unit old-unit
                 (eq (line-style-dashes new-value)
                     (line-style-dashes old-line-style)))
      (%update-dash-pattern medium (line-style-dashes new-value)))))

(defun %font-file-path-from-text-style (text-style)
  (multiple-value-bind (family face size)
      (text-style-components text-style)
    (alx:assoc-value mcclim-truetype::*families/faces* (list family face) :test #'equal)))

(defun %load-skia-typeface (font-path)
  (unless (uiop/filesystem:file-exists-p font-path)
      (error (format nil "File: ~a doesn't exist!" font-path)))
    (let* ((file-bytes-vec (skia-core::read-file-into-shareable-vector font-path))
           (typeface (skia-core::make-typeface file-bytes-vec)))
      typeface))

(defun %swap-skia-font (medium new-font)
  (if-let ((old-font (medium-skia-font medium)))
           (skia-core::destroy-font old-font))
  (setf (medium-skia-font medium) new-font))

(defun %update-skia-font-from-text-style (medium text-style)
  (let ((skia-font (medium-skia-font medium))
        (text-size (text-style-size text-style))
        (font-path (%font-file-path-from-text-style text-style))
        (font-paint (%ensure-medium-skia-font-paint medium)))
    (unless font-path (error "Error finding font-path for text-style: ~a" text-style))
    (let ((skia-typeface skia-typeface-exists-p) (gethash font-path (fontpath->skia-typeface (port medium))))
      (unless skia-typeface-exists-p
        (setf skia-typeface (%load-skia-typeface font-path))
        (unless skia-typeface (error "Error loading skia-typeface for font-path: ~a" font-path))
        (setf (gethash font-path (fontpath->skia-typeface (port medium))) skia-typeface))
      (if (and skia-font
               (skia-core::typeface-equal-p skia-typeface (skia-core::font-typeface skia-font)))
          (unless (= text-size (skia-core::font-size skia-font))
                (skia-core::set-font-size skia-font text-size))
          (let ((new-font (skia-core::make-font skia-typeface)))
            (skia-core::set-font-size new-font text-size)
            (skia-core::set-paint-style font-paint :stroke-and-fill-style)
            (skia-core::set-paint-anti-alias font-paint t)
            (%swap-skia-font medium new-font))))))

(defmethod (setf medium-text-style) :before (text-style (medium skia-opengl-medium))
  (let ((old-text-style (medium-text-style medium)))
    (unless (eq text-style old-text-style)
      (let ((text-style-mapping (text-style-mapping (port medium) text-style)))
        (log:info "changing text-style-mapping to: ~a, from: ~a" text-style-mapping old-text-style)
        (%update-skia-font-from-text-style medium text-style)))))

(defmethod initialize-instance :after ((medium skia-opengl-medium) &rest args)
  (declare (ignore args))
  (let ((ink (medium-ink medium))
        (line-style (medium-line-style medium))
        (text-style (medium-text-style medium))))
  (log:info "")
  (%update-skia-paint-from-ink medium ink)
  (%update-skia-paint-from-line-style medium line-style)
  (%update-skia-font-from-text-style text-style))

(defun %invoke-with-skia-drawing-state-synced-with-medium (medium continuation)
  (declare (ignorable medium continuation))
  (funcall continuation))
;;
;; Draw OPs playback
;;
(defmacro with-skia-canvas ((medium) &body body)
  (alx:once-only (medium)
    (alx:with-gensyms (gskia-draw-op)
      `(let ((canvas::*canvas* (%lookup-skia-canvas ,medium)))
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
