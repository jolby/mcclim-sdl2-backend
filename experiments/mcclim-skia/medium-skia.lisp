(in-package #:mcclim-skia)

(defmethod make-medium ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (make-instance 'skia-opengl-medium :port port :skia-canvas nil))

(defun %medium-mirror (medium)
  (sheet-mirror (sheet-mirrored-ancestor (medium-sheet medium))))

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

(defun push-paint-command (medium &key (color32argb #xFF000000) (stroke-width 1)
                                    (stroke-style :stroke-and-fill-style)
                                    (stroke-cap :butt-cap) (join-cap :miter-join))
  (let ((new-paint (%push-paint medium)))
    (skia-core::set-paint-color32argb new-paint color32argb)
    (skia-core::set-paint-style new-paint stroke-style)
    (skia-core::set-paint-stroke-cap new-paint stroke-cap)
    (skia-core::set-paint-stroke-join new-paint join-cap)
    (skia-core::set-paint-stroke-width new-paint stroke-width)
    (skia-core::set-paint-anti-alias new-paint t)))

(defun pop-paint-command (medium)
  (%pop-paint medium))

(defun %capture-paint-to-command-queue (medium ink line-style)
  (let* ((color32argb (%ink->color32argb ink))
         (ls-cap (line-style-cap-shape line-style))
         (ls-join (line-style-joint-shape line-style))
         (ls-width (line-style-thickness line-style)))
    (push-command medium #'push-paint-command medium
                  :color32argb color32argb :stroke-width ls-width
                  :stroke-cap (%paint-style-lookup ls-cap *cap-map*)
                  :join-cap (%paint-style-lookup ls-join *join-map*))))



(defun push-font-command (medium &key (color32argb #xFF000000)
                                   (size (text-style-size *default-text-style*))
                                   (face (text-style-face *default-text-style*))
                                   (family (text-style-family *default-text-style*)))
  (let ((new-paint (%push-paint medium))
        (new-font (%push-font medium (canvas::get-typeface family face))))
    (skia-core::set-font-size size)
    (skia-core::set-paint-color32argb new-paint color32argb)
    (skia-core::set-paint-style new-paint :stroke-and-fill-style)
    (skia-core::set-paint-anti-alias new-paint t)
    ))

(defun pop-font-command (medium)
  (%pop-font medium)
  (%pop-paint medium))

(defun %capture-font-to-command-queue (medium ink text-style)
  (let* ((color32argb (%ink->color32argb ink)))
    (push-command medium #'push-font-command medium :color32argb color32argb
                                                    :size (text-style-size text-style)
                                                    :face (text-style-face text-style)
                                                    :family (text-style-family text-style))))

(defun sk-font-needs-change-p (ink text-style &optional (font canvas::*font*))
  (let* ((changelist)
         (ts-size (clim-internals::normalize-font-size (text-style-size text-style)))
         (f-size (skia-core::font-size font)))
    (unless (= ts-size f-size) (push :size changelist)))
  changelist)


;;:butt-cap :round-cap :square-cap :last-cap :default-cap
;; :butt :square :round :no-end-point (default :butt)
(defvar *cap-map*
  '((:butt . :butt-cap)
    (:butt . :default-cap)
    (:square . :square-cap)
    (:square . :last-cap)
    (:round . :round-cap)))
;;:miter-join :round-join :bevel-join :last-join :default-join
;;:miter :bevel :round :none (default :miter)
(defvar *join-map*
  '((:miter . :miter-join)
    (:miter . :default-join)
    (:bevel . :bevel-join)
    (:bevel . :last-join)
    (:round . :round-join)))

(defun %line-style-match-p (ls ps map-alist)
  (loop :for (l . p) in map-alist
        :when (and (eq l ls) (eq p ps))
              :collect (list l p)))

(defun %paint-style-lookup (ls map-alist)
  (loop :for (l . p) in map-alist
        :when (eq l ls)
          :return p))

(defun sk-paint-needs-change-p (ink line-style &optional (paint canvas::*paint*))
  (let* ((changelist)
         (ls-cap (line-style-cap-shape line-style))
         (p-cap (skia-core::get-paint-stroke-cap paint))
         (ls-join (line-style-joint-shape line-style))
         (p-join (skia-core::get-paint-stroke-join paint)))
    (unless (every #'= (list (clime::color-rgba ink)) (list (skia-core::get-paint-color4f paint)))
      (push :color changelist))
    (unless (= (line-style-thickness line-style) (skia-core::get-paint-stroke-width paint))
      (push :stroke-width changelist))
    (unless (%line-style-match-p ls-cap p-cap *cap-map*)
      (push :cap-style changelist))
    (unless (%line-style-match-p ls-join p-join *join-map*)
      (push :join-style changelist))
    ;;XXX TODO dashes and other path effects... gradients,shadow,glow,blur etc effects?
  changelist))

(defun %invoke-with-capturing-medium-state (medium continuation)
  (let* ((ink (medium-ink medium))
         (line-style (medium-line-style medium))
         (text-style (medium-text-style medium))
         (clip (medium-clipping-region medium))
         (transform (medium-transformation medium))
         (paint-change-p (sk-paint-needs-change-p ink line-style))
         (font-change-p (sk-font-needs-change-p ink text-style))
        )
    (when paint-change-p (%capture-paint-to-command-queue medium ink line-style))
    (when font-change-p (%capture-font-to-command-queue medium ink text-style))
    (unwind-protect
         (progn
           (funcall continuation)
           )
      (when font-change-p (push-command medium #'%pop-font medium))
      (when paint-change-p (push-command medium #'%pop-paint medium)))))

;;
;; Draw OP recording
;;
(defmacro with-medium-to-skia-drawing-recorder ((medium) &body body)
  (alx:with-gensyms (ink line-style text-style clip transform)
    `(let* ((,ink (medium-ink ,medium))
            (,line-style (medium-line-style ,medium))
            (,text-style (medium-text-style ,medium))
            (,clip (medium-clipping-region ,medium))
            (,transform (medium-transformation ,medium))
            )
       (%capture-paint-to-command-queue ,medium ,ink ,line-style)
       (%capture-font-to-command-queue ,medium ,ink ,line-style)

       (unwind-protect
            (progn ,@body)
         (push-command medium #'pop-font-command medium)
         (push-command medium #'pop-paint-command medium)
         ))))

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
             (log:info "XXX paint: ~a" canvas::*paint*)
             (log:info "XXX font: ~a" canvas::*font*)
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

(defun %do-skia-output (medium)
  ;; (break)
  (handler-case
      (let ((canvas::*paint* nil)
            (canvas::*font* nil))
        (%push-paint medium)
        (log:info "XXX queue fill ptr: ~a"
                  (fill-pointer (medium-deferred-command-queue medium)))
        (unwind-protect
             (with-skia-canvas (medium)
               (drain-draw-commands medium)
               (log:info "XXX queue fill ptr: ~a"
                         (fill-pointer (medium-deferred-command-queue medium)))
               (canvas::flush-canvas))
          (%pop-paint medium)))
    (error (c)
      (log:error "~a" c))) )

(mcclim-sdl2::define-sdl2-request do-medium-skia-output (medium)
  (log:info "do-medium-skia-output on thread: ~a" (bt:current-thread))
  (%do-skia-output medium))

(defmethod medium-clear-area ((medium skia-opengl-medium) left top right bottom)
  (with-skia-canvas (medium)
    ;; (push-command medium #'canvas::rectangle x1 y1 width height)
    ;; (draw-rectangle* medium left top right bottom
    ;;                  :ink (compose-over (medium-background medium) +black+))
    (push-command medium #'canvas::clear-canvas)
    ))

(defmethod medium-drawable ((medium skia-opengl-medium))
  (%medium-mirror medium))

(defmethod medium-finish-output :before ((medium skia-opengl-medium))
  (alx:when-let ((mirror (medium-drawable medium)))
    (log:info "FINISH OUTPUT")
    (do-medium-skia-output medium)
    (sleep 0.2)))

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
