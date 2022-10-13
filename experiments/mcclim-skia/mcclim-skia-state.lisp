(in-package #:mcclim-skia)

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
;;:butt :square :round :no-end-point (default :butt)
(defvar *cap-map*
  '((:butt . :butt-cap)
    (:butt . :default-cap)
    (:square . :square-cap)
    (:square . :last-cap)
    (:round . :round-cap)))

;;:miter-join :round-join :bevel-join :last-join :default-join
;;:miter :bevel :round :none (default :miter)
(defun translate-cap-shape (clim-cap-shape)
  (case clim-cap-shape
    (:butt    :butt-cap)
    (:square  :square-cap)
    (:round   :round-cap)
    (otherwise :butt-cap)))

(defvar *join-map*
  '((:miter . :miter-join)
    (:miter . :default-join)
    (:bevel . :bevel-join)
    (:bevel . :last-join)
    (:round . :round-join)))

(defun translate-join-shape (clim-join-shape)
  (case clim-join-shape
    (:miter  :miter-join)
    (:none   :miter-join)
    (:bevel  :bevel-join)
    (:round  :round-join)
    (otherwise :miter-join)))

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
