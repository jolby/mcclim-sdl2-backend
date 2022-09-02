(in-package #:mcclim-sdl2)

;;; A complete sheet should subclass basic-sheet and pick its mixins:
;;;
;;;   Input protocol (xor):
;;;
;;;     standard-sheet-input-mixin
;;;     delegate-sheet-input-mixin
;;;     immediate-sheet-input-mixin
;;;     sheet-mute-input-mixin
;;;
;;;   Output protocol:
;;;
;;;     standard-sheet-output-mixin xor sheet-mute-output-mixin
;;;     sheet-with-medium-mixin
;;;         permanent-medium-sheet-output-mixin
;;;         temporary-medium-sheet-output-mixin
;;;
;;;   Genealogy:
;;;
;;;     sheet-parent-mixin
;;;     sheet-leaf-mixin xor sheet-single-child-mixin xor sheet-multiple-child-mixin
;;;
;;;   Repainting (xor):
;;;
;;;     standard-repainting-mixin
;;;     immediate-repainting-mixin
;;;     sheet-mute-repainting-mixin
;;;
;;;   Geometry (xor):
;;;
;;;     sheet-identity-transformation-mixin
;;;     sheet-translation-mixin
;;;     sheet-y-inverting-transformation-mixin
;;;     sheet-transformation-mixin
;;;
;;;   Windowing (zero or more, may be mixed, the order of mixins is important)
;;;
;;;     top-level-sheet-mixin
;;;     unmanaged-sheet-mixin
;;;     mirrored-sheet-mixin
;;;
(defvar *glider*
  (make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defclass plain-sheet (;; repainting
                       immediate-repainting-mixin
                       ;; input
                       immediate-sheet-input-mixin
                       ;; output
                       permanent-medium-sheet-output-mixin
                       ;temporary-medium-sheet-output-mixin
                       ;sheet-with-medium-mixin
                       ;sheet-mute-output-mixin
                       ;; geometry
                       sheet-identity-transformation-mixin
                       ;; genealogy
                       sheet-parent-mixin
                       sheet-leaf-mixin
                       ;; windowing
                       top-level-sheet-mixin
                       mirrored-sheet-mixin
                       ;; the base class
                       basic-sheet)
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Test Sheet"
                     :region (make-rectangle* -200 -200 200 200)))

(defmethod handle-event ((sheet plain-sheet) event)
  (log:info "Unhandled event ~s has arrived." (class-name (class-of event))))

(defmethod handle-event ((sheet plain-sheet) (event window-manager-delete-event))
  (destroy-mirror (port sheet) sheet))

(defmethod handle-event ((sheet plain-sheet) (event window-repaint-event))
  (handle-repaint (event-sheet sheet) (window-event-region event)))

(defmethod handle-repaint ((sheet plain-sheet) region)
  (declare (ignore region)))

(defun open-plain-sheet (path &optional restartp)
  (let ((port (find-port :server-path path)))
    (when restartp
      (restart-port port))
    (let (;; Supplying :PORT here is a kludge in the core.
          (sheet (make-instance 'plain-sheet :port port))
          (graft (find-graft :port port)))
      (sheet-adopt-child graft sheet)
      (setf (climi::%sheet-native-region sheet)
            (make-rectangle* 0 0 400 400)
                                        ;(sheet-region sheet)
            )
      sheet)))

(defun close-plain-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet))

(defparameter *xxx* (open-plain-sheet :sdl2))

(defun %do-it ()
  (let ((medium *xxx*))
    (medium-clear-area medium -200 -200 200 200)
    (draw-rectangle* medium -175 -175 175 175 :ink +light-green+)
    (draw-circle* medium 0 0 25 :ink (alexandria:random-elt
                                      (make-contrasting-inks 8)))
    ;(draw-text* medium "(0,0)" 0 0)
    (medium-finish-output *xxx*)))

(define-sdl2-request do-it ()
  (%do-it))

;#+ ()
(let ((result (do-it :synchronize t)))
  (if (typep result 'condition)
      (error result)
      'done))

;(%do-it 50 50 300 300)





;; (defvar *yyy* (open-plain-sheet :clx-ttf))
;; (setf *xxx* (open-plain-sheet :sdl2))

;;(sheet-disown-child (sheet-parent *xxx*) *xxx*)

;;(open-plain-sheet :clx-ttf)
;; (testme *xxx*)
;; (defun testme (sheet)
;;   ;; (let ((window (sdl2-window (sheet-mirror sheet))))
;;   ;;   (sdl2:get-window-surface window)
;;   ;;   (sdl2:update-window window))
;;   ;; #+ (or)
;;   (let* ((surface (create-surface sheet 100 100))
;;          (window (sdl2-window (sheet-mirror sheet)))
;;          (target (sdl2:get-window-surface window)))
;;     (update-surface surface *glider*)
;;     (blit-surface surface target)
;;     (destroy-surface surface)
;;     (sdl2:update-window window)))

;; (testme *xxx*)

;; ;;; FIXME CLX assumes that all sheet are panes (i.e calls compose-space)
;; ;; (defvar *yyy* (open-plain-sheet :clx-ttf))


;; ;; (setf (sheet-pretty-name *xxx*)
;; ;;       "Justysianka Mazuranka Kochanka")

;; ;; (destroy-mirror (port *xxx*) *xxx*)
