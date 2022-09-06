(in-package #:mcclim-skia)

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

(defclass skia-mirrored-sheet-mixin (mirrored-sheet-mixin) ())

(defclass sdl2-skia-top-level-sheet
    (top-level-sheet-mixin skia-mirrored-sheet-mixin basic-sheet)
  ())

(defclass sdl2-skia-window (sdl2-skia-top-level-sheet basic-pane) ())

(defclass skia-app-sheet (;; repainting
                          immediate-repainting-mixin
                          ;; input
                          immediate-sheet-input-mixin
                          ;; output
                          permanent-medium-sheet-output-mixin
                          ;;temporary-medium-sheet-output-mixin
                          ;;sheet-with-medium-mixin
                          ;;sheet-mute-output-mixin
                          ;; geometry
                          sheet-identity-transformation-mixin
                          ;; genealogy
                          sheet-parent-mixin
                          sheet-leaf-mixin
                          ;; windowing
                          sdl2-skia-top-level-sheet
                      )
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Skia Test Sheet"
                     :region (make-rectangle* -200 -200 1080 720)))

(defun simple-draw (sheet)
  (let ((medium sheet))
    (with-bounding-rectangle* (x1 y1 x2 y2) medium
      (medium-clear-area medium x1 y1 x2 y2)
      (draw-rectangle* medium (+ x1 10) (+ y1 10) (- x2 10) (- y2 10)
                       :ink +deep-sky-blue+)
      (draw-circle* medium 0 0 25 :ink (alexandria:random-elt
                                        (make-contrasting-inks 8)))
                                        ;(draw-text* medium "(0,0)" 0 0)
                                        ;(sleep 1)
      (medium-finish-output sheet))))

(defmethod handle-event ((sheet skia-app-sheet) event)
  (log:info "Unhandled event ~s has arrived." (class-name (class-of event))))

(defmethod handle-event ((sheet skia-app-sheet) (event window-configuration-event))
  ;; FIXME resize-sheet may call port-set-window-geometry and then we will
  ;; receive this event back again. currently core's handler uses a flag
  ;; *configuration-event-p* to inhibit this behavior, but we need to come up
  ;; with something better. perhaps we should handle differently size-changed
  ;; and resize from sdl events.
  (resize-sheet sheet
                (climi::window-configuration-event-width event)
                (climi::window-configuration-event-height event))
  (repaint-sheet sheet +everywhere+))

(defmethod handle-event ((sheet skia-app-sheet) (event window-manager-delete-event))
  (let ((mirror (sheet-direct-mirror sheet)))
    (destroy-mirror (port sheet) mirror)))

(defmethod handle-event ((sheet skia-app-sheet) (event window-repaint-event))
  (handle-repaint sheet (window-event-region event)))

(defmethod handle-repaint ((sheet skia-app-sheet) region)
  (log:warn "Repainting a window (region ~s)." region)
  (simple-draw sheet))

(defun open-skia-sheet (path &optional restartp)
  (let ((port (find-port :server-path path)))
    (when restartp
      (restart-port port))
    (let (;; Supplying :PORT here is a kludge in the core.
          (sheet (make-instance 'skia-app-sheet :port port))
          (graft (find-graft :port port)))
      (realize-mirror port sheet)
      (sheet-adopt-child graft sheet)
      sheet)))

(defun close-skia-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet))


(mcclim-sdl2::define-sdl2-request do-simple-draw ()
  (simple-draw *skia-sheet*))

(comment
  (defparameter *skia-sheet* (open-skia-sheet :sdl2 t))

  (close-skia-sheet *skia-sheet*)

  (defparameter *skia-port* (find-port :server-path :sdl2))
  (defparameter *new-sheet* (make-instance 'skia-app-sheet :port *skia-port*))

  (let ((result (do-simple-draw :synchronize t)))
    (if (typep result 'condition)
        (error result)
        'done))
)
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
