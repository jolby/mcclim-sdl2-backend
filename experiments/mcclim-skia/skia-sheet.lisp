(in-package #:mcclim-skia)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

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
      (log:info "bounding rect: x1: ~a, y1: ~a, x2: ~a, y2: ~a" x1 y1 x2 y2)
      (medium-clear-area medium x1 y1 x2 y2)
      (medium-draw-line* medium 0 0 500 500)
      (draw-rectangle* medium
                       (+ x1 10) (+ y1 10)
                       (- x2 10) (- y2 10)
                       :ink +deep-sky-blue+)
      (draw-circle* medium 0 0 25 :ink (alexandria:random-elt
                                        (make-contrasting-inks 8)))
      (draw-text* medium "(100,100)" 100 100)
      ;;(sleep 1)
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
  (log:info "DELETE. sheet: ~a. (sheet-direct-mirror sheet) ==> ~a"
            sheet (sheet-direct-mirror sheet))
  (log:info "DELETE. current-thread: ~a" (bt:current-thread))
  ;;This works: I think by avoiding going through the SDL request
  ;;mechanism. That should be okay because event handlers should be being called
  ;;on the SDL thread anyways (I think...)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (%destroy-sdl2-opengl-skia-mirror mirror))

  ;; (break)
  ;; XXX this gets called twice. Why??
  ;; (destroy-mirror (port sheet) sheet)
  ;; (alx:when-let ((mirror (sheet-direct-mirror sheet)))
  ;;   (destroy-mirror (port sheet) mirror))
  )

(defmethod handle-event ((sheet skia-app-sheet) (event window-repaint-event))
  (handle-repaint sheet (window-event-region event)))

(defmethod handle-repaint ((sheet skia-app-sheet) region)
  (log:warn "Repainting a window (region ~s)." region)

  (simple-draw sheet))


(defparameter *skia-port* nil)
(defparameter *skia-mirror* nil)
(defparameter *skia-sheet* nil)
(defparameter *skia-medium* nil)
(defparameter *skia-default-typeface* nil)

(defun load-test-font ()
  (let ((font-path (asdf:system-relative-pathname
                    "mcclim-sdl2-skia" "assets/fonts/NimbusRoman-Regular.otf")))
    (unless (uiop/filesystem:file-exists-p font-path)
      (error (format nil "File: ~a doesn't exist!" font-path)))
    (let* ((file-bytes-vec (skia-core::read-file-into-shareable-vector font-path))
          (typeface (skia-core::make-typeface file-bytes-vec)))
      (setf *skia-default-typeface* typeface)
      (setf canvas::*default-typeface* typeface))))

(defun open-skia-sheet (path &optional restartp)
  (let ((port (find-port :server-path path)))
    (when restartp
      (restart-port port))
    (setf *skia-port* port)
    (unless *skia-default-typeface* (load-test-font))
    (let* (;; Supplying :PORT here is a kludge in the core.
          (sheet (make-instance 'skia-app-sheet :port port))
          (graft (find-graft :port port))
          (mirror (realize-mirror port sheet)))
      (sheet-adopt-child graft sheet)
      (setf *skia-mirror* mirror
            *skia-medium* (sheet-medium sheet)
            *skia-sheet* sheet))))

(defun close-skia-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet))

(mcclim-sdl2::define-sdl2-request do-simple-draw (sheet)
  (simple-draw *skia-sheet*))

(defun nuke-state ()
  (when *skia-sheet* (destroy-mirror *skia-port* *skia-sheet*) (setf *skia-sheet* nil))
  (when *skia-mirror* (destroy-mirror *skia-port* *skia-mirror*) (setf *skia-mirror* nil))
  (when *skia-port* (destroy-port *skia-port*) (setf *skia-port* nil)))

(comment

  (load-test-font)

  (defparameter *skia-port* (find-port :server-path :sdl2))
  (defparameter *skia-sheet* (make-instance 'skia-app-sheet :port *skia-port*))
  (defparameter *skia-sheet* (open-skia-sheet :sdl2 t))

  (open-skia-sheet :sdl2 t)
  (let ((result (do-simple-draw *skia-sheet* :synchronize t)))
    (if (typep result 'condition)
        (error result)
        'done))
  (sheet-direct-mirror *skia-sheet*)
  (close-skia-sheet *skia-sheet*)
  (nuke-state)
  (destroy-port (find-port :server-path :sdl2))
)
