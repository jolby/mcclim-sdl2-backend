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


(defclass skia-app-sheet (;; repainting
                          immediate-repainting-mixin
                          ;; input
                          immediate-sheet-input-mixin
                          ;; output
                          permanent-medium-sheet-output-mixin
                          standard-output-recording-stream
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
                     ;; :region (make-rectangle* -200 -200 1080 720)
                     :region (make-rectangle* 0 0 1080 720)
                     ;; :buffering-p t
   ))

(defun simple-draw (sheet)
  (let ((medium (sheet-medium sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2) sheet
      (log:info "SIMPLE-DRAW current-thread: ~a" (bt:current-thread))
      (log:info "bounding rect: x1: ~a, y1: ~a, x2: ~a, y2: ~a" x1 y1 x2 y2)
      (medium-clear-area medium x1 y1 x2 y2)
      ;; Draw a Material-ish 'Card' inset from the window
      ;; region by 10px
      (let ((outer-card-path (skia-core::path-round-rectangle
                      (+ x1 10) (+ y1 10) 1060 700 5 5))
            (inner-card-path (skia-core::path-round-rectangle
                              (+ x1 650) (+ y1 10) 400 650 5 5))
            (shadow-paint (skia-core::make-paint :color #xffeeeeee
                                                 :anti-alias t)))
        (unwind-protect
             (progn
               (canvas::draw-shadowed-path outer-card-path shadow-paint 5
                                           :canvas (medium-skia-canvas medium))

               (draw-circle* medium 50 50 25
                             :ink (alexandria:random-elt
                                   (make-contrasting-inks 8)))
               (draw-point* medium 100 100 :ink +black+ :line-thickness 8)
               (draw-text* medium "(100,100)" 100 100
                           :text-style (make-text-style :sans-serif :roman 32)
                           :ink +red+)
               (draw-line* medium 50 50 500 500 :ink +red+ :line-thickness 4)
               (draw-point* medium 50 50 :ink +green+ :line-thickness 8)
               (draw-point* medium 500 500 :ink +red+ :line-thickness 8)
               ;; Test opacity- will we see the red line undneath??
               (draw-circle* medium 500 500 100
                             :ink (clim-internals::make-rgba-color
                                   0.4 0.8 0.5 0.8))

               ;; Make inner card at higher elevation from outer
               (canvas::draw-shadowed-path inner-card-path shadow-paint 10
                                           :canvas (medium-skia-canvas medium))

               ;; Make a text header for inner card
               (draw-text* medium "Card Header" 700 60
                           :text-style (make-text-style :sans-serif :roman 48))
               (medium-finish-output sheet))

        (skia-core::destroy-paint shadow-paint)
        (skia-core::destroy-path outer-card-path))))))

;;XXX REMEMBER: all drawing calls must be submitted to the main SDL
;;rendering thread. If not, you'll just get a black window.
;;XXX Not so sure about this actually. Maybe just the canvas::flush and
;;swap buffer calls need to be on main SDL thread???
(mcclim-sdl2::define-sdl2-request do-simple-draw (sheet)
  (simple-draw *skia-sheet*))

(defmethod handle-event ((sheet skia-app-sheet) event)
  (log:info "Unhandled event ~s has arrived." (class-name (class-of event))))

(defmethod handle-event ((sheet skia-app-sheet) (event window-configuration-event))
  ;; FIXME resize-sheet may call port-set-window-geometry and then we will
  ;; receive this event back again. currently core's handler uses a flag
  ;; *configuration-event-p* to inhibit this behavior, but we need to come up
  ;; with something better. perhaps we should handle differently size-changed
  ;; and resize from sdl events.
  (let ((climi::*configuration-event-p* sheet))
    (resize-sheet sheet
                  (climi::window-configuration-event-width event)
                  (climi::window-configuration-event-height event))
    (repaint-sheet sheet +everywhere+)))

(defmethod handle-event ((sheet skia-app-sheet) (event window-manager-delete-event))
  (log:info "DELETE. sheet: ~a. (sheet-direct-mirror sheet) ==> ~a"
            sheet (sheet-direct-mirror sheet))
  (log:info "DELETE. current-thread: ~a" (bt:current-thread))
  ;;This works: I think by avoiding going through the SDL request
  ;;mechanism. That should be okay because event handlers should be being called
  ;;on the SDL thread anyways (I think...)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (%destroy-sdl2-opengl-skia-mirror mirror)))

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


(defun nuke-state ()
  (when *skia-sheet* (destroy-mirror *skia-port* *skia-sheet*) (setf *skia-sheet* nil))
  (when *skia-mirror* (destroy-mirror *skia-port* *skia-mirror*) (setf *skia-mirror* nil))
  (when *skia-port* (destroy-port *skia-port*) (setf *skia-port* nil)))

(comment

  (load-test-font)

  (defparameter *skia-port* (find-port :server-path :sdl2-skia-ttf))
  (defparameter *skia-sheet* (make-instance 'skia-app-sheet :port *skia-port*))
  (defparameter *skia-sheet* (open-skia-sheet :sdl2 t))

  (open-skia-sheet :sdl2-skia-ttf t)
  (let ((result (do-simple-draw *skia-sheet* :synchronize t)))
    (if (typep result 'condition)
        (error result)
        'done))
  (simple-draw *skia-sheet*)
  (sheet-direct-mirror *skia-sheet*)
  (close-skia-sheet *skia-sheet*)
  (nuke-state)
  (destroy-port (find-port :server-path :sdl2-skia-ttf))
)
