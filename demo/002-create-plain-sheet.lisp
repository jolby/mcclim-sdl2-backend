(in-package :clim-sdl2-demo/002-create-plain-sheet)

(defvar *current-port* nil)
(defvar *plain-sheet* nil)

(defvar *glider*
  (clim:make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defclass plain-sheet (;; repainting
                       clim:immediate-repainting-mixin
                       ;; input
                       clim:immediate-sheet-input-mixin
                       ;; output
                       clim:sheet-mute-output-mixin
                       ;; geometry
                       clim:sheet-identity-transformation-mixin
                       ;; genealogy
                       clim:sheet-parent-mixin
                       clim:sheet-leaf-mixin
                       ;; windowing
                       clime:top-level-sheet-mixin
                       clim:mirrored-sheet-mixin
                       ;; the base class
                       clim:basic-sheet)
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Test Sheet"
                     :region (clim:make-rectangle* 1000 100 1400 500)))

(defun ensure-current-port ()
  (unless *current-port*
    (let ((sdl2-port (make-instance 'clim-sdl2::sdl2-port)))
      (setf *current-port* sdl2-port)))
  *current-port*)

(defun start-port-event-loop ()
  (let ((port (ensure-current-port)))
    (clim-sdl2::run-port-loop-in-main-thread port)))

(defun stop-port-event-loop ()
  (clim-sdl2::sdl2-exit-port)
  ;;Alternative:
  ;;(setf clim-sdl2::*keep-running* nil)
  )

(defmacro run-form (&body body)
  `(log:info "~a ==> ~a" (quote ,body) ,@body))

(defun port-event-loop-demo ()
  (unless *current-port*
    (error "No current sdl2-port. Use ensure-current-port before calling this."))
  (unless clim-sdl2::*is-running*
    (error "No current sdl2-port loop running. Use start-port-event-loop before calling this."))

  )

(defun run ()
  (unwind-protect
       (progn
         (start-port-event-loop)
         ;; XXX should probably have an is-running condition var
         ;; on port???
         (sleep 1)
         (port-event-loop-demo)
         (sleep 5)
         (stop-port-event-loop))
    (setf clim-sdl2::*keep-running* nil)))
