(in-package :clim-sdl2-demo/001-port-event-loop)

(defvar *current-port* nil)

(defun ensure-current-port ()
  (unless *current-port*
    (let ((sdl2-port (make-instance 'clim-sdl2::sdl2-port)))
      (setf *current-port* sdl2-port)
      sdl2-port)))

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

  (run-form (clim-sdl2::sdl2-ping))
  (run-form (clim-sdl2::sdl2-ping :synchronize t))
  (run-form (clim-sdl2::sdl2-delay 1000))
  (run-form (clim-sdl2::sdl2-delay 1000 :synchronize t))
  (run-form (clim-sdl2::sdl2-delay 1000 :synchronize 500))
  (run-form (clim-sdl2::sdl2-delay 1000 :synchronize 2000)))

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
