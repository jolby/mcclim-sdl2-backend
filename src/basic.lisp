(in-package :clim-sdl2)

(defvar *initialized-p* nil)
(defvar *initialized-cv* (clim-sys:make-condition-variable))
(defvar *initialized-lock* (clim-sys:make-lock "SDL2 init cv lock"))

(defun %init-sdl2 ()
  (unless *initialized-p*
    (sdl2:init* '(:everything))
    (setf *initialized-p* t)
    (clim-sys:condition-notify *initialized-cv*)
    (log:info "Hello!")))

(defun %quit-sdl2 ()
  (when *initialized-p*
    (log:info "Good bye.")
    (setf *initialized-p* nil)
    (sdl2:quit*)))

(defun %read-sdl2 (event timeout)
  (let ((rc (if (null timeout)
                (sdl2-ffi.functions:sdl-wait-event event)
                (sdl2-ffi.functions:sdl-wait-event-timeout event timeout))))
    (= rc 1)))

    ;;; This implements semantics of process-next-event but without distributing
    ;;; the event - there is no need for the port argument.
(defun %next-sdl2 (wait-function timeout)
  (when (maybe-funcall wait-function)
    (return-from %next-sdl2 (values nil :wait-function)))
  (sdl2:with-sdl-event (event)
    (alx:if-let ((ev (%read-sdl2 event timeout)))
      (let ((event-type (sdl2:get-event-type event)))
        (values (handle-sdl2-event event-type event) event-type))
      (if (maybe-funcall wait-function)
          (values nil :wait-function)
          (values nil :timeout)))))
