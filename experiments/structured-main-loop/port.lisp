(in-package #:mcclim-sdl2)

;;; This variable is bound in the context of event processing so it is
;;; possible to reference the port making it a requiered argument for
;;; clim-agnostic handlers.
(defvar *sdl2-port*)

;;; SDL2 is always expecting to run in a single thread. Moreover a process may
;;; have only a one context. Ergo SDL2 thread is a synchronized singleton.

(defclass sdl2-port (resource-manager-mixin basic-port)
  ((main-thread
    :allocation :class
    :initform nil
    :accessor main-thread)
   (main-thread-lock
    :allocation :class
    :initform (clim-sys:make-lock "McCLIM SDL2 lock")
    :reader main-thread-lock)
   (sdl2-clients
    :allocation :class
    :initform '()
    :accessor sdl2-clients)))

(defmethod print-object ((port sdl2-port) stream)
  (print-unreadable-object (port stream :type t :identity t)
    (if (main-thread port)
        (format stream "(active)")
        (format stream "(inactive)"))))

(defmethod find-port-type ((port (eql :sdl2)))
  (values 'sdl2-port (constantly nil)))

(defmethod initialize-instance :after ((port sdl2-port) &key server-path)
  (declare (ignore server-path))
  (clim-sys:with-lock-held ((main-thread-lock port))
    (%init-port port)
    (push port (sdl2-clients port))))

(defmethod destroy-port ((port sdl2-port))
  ;; Close the SDL2 loop only after the last port is destroyed.
  (when (member port (sdl2-clients port))
    (clim-sys:with-lock-held ((main-thread-lock port))
      (alx:removef (sdl2-clients port) port)
      (when (null (sdl2-clients port))
        (free-resources port)
        (%quit-port port))))
  port)

(defmethod restart-port ((port sdl2-port))
  (clim-sys:with-lock-held ((main-thread-lock port))
    (free-resources port)
    (%quit-port port)
    (%init-port port))
  port)

(define-condition sdl2-clim () ())
(define-condition sdl2-error (sdl2-clim simple-error) ())
(define-condition sdl2-exit-port (sdl2-clim) ())



;;; User-requested exit - this event is usually signaled when the last window
;;; is closed (in addition to windowevent/close), but may be also prompted by
;;; the window manager or an interrupt. The application may ignore it.
(defvar *quit-stops-the-port-p* nil)
(define-sdl2-handler (event :quit) ()
  (log:info "Quit requested... ~a"
            (if *quit-stops-the-port-p* "signaling" "ignoring"))
  (when *quit-stops-the-port-p*
    (signal 'sdl2-exit-port)))

(define-sdl2-request sdl2-exit-port ()
  (signal 'sdl2-exit-port))

;;; This function should be called with synchronization as a timeout - it will
;;; return :pong only when the event is processed (so the loop is processing).
#+ (or) (sdl2-ping :synchronize 0.1)
(define-sdl2-request sdl2-ping ()
  :pong)

;;; This function is for testing.
;;; WARNING this will freeze the event loop for requested amount of time.
(define-sdl2-request sdl2-delay (ms)
  (sdl2-ffi.functions:sdl-delay ms)
  :done)

;;; This function is for testing.
;;; WARNING this function will signal an error in the event loop.
(define-sdl2-request sdl2-error (message)
  (error message))
