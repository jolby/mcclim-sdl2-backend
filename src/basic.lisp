(in-package :clim-sdl2)

(defvar *initialized-p* nil)
(defvar *initialized-cv* (clim-sys:make-condition-variable))
(defvar *initialized-lock* (clim-sys:make-lock "SDL2 init cv lock"))

;; User-requested exit - this event is usually signaled when the last window
;; is closed (in addition to windowevent/close), but may be also prompted by
;; the window manager or an interrupt. The application may ignore it.
(defvar *quit-stops-the-port-p* nil)

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


;;;; Event handling

;; The function ~handle-sdl2-event~ is used to implement event handling. When
;; function that reads events calls ~handle-sdl2-event~ to dispatch on them.
;; Methods for this function are defined with a macro ~define-sdl2-handler~.
(defgeneric handle-sdl2-event (event-type event))

(defun %extract-parameter-pairs (params)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    (nreverse parameter-pairs)))

(defun expand-handler-for-core-event (sdl-event event-type params forms)
  (let ((parameter-pairs (%extract-parameter-pairs params)))
    `(let (,@(sdl2::unpack-event-params sdl-event
                                       event-type
                                       parameter-pairs))
       ,@forms)))

(defmacro define-sdl2-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(expand-handler-for-core-event event event-type event-params handler-body)))

