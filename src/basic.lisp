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

(defun expand-handler-for-core-event (sdl-event event-type params forms)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    `(let (,@(sdl2::unpack-event-params sdl-event
                                       event-type
                                       (nreverse parameter-pairs)))
       ,@forms)))

(defmacro define-sdl2-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(expand-handler-for-core-event event event-type event-params handler-body)))

(defun expand-handler-for-sdl2-request (event-type params forms)

  )

;; Convenience macro for safely communicating with the main thread
;; This macro registers an user event type, defines a function ~request-fn-name~
;; that queues event of that user event type and a method on ~handle-sdl2-event~ that
;; implements the body. In single-threaded-processing mode handler is called directly.
(defmacro define-sdl2-request (request-fn-name (&rest event-params &key synchronize) &body handler-body)
  (let ((user-event-type (alx:make-keyword request-fn-name))
        (event-sym (alx:make-gensym "event-")))
    `(progn
       ;; Register the new user event type with the SDL2 event type registry
       (sdl2:register-user-event-type ,user-event-type)

       ;; Create function to push the event onto the SDL2 event queue
       ;; where the processing loop is running on the main thread
       (defun ,request-fn-name (,@event-params &key synchronize)
         (sdl2:push-event ,user-event-type))

       ;; Create handler for above user event type
       (defmethod handle-sdl2-event ((event-type (eql ,user-event-type)) ,event-sym)
         ,(expand-handler-for-core-event event-sym user-event-type event-params handler-body)))))

;; This implements semantics of process-next-event but without distributing
;; the event - there is no need for the port argument.
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

(define-sdl2-handler (event :quit) ()
  (log:info "Quit requested... ~a"
            (if *quit-stops-the-port-p* "signaling" "ignoring"))
  (when *quit-stops-the-port-p*
    (signal 'sdl2-exit-port)))

(define-sdl2-handler (event :foobar) (:data1 d1 :data2 d2)
  (log:info ":foobar requested... d1: ~a, d2: ~a ~%" d1 d2))

(define-sdl2-request sdl2-exit-port ()
  (signal 'sdl2-exit-port))

;; This function should be called with synchronization as a timeout - it will
;; return :pong only when the event is processed (so the loop is processing).
(define-sdl2-request sdl2-ping ()
  :pong)

;; This function is for testing.
;; WARNING this will freeze the event loop for requested amount of time.
(define-sdl2-request sdl2-delay (ms)
  (sdl2:delay ms)
  :done)

