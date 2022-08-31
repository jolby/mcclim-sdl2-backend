(in-package :clim-sdl2)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

(defvar *initialized-p* nil)
(defvar *initialized-cv* (clim-sys:make-condition-variable))
(defvar *initialized-lock* (clim-sys:make-lock "SDL2 init cv lock"))

(defun %init-sdl2 ()
  (unless *initialized-p*
    (sdl2:init* '(:everything))
    ;; (sdl2:init :everything)
    (setf *initialized-p* t)
    (clim-sys:condition-notify *initialized-cv*)
    (log:info "Hello!")))

(defun %quit-sdl2 ()
  (when *initialized-p*
    (log:info "Good bye.")
    (setf *initialized-p* nil)
    (sdl2:quit*)
    ;; (sdl2:quit)
    ))

(defun %read-sdl2 (event timeout)
  (let ((rc (if (null timeout)
                (sdl2-ffi.functions:sdl-wait-event event)
                (sdl2-ffi.functions:sdl-wait-event-timeout event timeout))))
    (= rc 1)))

(defvar *future-result-keyword* :clim-sdl2.future-result)
(defvar *last-future-result* nil)

(defun %get-user-event-id (sdl2-event)
  (sdl2::c-ref sdl2-event sdl2-ffi:sdl-event :user :code))

(defun %get-user-data-for-event (sdl2-event)
  (sdl2::get-user-data (%get-user-event-id sdl2-event)))

(defun %set-user-data-for-event (sdl2-event user-data)
  (if (zerop (%get-user-event-id sdl2-event))
      (let ((new-event-id (sdl2::add-user-data user-data)))
        (setf
         (sdl2::c-ref sdl2-event sdl2-ffi:sdl-event :user :code)
         new-event-id))
      ;; This isn't really supported by the sdl2 user data api but might be useful
      ;; for testing user events
      (setf (gethash (%get-user-event-id sdl2-event) sdl2::*user-events*) user-data)))

(defun %get-future-result-for-event (sdl2-event)
  (if-let ((user-data (%get-user-data-for-event sdl2-event)))
    (getf user-data *future-result-keyword*)))

;;;; Event handling

;; The function ~handle-sdl2-event~ is used to implement event handling. When
;; function that reads events calls ~handle-sdl2-event~ to dispatch on them.
;; Methods for this function are defined with a macro ~define-sdl2-handler~.
(defgeneric handle-sdl2-event (event-type event))

(defgeneric handle-sdl2-window-event (event-key sheet timestamp data1 data2)
  (:method (event-key sheet timestamp data1 data2)
    (log:debug "Unhandled window event ~s." event-key)))


(defmethod handle-sdl2-event ((any-event-type t) (any-event t))
  (log:warn "Unknown event type: ~a" any-event-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interleave (&rest lsts)
    (apply 'mapcan 'list lsts))

  (defun %plist-keywords (plist)
    (loop :for (k nil) :on plist :by #'cddr :collect k))

  (defun %make-plist-from-binding-names (binding-names)
    (interleave (mapcar #'(lambda (x) (alx:make-keyword x)) binding-names) binding-names))

  (defun %make-keyword-binding-pairs (binding-names)
    (loop :for bn :in binding-names collect (list (alx:make-keyword bn) bn)))

  (defun %expand-binding-form-from-plist (varname bindings-plist-sym)
    `(,varname (getf ,bindings-plist-sym ,(alx:make-keyword varname))))

  (defun %expand-binding-pairs-from-plist (binding-names bindings-plist-sym)
    (mapcar #'(lambda (varname) (%expand-binding-form-from-plist varname bindings-plist-sym)) binding-names))

  (defun %expand-plist-to-let-bindings-context (binding-names bindings-plist-sym  body-forms)
    `(let ,(%expand-binding-pairs-from-plist binding-names bindings-plist-sym)
       ,@body-forms))

  (defun %expand-lexical-context-for-core-event-handler (sdl-event event-type params body-forms)
    (let ((parameter-pairs (%make-keyword-binding-pairs params)))
      (log:info "extracted parameter pairs: ~s" parameter-pairs)
      `(let (,@(sdl2::unpack-event-params sdl-event
                                          event-type
                                          parameter-pairs))
         ,@body-forms)))

  (defun %expand-lexical-context-for-user-event-handler (sdl-event event-type binding-names body-forms)
    (let ((user-data-sym (alx:make-gensym "user-data-")))
      `(let* ((,user-data-sym (sdl2::get-user-data (sdl2::c-ref ,sdl-event sdl2-ffi:sdl-event :user :code))))
         ,(%expand-plist-to-let-bindings-context binding-names user-data-sym body-forms))))

  )

(defmacro define-sdl2-core-event-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(%expand-lexical-context-for-core-event-handler event event-type event-params handler-body)))

(defmacro define-sdl2-user-event-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(%expand-lexical-context-for-user-event-handler event event-type event-params handler-body)))

;;SDL Requests
(defun push-event (event args-plist)
  (sdl2:push-user-event event args-plist))

(defun push-event-sync-wait (event args-plist &key timeout)
  (let* ((fr (%make-future-result))
         (lock (fres-lock fr))
         (cv (fres-completion-cv fr))
         (cv-return-code nil)
         ;;Convert our timeout expressed in milliseconds to
         ;;seconds as expected by bt:condition-wait
         (timeout (if timeout (/ timeout 1000))))
    (setf (getf args-plist *future-result-keyword*) fr)
    (sdl2:push-user-event event args-plist)
    (bt:with-lock-held (lock)
      (setf cv-return-code (bt:condition-wait cv lock :timeout timeout)))
    ;;XXX debug
    ;; (setf *last-future-result* fr)
    (fres-value fr)))

;; This macro registers an user event type, defines a function
;; ~request-fn-name~ that queues event of that type and a method on
;; ~handle-sdl2-event~ that implements the body. In single-processing mode
;; handler is called directly.

;; The arglist is similar as with the "system events" but we expand them by a
;; different mechanism. Request lambda list is (,@args &key synchronize). The
;; arguments provided to that function are bundled into a plist that is attached
;; to the sdl event as user-data. The keyword argument allows to wait for the
;; result returned by the handler. On the handler side, the arguments bound to
;; the plist are expanded into let bindings that surround the handler body.
(defmacro define-sdl2-request (request-fn-name (&rest event-params) &body handler-body)
  (let ((user-event-type (alx:make-keyword request-fn-name)))
  `(progn
     (sdl2:register-user-event-type ,user-event-type)

     (defun ,request-fn-name (,@event-params &key (synchronize nil synchronize-p))
       (declare (ignorable synchronize synchronize-p))
       (let ((event-params-plist (list ,@(%make-plist-from-binding-names event-params))))
         (cond ((null synchronize) (push-event ,user-event-type event-params-plist))
               ((numberp synchronize) (push-event-sync-wait ,user-event-type event-params-plist
                                                            :timeout synchronize))
               (t (push-event-sync-wait ,user-event-type event-params-plist :timeout nil)))))

     (define-sdl2-user-event-handler (event ,user-event-type) (,@event-params)
       ,@handler-body))))



;;;; =========================================================================
;;;;
;;;; Basic Concrete Handlers/Requests to test event loop functionality
;;;;

;; User-requested exit - this event is usually signaled when the last window
;; is closed (in addition to windowevent/close), but may be also prompted by
;; the window manager or an interrupt. The application may ignore it.
(defvar *quit-stops-the-port-p* nil)

(define-sdl2-core-event-handler (event :quit) ()
      (log:info "Quit requested... ~a"
                (if *quit-stops-the-port-p* "signaling" "ignoring"))
      (when *quit-stops-the-port-p*
        (signal 'sdl2-exit-port)))

(define-sdl2-request sdl2-exit-port ()
  (signal 'sdl2-exit-port))

(define-sdl2-request sdl2-event-handler-error (msg)
  (signal 'sdl2-event-handler-error :report msg))

;; This function should be called with synchronization as a timeout - it will
;; return :pong only when the event is processed (so the loop is processing).
(define-sdl2-request sdl2-ping ()
  :pong)

;; This function is for testing.
;; WARNING this will freeze the event loop for requested amount of time.
(define-sdl2-request sdl2-delay (ms)
  (sdl2:delay ms)
  :done)


;;;; =========================================================================
;;;;
;;;; Window event handling
;;;;
(define-sdl2-core-event-handler (ev :windowevent) (event window-id timestamp data1 data2)
  (alexandria:when-let ((sheet (get-mirror-sheet *sdl2-port* window-id)))
    (let ((event-key (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id event)))
      (handle-sdl2-window-event event-key sheet timestamp data1 data2))))

(defmethod handle-sdl2-window-event ((key (eql :close)) sheet stamp d1 d2)
  (log:info "Destroying a window.")
  (clim:destroy-mirror (clim:port sheet) sheet))

;; Between pressing quit and the actual close the user may still use the
;; window for a brief period, so i.e a window event may sneak in. The window
;; event handler should ignore events to windows that are already destroyed.
(defmethod handle-sdl2-window-event ((key (eql :exposed)) sheet stamp d1 d2)
  (log:info "Repainting a window.")
  ;; The call to GET-WINDOW-SURFACE is for side the effect, namely to ensure
  ;; that the surface is allocated (to be able to call UPDATE-WINDOW).
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2:get-window-surface window)
      (sdl2:update-window window))))
