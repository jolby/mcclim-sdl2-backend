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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %extract-parameter-pairs (params)
    (log:info "params: ~s " params)
    (let ((parameter-pairs nil))
      (do ((keyword params (if (cdr keyword)
                               (cddr keyword)
                               nil)))
          ((null keyword))
        (push (list (first keyword) (second keyword)) parameter-pairs))
      (nreverse parameter-pairs)))

  (defun %kw-pair-to-binding-form (k v)
    `(,(intern (format nil "~a" k)) ,v))

  (defun %unpack-user-event-params (params)
    (log:info "params: ~s" params)
    (loop :for (k v) :on params :by #'cddr :while v :collect (%kw-pair-to-binding-form k v)))

  (defun %expand-plist-to-let-bindings-context (plist-arg-names plist-place-name body-forms)
    (flet ((expand-binding-form (varname)
             `(,varname (getf ,plist-place-name ,(alexandria:make-keyword varname)))))
      `(let ,(mapcar #'expand-binding-form plist-arg-names)
         ,@body-forms)
      ))

  (comment
    (%expand-plist-to-let-bindings-context '(a z w) 'blah-sym '((format t "w: ~a" w)))
    )

  (defun %expand-bindings-context-for-core-event (sdl-event event-type params forms)
    (let ((parameter-pairs (%extract-parameter-pairs params)))
      (log:info "extracted parameter pairs: ~s" parameter-pairs)
      `(let (,@(sdl2::unpack-event-params sdl-event
                                          event-type
                                          parameter-pairs))
         ,@forms)))

  (defun %expand-bindings-context-for-user-event (sdl-event event-type params body-forms)
    (let ((user-data-sym (alx:make-gensym "user-data-"))
          (user-data-varnames (alx:make-gensym "user-data-varnames")))
      `(let* ((,user-data-sym (sdl2::get-user-data (sdl2::c-ref ,sdl-event sdl2-ffi:sdl-event :user :code)))
              (,user-data-varnames (loop :for (k nil) :on ,user-data-sym :by #'cddr :collect k)))
         ;; (destructuring-bind (&key ,@(%unpack-user-event-params params)) ,user-data-sym
         ;;   ,@forms)
         ,(%expand-plist-to-let-bindings-context user-data-varnames user-data-sym body-forms)
         )))

  )

(comment

  (%kw-pair-to-binding-form :A 24)
  (%expand-bindings-context-for-user-event '(:user-event) :sdl-exit-port '(:foo 1 :bar 2) '((format t "Got foo: ~a ~%" foo)))

  (%extract-parameter-pairs '(:foo 22 :bar 24))
  (%unpack-user-event-params '(:foo 22 :bar 24))

  )

(defmacro define-sdl2-event-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(%expand-bindings-context-for-core-event event event-type event-params handler-body)))

(defmacro define-sdl2-user-event-handler ((event event-type) (&rest event-params) &body handler-body)
  `(defmethod handle-sdl2-event ((event-type (eql ,event-type)) ,event)
     ,(%expand-bindings-context-for-user-event event event-type event-params handler-body)))

(defmacro define-sdl2-request (request-fn-name (&rest event-params) &body handler-body)
  (let ((user-event-type (alx:make-keyword request-fn-name))
        (event-sym (alx:make-gensym "event-"))
        (sync-sym (alx:make-gensym "sync-")))
  `(progn
     (sdl2:register-user-event-type ,user-event-type)

     (defun ,request-fn-name (&rest event-params &key (synchronize nil synchronize-p) &allow-other-keys)
       (remf event-params :synchronize)
       (cond ((null synchronize) (apply #'push-event ,user-event-type event-params))
             ((numberp synchronize) (apply #'push-event ,user-event-type event-params))
             (t (apply #'push-event ,user-event-type event-params))))

     (define-sdl2-user-event-handler (event ,user-event-type) (,@event-params)
       ,@handler-body)
    )
    ))
(comment
(define-sdl2-request change-window-size (:window window :x x :y y :w w :h h)
    (sdl2-ffi.functions:sdl-set-window-position window x y)
    (sdl2-ffi.functions:sdl-set-window-size window w h))
  )

(define-sdl2-event-handler (event :quit) ()
      (log:info "Quit requested... ~a"
                (if *quit-stops-the-port-p* "signaling" "ignoring"))
      (when *quit-stops-the-port-p*
        (signal 'sdl2-exit-port)))

(define-sdl2-event-handler (ev :windowevent) (:event event :window-id window-id :timestamp timestamp :data1 data1 :data2 data2)
  ;; (alexandria:when-let ((sheet (get-mirror-sheet *sdl2-port* window-id)))
  ;;   (let ((event-key (autowrap:enum-key '(:enum (windowevent.event)) event)))
  ;;     (handle-sdl2-window-event event-key sheet timestamp data1 data2)))
  (log:info "ev: ~a, w-id: ~a ts: ~a" event window-id timestamp)
  )

;;SDL Requests
(defun push-event (event &rest args)
  (if args
      (log:info "would call sdl2:push-user-event: ~s with user-data: ~s ~%" event args)
      (log:info "would call sdl2:push-user-event ~s with no user-data ~%" event)))

(progn
  ;; Register the :sdl-exit-port event type with the SDL2 event type registry
  (sdl2:register-user-event-type :sdl-exit-port)

  (defun sdl-exit-port (&rest args &key (synchronize nil synchronize-p) &allow-other-keys)
    (remf args :synchronize)
    (cond ((null synchronize) (apply #'push-event :sdl-exit-port args))
          ((numberp synchronize) (apply #'push-event :sdl-exit-port-sync-num args))
          (t (apply #'push-event :sdl-exit-port-sync-1 args)))

    )

  )



(comment
  (sdl-exit-port :foo 1 :bar 2)
  (sdl-exit-port :synchronize .5 :foo 1 :bar 2)
  (sdl-exit-port)
  )
