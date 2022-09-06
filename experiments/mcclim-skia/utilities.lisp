(in-package #:skia-core)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

(defun enumval (enum value)
  (if (integerp value)
      (cffi:foreign-enum-keyword enum value)
      (cffi:foreign-enum-value enum value)))

(define-compiler-macro enumval (&whole whole enum value)
  (alx:if-let ((quoted (when (and (listp enum)
                                  (eq 'quote (first enum)))
                         (second enum))))
    (cond
      ((keywordp value)
       (alx:if-let ((value (cffi:foreign-enum-value quoted value :errorp nil)))
         value
         whole))
      ((integerp value)
       (alx:if-let ((value (cffi:foreign-enum-keyword quoted value :errorp nil)))
         value
         whole))
      (t whole))
    whole))

(defmacro define-enumval-extractor (name enum)
  `(progn
     (defun ,name (value)
       (enumval ',enum value))
     (define-compiler-macro ,name (value)
       `(enumval ',',enum ,value))))

(define-condition skia-error (error)
  ((string :initarg :string :initform nil :accessor skia-error-string))
  (:report (lambda (c s)
             (with-slots (string) c
               (format s "Skia Error: ~A" string)))))

(defmacro check-false (form &optional err-msg)
  (alx:with-gensyms (rc)
    `(let ((,rc ,form))
       (when (not ,rc)
         (error 'skia-error :rc ,rc :string (or ,err-msg (format nil "~A can't be false!" (quote ,form)))))
       ,rc)))

(defmacro check-nil (form &optional err-msg)
  (alx:with-gensyms (v)
    `(let ((,v ,form))
       (if (null ,v)
           (error 'skia-error :string (or ,err-msg (format nil "~A can't be nil!" (quote ,form))))
           ,v))))

(defmacro check-nullptr (form &optional err-msg)
  (alx:with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p ,wrapper)
           (error 'skia-error :string (or ,err-msg (format nil "~A can't be null pointer!" (quote ,form))))
           ,wrapper))))
;;
;; Structured cleanups
(defvar *cleanup-context* nil)
(defvar *cleanup-context-chain* nil)
(declaim (special *cleanup-context* *cleanup-context-chain*))

(defun push-cleanup (obj cleanup-fn &optional (cleanup-error-handler nil))
  ;; (unless *cleanup-context* (error "No *cleanup-context* established. Must call this wrapped in with-cleanup-context"))
  (push (list obj cleanup-fn cleanup-error-handler) *cleanup-context*)
  obj)

(defun run-cleanup (obj cleanup-fn &optional cleanup-error-handler)
  ;; (log:info "run-cleanup: ~a ~a ~a" obj cleanup-fn cleanup-error-handler)
  (if cleanup-error-handler
      (handler-bind ((error #'(lambda (c) (funcall cleanup-error-handler c))))
        (funcall cleanup-fn obj))
      (funcall cleanup-fn obj)))

(defun run-cleanups (cleanups &optional cleanup-error-handler)
  (flet ((do-run-cleanups (cleanup-list)
           ;; (log:info "cleanups: ~a" cleanups)
           (loop :for (obj cleanup-fn cleanup-error-handler) :in cleanup-list
                 :do (progn (run-cleanup obj cleanup-fn cleanup-error-handler)))))
    (if cleanup-error-handler
        (handler-bind ((error #'(lambda (c) (funcall cleanup-error-handler c))))
          (do-run-cleanups cleanups))
        (do-run-cleanups cleanups))))

(defun log-and-return-nil-handler (c)
  (invoke-restart 'log-and-return-nil))

(defmacro with-cleanups-on-error ((&optional (default-cleanup-error-handler-fn #'log-and-return-nil-handler))
                                  &body body)
  `(progn
     (unless *cleanup-context-chain*
       (setf *cleanup-context-chain* (list)))
     (setf *cleanup-context* (list))
     (push *cleanup-context* *cleanup-context-chain*)
     (unwind-protect
          (progn
            (handler-case
                (progn
                  ,@body)
              (error (c)
                (log:error "Error inside cleanup context: ~a." c)
                (when *cleanup-context*
                  (log:warn "Running ~a cleanups" (length *cleanup-context*))
                  (run-cleanups *cleanup-context*))
                )))
       (setf *cleanup-context* (if *cleanup-context-chain* (pop *cleanup-context-chain*) nil)))))
