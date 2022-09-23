(in-package #:skia-core)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

(defun %display-info (&optional (display-index 0))
    (cffi:with-foreign-objects ((ddpi :float)
                                (hdpi :float)
                                (vdpi :float))
      (sdl2-ffi.functions:sdl-get-display-dpi display-index ddpi hdpi vdpi)
        (let* ((rect (sdl2:get-display-bounds display-index))
               (w (sdl2:rect-width rect))
               (h (sdl2:rect-height rect))
               (ddpi (cffi:mem-ref ddpi :float))
               (hdpi (cffi:mem-ref hdpi :float))
               (vdpi (cffi:mem-ref vdpi :float))
               (display-inches-w (/ w hdpi))
               (display-inches-h (/ h vdpi))
               (display-mm-w (* 25.4s0 (/ w hdpi)))
               (display-mm-h (* 25.4s0 (/ h vdpi))))
          `(:diagonal-dpi ,ddpi :horizontal-dpi ,hdpi :vertical-dpi ,vdpi
            :display-w ,w :display-h ,h
            :display-inches-w ,display-inches-w :display-inches-h ,display-inches-h
            :display-mm-w ,display-mm-w :display-mm-h ,display-mm-h) )))
(comment
  (defvar *display-info* nil)
  (unwind-protect
       (progn
         (mcclim-sdl2::%init-sdl2)
         (setf *display-info* (%display-info)))
    (mcclim-sdl2::%quit-sdl2))
  ;;==>
  (:DIAGONAL-DPI 153.27798 :HORIZONTAL-DPI 153.21497 :VERTICAL-DPI 153.42007
   :DISPLAY-W 2256 :DISPLAY-H 1504
   :DISPLAY-INCHES-W 14.72441 :DISPLAY-INCHES-H 9.80315
   :DISPLAY-MM-W 374.0 :DISPLAY-MM-H 249.00002)


)

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

;; From borodust's alien-works
(defun read-file-into-shareable-vector (location
                                        &key
                                          ((:into provided-shareable-vector))
                                          offset ((:size provided-size)))
  (when (and provided-shareable-vector
             provided-size
             (> provided-size (length provided-shareable-vector)))
    (error "Provided size is smaller than length of provided shareable vector"))
  (with-open-file (stream location :direction :input
                                   :element-type '(unsigned-byte 8))
    (let* ((file-size (file-length stream))
           (offset (if (> file-size 0)
                       (mod (or offset 0) file-size)
                       0))
           (rest-file-size (- file-size offset))
           (calculated-size
             (min rest-file-size
                  (or provided-size rest-file-size)
                  (or (and provided-shareable-vector (length provided-shareable-vector))
                      rest-file-size))))
      (when (> (+ offset (or provided-size 0)) file-size)
        (error "Sum of offset and provided size is greater than size of the ~A: got ~A, expected no more than ~A"
               location (+ offset calculated-size) file-size))
      (file-position stream offset)
      (let* ((out (if provided-shareable-vector
                      provided-shareable-vector
                      (cffi:make-shareable-byte-vector calculated-size))))
        (read-sequence out stream :start 0 :end calculated-size)
        (values out file-size)))))

(defun try-static-vector-pointer (data)
  (ignore-errors
   (static-vectors:static-vector-pointer data)))


(defun try-shareable-vector-pointer (data)
  (ignore-errors
   (cffi:with-pointer-to-vector-data (ptr data)
     ptr)))


(defmacro with-pinned-array-pointer ((ptr data &key try-pinned-copy)
                                     &body body)
  (alx:with-gensyms (body-fu tmp-vec)
    (alx:once-only (data)
      `(flet ((,body-fu (,ptr)
                ,@body))
         (alx:if-let (,ptr (try-static-vector-pointer ,data))
           (funcall #',body-fu ,ptr)
           (if (try-shareable-vector-pointer ,data)
               (cffi:with-pointer-to-vector-data (,ptr ,data)
                 (funcall #',body-fu ,ptr))
               ,(if try-pinned-copy
                    `(sv:with-static-vector (,tmp-vec
                                             (length ,data)
                                             :element-type (array-element-type ,data)
                                             :initial-contents ,data)
                       (funcall #',body-fu (sv:static-vector-pointer ,tmp-vec)))
                    '(error "Failed to pin the array"))))))))
