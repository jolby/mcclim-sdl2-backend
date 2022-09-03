(in-package #:mcclim-sdl2)

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


(define-enumval-extractor color-type-enum %skia:sk-color-type)
(define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(define-enumval-extractor clip-op-enum %skia:sk-clip-op)

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
