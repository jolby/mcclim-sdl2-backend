(in-package #:mcclim-sdl2)

(define-enumval-extractor color-type-enum %skia:sk-color-type)
(define-enumval-extractor surface-origin-enum %skia:gr-surface-origin)
(define-enumval-extractor clip-op-enum %skia:sk-clip-op)

(define-condition skia-error (error)
  ((string :initarg :string :initform nil :accessor skia-error-string))
  (:report (lambda (c s)
             (with-slots (string) c
               (format s "Skia Error: ~A" string)))))

(defclass skia-context ()
  ((%native-interface-sp :initform nil :initarg :native-interface-sp :accessor native-interface-sp)
   (%gl-context-sp  :initform nil :initarg :gl-context-sp :accessor gl-context-sp)
   (%framebuffer-info :initform nil :initarg :framebuffer-info :accessor framebuffer-info)
   (%backend-render-target :initform nil :initarg :backend-render-target :accessor backend-render-target)
   (%surface-sp :initform nil :initarg :surface-sp :accessor surface-sp)
   (%canvas :initform nil :initarg :canvas :accessor canvas)))

(defmacro define-smart-pointer-functions ((name spec))
  (alx:with-gensyms(sp)
    (let ((deref-name (intern (format nil "DEREF-~a-SP" (symbol-name name))))
          (unref-name (intern (format nil "UNREF-~a-SP" (symbol-name name)))))
    `(progn
       (defun ,deref-name (,sp)
         (check-nullptr
          (%skia:get :const ,spec ,sp)))
       ;;(%skia::unref :const '(claw-utils:claw-pointer %skia::sk-ref-cnt-base) %surface-sp)
       (defun ,unref-name (,sp)
         (%skia:unref :const '(claw-utils:claw-pointer %skia::sk-ref-cnt-base) ,sp))))))

(define-smart-pointer-functions (native-interface '(:pointer %skia:sk-sp<const+gr-gl-interface>)))
(define-smart-pointer-functions (gl-context '(:pointer %skia:sk-sp<gr-direct-context>)))
(define-smart-pointer-functions (surface '(:pointer %skia:sk-sp<sk-surface>)))

(defun interface-of (sk-ctx)
  (deref-native-interface-sp (native-interface-sp sk-ctx)))

(defun gl-context-of (sk-ctx)
  (deref-gl-context-sp (native-interface-sp sk-ctx)))

(defun surface-of (sk-ctx)
  (deref-surface-sp (surface-sp sk-ctx)))

(defun make-native-gl-interface ()
  (let ((native-interface-sp (check-nullptr
                              (%skia:gr-gl-make-native-interface
                               '(:pointer %skia:sk-sp<const+gr-gl-interface>)
                               (iffi:intricate-alloc '%skia:sk-sp<const+gr-gl-interface>)))))
    ;;It's possible for the skia wrapper to return a skia sp wrapped around a null pointer
    ;;we check for that before returning the wrapped object
    (deref-native-interface-sp native-interface-sp)
    native-interface-sp))

(defun make-gl-context (gl-interface-sp)
  (let ((gl-context-sp (check-nullptr
                              (%skia:gr-direct-context+make-gl
                               '(:pointer %skia:sk-sp<gr-direct-context>) (iffi:intricate-alloc '%skia:sk-sp<gr-direct-context>)
                               '(:pointer %skia:sk-sp<const+gr-gl-interface>) gl-interface-sp))))
    ;;It's possible for the skia wrapper to return a skia sp wrapped around a null pointer
    ;;we check for that before returning the wrapped object
    (deref-gl-context-sp gl-context-sp)
    gl-context-sp))

(defun make-framebuffer-info (&key
                                (framebuffer-id (gl:get-integer :draw-framebuffer-binding))
                                (framebuffer-format (cffi::foreign-enum-value '%gl::enum :rgba8)))
  (let ((framebuffer-info  (iffi:make-intricate-instance '%skia:gr-gl-framebuffer-info)))
    (iffi:with-intricate-slots %skia:gr-gl-framebuffer-info
        ((fbo-id %skia:f-fboid) (format %skia:f-format)) framebuffer-info
      (setf fbo-id framebuffer-id
            format framebuffer-format))
    framebuffer-info))

(defun make-backend-render-target-gl (device-width device-height
                                      &key
                                        (sample-count 0)
                                        (stencil-bits 8)
                                        (framebuffer (make-framebuffer-info)))
  (iffi:make-intricate-instance
   '%skia:gr-backend-render-target
   :int device-width
   :int device-height
   :int sample-count
   :int stencil-bits
   '(:pointer %skia:gr-gl-framebuffer-info) framebuffer))

;;;
;;; Skia Surface (SkSurface)
;;;
(defun make-surface-image-snapshot (surface)
  (let* ((sk-image-sp (iffi:intricate-alloc '%skia:sk-sp<sk-image>)))
    (%skia:make-image-snapshot '(claw-utils:claw-pointer %skia:sk-sp<sk-image>) sk-image-sp
                               '(claw-utils:claw-pointer %skia:sk-surface) surface)))

(defun make-surface-from-backend-render-target (context render-target surface-props)
  (iffi:with-intricate-instance (color-space-sp %skia:sk-sp<sk-color-space>)
    (%skia:sk-surface+make-from-backend-render-target
     '(:pointer %skia:sk-sp<sk-surface>) (iffi:intricate-alloc '%skia:sk-sp<sk-surface>)
     '(:pointer %skia:gr-recording-context) context
     '(:pointer %skia:gr-backend-render-target) render-target
     '%skia:gr-surface-origin (surface-origin-enum :bottom-left-gr-surface-origin)
     '%skia:sk-color-type (color-type-enum :rgba-8888-sk-color-type)
     '(:pointer %skia:sk-sp<sk-color-space>) color-space-sp
     '(:pointer %skia:sk-surface-props) surface-props
     '%skia:sk-surface+render-target-release-proc (cffi:null-pointer)
     '%skia:sk-surface+release-context (cffi:null-pointer))))

(defun make-skia-context (width height)
  (let* ((native-interface-sp (make-native-gl-interface))
         (gl-context-sp (make-gl-context native-interface-sp))
         (gl-context (deref-gl-context-sp gl-context-sp))
         (framebuffer-info (make-framebuffer-info))
         (backend-render-target (make-backend-render-target-gl width height))
         (surface-sp (iffi:with-intricate-instances ((surface-props %skia:sk-surface-props))
                       (make-surface-from-backend-render-target gl-context
                                                                 backend-render-target
                                                                 surface-props)))
         (canvas (%skia:get-canvas '(:pointer %skia:sk-surface) (deref-surface-sp surface-sp)))
         (skia-ctx (make-instance 'skia-context :native-interface-sp native-interface-sp
                                                :gl-context-sp gl-context-sp :framebuffer-info framebuffer-info
                                                :backend-render-target backend-render-target :surface-sp surface-sp
                                                :canvas canvas)))
    skia-ctx))
