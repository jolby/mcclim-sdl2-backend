(in-package #:mcclim-skia)

(defclass sdl2-opengl-skia-mirror (sdl2-opengl-mirror)
  ((skia-context
    :initarg :skia-context
    :reader skia-context)))

(defclass opengl-mirrored-sheet-mixin (mirrored-sheet-mixin) ())

(defclass sdl2-opengl-top-level-sheet
    (top-level-sheet-mixin opengl-mirrored-sheet-mixin basic-sheet)
  ())

(defclass sdl2-opengl-window (sdl2-opengl-top-level-sheet basic-pane) ())

(defclass skia-mirrored-sheet-mixin (mirrored-sheet-mixin) ())

(defclass sdl2-skia-top-level-sheet
    (top-level-sheet-mixin skia-mirrored-sheet-mixin basic-sheet)
  ())

(defclass sdl2-skia-window (sdl2-skia-top-level-sheet basic-pane) ())

(defun %create-context-by-version (window context-version-major context-version-minor context-profile-mask)
    (sdl2:gl-set-attr :context-profile-mask context-profile-mask)
    (sdl2:gl-set-attr :context-major-version context-version-major)
    (sdl2:gl-set-attr :context-minor-version context-version-minor)
    (sdl2:gl-create-context window))

(defun %search-for-context (window context-profile-mask)
  (let ((context nil))
    (loop :for (major minor) :in `((4 6) (4 5) (4 4) (4 3)
                                   (4 2) (4 1) (4 0) (3 3))
          :until context
          :do (handler-case
                  (progn
                    (sdl2:gl-set-attr :context-profile-mask context-profile-mask)
                    (sdl2:gl-set-attr :context-major-version major)
                    (sdl2:gl-set-attr :context-minor-version minor)
                    (setf context (sdl2:gl-create-context window)))
                (error (log:error "Unable to create context: ~a.~a" major minor))))
    context))

(defun make-gl-context-for-window (window &key
                                            (shared-context nil)
                                            (context-profile-mask (autowrap:enum-value 'sdl2-ffi:sdl-glprofile :core))
                                            (context-major-version nil) (context-minor-version nil)
                                            (buffer-size 24) (double-buffer t) (depth-size 0) (stencil-size 8)
                                            (alpha-size 8) (red-size 8) (green-size 8) (blue-size 8))
  (setf cl-opengl-bindings::*gl-get-proc-address* #'sdl2::gl-get-proc-address)
  (sdl2:gl-set-attr :alpha-size alpha-size)
  (sdl2:gl-set-attr :red-size red-size)
  (sdl2:gl-set-attr :green-size green-size)
  (sdl2:gl-set-attr :blue-size blue-size)
  (sdl2:gl-set-attr :depth-size depth-size)
  (when stencil-size
    (sdl2:gl-set-attr :stencil-size stencil-size))
  (sdl2:gl-set-attr :buffer-size buffer-size)
  (sdl2:gl-set-attr :doublebuffer (if double-buffer 1 0))
  (when shared-context
    (sdl2:gl-set-attr :share-with-current-context 1))
  (let ((context (if (and context-major-version context-minor-version)
                     (%create-context-by-version window context-major-version context-minor-version context-profile-mask)
                     (%search-for-context window context-profile-mask))))
    (assert context ()
            "McCLIM mirror skia. Unable to create suitable OpenGL context.
Your machine must support at least GL 3.3")
    (values context surface)))

(defmethod realize-mirror ((port sdl2-port) (sheet sdl2-skia-window))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (window (sdl2:create-window
                    :title title :flags '(:shown :opengl) :x x :y y :w w :h h))
           ;; (gl-context (sdl2:gl-create-context window))
           (gl-context (make-gl-context-for-window window)))
      (sdl2:gl-make-current window gl-context)
      (make-instance 'sdl2-opengl-mirror :window window :gl-context gl-context))))

(defmethod destroy-mirror ((port sdl2-port) (sheet sdl2-skia-window))
  (let* ((mirror (sheet-direct-mirror sheet)))
    (sdl2:gl-delete-context (gl-context mirror))
    (sdl2:destroy-window (window mirror))))
