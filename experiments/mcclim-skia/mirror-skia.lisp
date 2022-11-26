(in-package #:mcclim-skia)


(mcclim-sdl2::define-sdl2-request create-skia-mirror-for-sheet (sheet)
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (window (sdl2:create-window
                    :title title :flags '(:shown :opengl :resizable)
                    :x x :y y :w w :h h))
           (id (sdl2-ffi.functions:sdl-get-window-id window))
           (gl-context (make-gl-context-for-window window))
           (skia-context (skia-core:make-skia-context w h)))
      (sdl2:gl-make-current window gl-context)
      (gl:clear-color 1.0 1.0 1.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (make-instance 'sdl2-opengl-skia-mirror :sheet sheet :window window :id id
                                              :gl-context gl-context :skia-context skia-context))))

(defun %destroy-sdl2-opengl-skia-mirror (mirror)
  (skia-core:destroy-skia-context (skia-context mirror))
  (sdl2:gl-delete-context (gl-context mirror))
  (sdl2:destroy-window (window mirror))
  (log:info "DONE destroy-skia-window: mirror: ~a" mirror)
  (setf (gl-context mirror) nil
        (window mirror) nil
        (skia-context mirror) nil))

(mcclim-sdl2::define-sdl2-request destroy-skia-mirror (mirror)
  (unless mirror (error "No sdl2-opengl-skia-mirror provided!"))
    (%destroy-sdl2-opengl-skia-mirror mirror))

(mcclim-sdl2::define-sdl2-request destroy-skia-window (sheet)
  (log:info "BEGIN TRY destroy-skia-window: sheet: ~a, current thread: ~a"
            sheet (bt:current-thread))

  (alx:if-let ((mirror (sheet-mirror sheet)))
    (progn
      (log:info "BEGIN DO destroy-skia-window: mirror: ~a, sheet: ~a" mirror sheet)
      (%destroy-sdl2-opengl-skia-mirror mirror))
    (progn
      (log:warn "No mirror available for sheet: ~a!" sheet)
      (break))
    ))

(defvar *last-mirrored-sheet* nil)

(defmethod realize-mirror ((port sdl2-skia-port) (sheet top-level-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((mirror (create-skia-mirror-for-sheet sheet :synchronize t))
           (id (sdl2-ffi.functions:sdl-get-window-id (window mirror)))
           (native-region (make-rectangle* 0 0 w h))
           (native-transformation (make-translation-transformation (- x) (- y))))
      (alx:when-let ((icon (sheet-icon sheet)))
        (mcclim-sdl2::change-window-icon id (alx:ensure-car icon)))
      (setf (climi::%sheet-native-region sheet) native-region
            (climi::%sheet-native-transformation sheet) native-transformation
            (mcclim-sdl2::id->mirror port id) mirror
            ;;XXX check if mirrored-sheet first?
            (climi::%sheet-direct-mirror sheet) mirror)
      (log:info "REALIZE: w: ~a, h: ~a win-id: ~a, sheet: ~a, mirror: ~a" w h id sheet mirror)
      (setf *last-mirrored-sheet* sheet)
      mirror)))

(defmethod destroy-mirror ((port sdl2-skia-port) (sheet top-level-sheet-mixin))
  (destroy-skia-window sheet :synchronize t))

(defmethod destroy-mirror ((port sdl2-skia-port) (mirror sdl2-opengl-skia-mirror))
  (destroy-skia-mirror mirror :synchronize t))
