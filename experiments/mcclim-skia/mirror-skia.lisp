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
  (log:info "BEGIN destroy-skia-window: mirror: ~a, sheet: ~a" (sheet-direct-mirror sheet) sheet)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    ;; (break)
    (%destroy-sdl2-opengl-skia-mirror mirror)
    ))


(defmethod realize-mirror ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (log:info "REALIZE: w: ~a, h: ~a sheet: ~a" w h sheet)
    (let* ((mirror (create-skia-mirror-for-sheet sheet :synchronize t))
          (id (sdl2-ffi.functions:sdl-get-window-id (window mirror)))
          (native-region (make-rectangle* 0 0 w h))
          (native-transformation (make-translation-transformation (- x) (- y))))
      (alx:when-let ((icon (sheet-icon sheet)))
        (mcclim-sdl2::change-window-icon id (alx:ensure-car icon)))
      (setf (climi::%sheet-native-region sheet) native-region
            (climi::%sheet-native-transformation sheet) native-transformation
            (mcclim-sdl2::id->mirror port id) mirror))))

(defmethod destroy-mirror ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (destroy-skia-window sheet))

(defmethod destroy-mirror ((port mcclim-sdl2::sdl2-port) (mirror sdl2-opengl-skia-mirror))
  (destroy-skia-mirror mirror))
