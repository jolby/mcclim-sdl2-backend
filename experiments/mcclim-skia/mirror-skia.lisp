(in-package #:mcclim-skia)

(defclass sdl2-opengl-skia-mirror (sdl2-opengl-mirror)
  ((skia-context
    :initarg :skia-context
    :accessor skia-context)))

(mcclim-sdl2::define-sdl2-request create-skia-window-for-sheet (sheet)
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (window (sdl2:create-window
                    :title title :flags '(:shown :opengl :resizable)
                    :x x :y y :w w :h h))
           (id (sdl2-ffi.functions:sdl-get-window-id window))
           (gl-context (make-gl-context-for-window window))
           (skia-context (skia-core:make-skia-context w h)))
      (sdl2:gl-make-current window gl-context)
      (make-instance 'sdl2-opengl-skia-mirror :sheet sheet :window window :id id
                                              :gl-context gl-context :skia-context skia-context))))

(mcclim-sdl2::define-sdl2-request destroy-skia-window (sheet)
  (let* ((mirror (sheet-direct-mirror sheet)))
    (skia-core:destroy-skia-context (skia-context mirror))
    (sdl2:gl-delete-context (gl-context mirror))
    (sdl2:destroy-window (window mirror))
    (setf (gl-context mirror) nil
          (window mirror) nil
          (skia-context mirror) nil)))

(defmethod realize-mirror ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (log:info "REALIZE: w: ~a, h: ~a sheet: ~a" w h sheet)
    (let* ((mirror (create-skia-window-for-sheet sheet :synchronize t))
          (id (sdl2-ffi.functions:sdl-get-window-id (window mirror)))
          (native-region (make-rectangle* 0 0 w h))
          (native-transformation (make-translation-transformation (- x) (- y))))
      (alx:when-let ((icon (sheet-icon sheet)))
        (mcclim-sdl2::change-window-icon id (alx:ensure-car icon)))
      (setf (climi::%sheet-native-region sheet) native-region
            (climi::%sheet-native-transformation sheet) native-transformation
            (mcclim-sdl2::id->mirror port id) mirror)
      mirror)))

(defmethod destroy-mirror ((port mcclim-sdl2::sdl2-port) (sheet sdl2-skia-top-level-sheet))
  (destroy-skia-window sheet))

