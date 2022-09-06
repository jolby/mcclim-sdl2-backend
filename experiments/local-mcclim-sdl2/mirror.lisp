(in-package #:mcclim-sdl2)

;;; FIXME the mirror should not depend on the used render (hence, not subclass
;;; image-mirror-mixin) - the reason for that is that we want to be able to plug
;;; an arbitrary renderer that is capable of working with SDL2 (i.e opengl).
;;;


(defclass mirror-with-sheet-mixin ()
  ((mirror-sheet :initarg :sheet :accessor mirror-sheet)))

(defclass sdl2-window-handle-mixin ()
  ((window-id :initarg :id :reader window-id)))

;;; Ideally the only difference would be creating a different medium class. For
;;; now let's ignore this concern and carry on.
(defclass sdl2-mirror (mcclim-render::image-mirror-mixin
                       mirror-with-sheet-mixin sdl2-window-handle-mixin)
  ())

(defmethod initialize-instance :after ((mirror sdl2-mirror) &rest initargs)
  (declare (ignore initargs))
  (setf (mcclim-render:image-mirror-image mirror)
        (mcclim-render::%create-mirror-image mirror 1024 1024)))

(defmethod realize-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (log:info "Creating a new window [~s ~s :w ~s :h ~s]" x y w h)
    (let* ((title (sheet-pretty-name sheet))
           (flags '(:shown :resizable))
           (id (create-window title :centered :centered w h flags :synchronize t))
           (mirror (make-instance 'sdl2-mirror :id id :sheet sheet))
           (native-region (make-rectangle* 0 0 w h))
           (native-transformation (make-translation-transformation (- x) (- y))))
      (alx:when-let ((icon (sheet-icon sheet)))
        (change-window-icon id (alx:ensure-car icon)))
      (setf (climi::%sheet-native-region sheet) native-region
            (climi::%sheet-native-transformation sheet) native-transformation
            (id->mirror port id) mirror))))

(defmethod realize-mirror ((port sdl2-port) (sheet unmanaged-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title "(McCLIM)")
           (flags '(:borderless))
           (id (create-window title :centered :centered w h flags :synchronize t))
           (mirror (make-instance 'sdl2-mirror :id id :sheet sheet))
           (native-region (make-rectangle* 0 0 w h))
           (native-transformation (make-translation-transformation (- x) (- y))))
      (setf (climi::%sheet-native-region sheet) native-region
            (climi::%sheet-native-transformation sheet) native-transformation
            (id->mirror port id) mirror))))

#+ (or) ;; SDL2 port does not implement mirrored sub-windows.
(defmethod realize-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (error "fueh"))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-direct-mirror sheet))
         (window-id (window-id mirror)))
    (destroy-window window-id)
    (setf (id->mirror port window-id) nil)))

(defmethod port-set-mirror-geometry
    ((port sdl2-port) (sheet mirrored-sheet-mixin) region)
  (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
    (change-window-size (sheet-direct-mirror sheet) x1 y1 w h)
    (values x1 y1 x2 y2)))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (show-window (sheet-direct-mirror sheet)))

(defmethod port-disable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (hide-window (sheet-direct-mirror sheet)))

;;; The following functions are specific to top-level sheets.

(defmethod port-set-mirror-name
    ((port sdl2-port) (sheet top-level-sheet-mixin) name)
  (change-window-title (sheet-direct-mirror sheet) name))

(defmethod port-set-mirror-icon
    ((port sdl2-port) (sheet top-level-sheet-mixin) icon)
  (alx:when-let ((window (sheet-direct-mirror sheet)))
    (change-window-icon (sheet-direct-mirror sheet) icon)))

(defmethod port-shrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (minimize-window (sheet-direct-mirror sheet)))

(defmethod port-unshrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (restore-window (sheet-direct-mirror sheet)))

(defmethod raise-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (raise-window (sheet-direct-mirror sheet)))

(defmethod bury-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (log:warn "Unsupported operation."))


;;; Requests

(define-sdl2-request create-window (title x y width height flags)
  (log:info "Creating a new window.")
  (let ((window (sdl2:create-window :title title :flags flags
                                    :x x :y y :w width :h height)))
    (sdl2-ffi.functions:sdl-get-window-id window)))

(define-sdl2-request destroy-window (window-id)
  (log:info "Destroying window ~s." window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2:destroy-window window)))

(define-sdl2-request change-window-title (window-id title)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-set-window-title window title)))

(define-sdl2-request change-window-icon (window-id icon)
  (alx:when-let ((window (sdl2-window window-id)))
    (let* ((array (pattern-array icon))
           (width (pattern-width icon))
           (height (pattern-height icon))
           (depth 32)
           (pitch (* 4 width)))
      (cffi:with-foreign-array (arr array `(:array :uint32 ,height ,width))
        (let ((surface
                (sdl2-ffi.functions:sdl-create-rgb-surface-from
                 arr width height depth pitch
                 #x00ff0000 #x0000ff00 #x000000ff #xff000000)))
          (unwind-protect
               (sdl2-ffi.functions:sdl-set-window-icon window surface)
            (sdl2-ffi.functions:sdl-free-surface surface)))))))

(define-sdl2-request change-window-size (window-id x y w h)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-set-window-position window x y)
    (sdl2-ffi.functions:sdl-set-window-size window w h)))

(define-sdl2-request hide-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-hide-window window)))

(define-sdl2-request show-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-show-window window)))

(define-sdl2-request minimize-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-minimize-window window)))

(define-sdl2-request restore-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-restore-window window)))

;;; Specified by SDL2 but doesn't seem to work on X11.
(define-sdl2-request raise-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-raise-window window)))

#+ (or) ;; Not specified by SDL2.
(define-sdl2-request bury-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-bury-window window)))


;;; Window SDL2 event handlers.

(define-sdl2-handler (ev :windowevent) (event window-id timestamp data1 data2)
  ;; There is a brief period after creating the window when there is no mapping.
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror)))
    (let ((event-key (autowrap:enum-key '(:enum (windowevent.event)) event)))
      (handle-sdl2-window-event event-key sheet timestamp data1 data2))))

(defgeneric handle-sdl2-window-event (event-key sheet timestamp data1 data2)
  (:method (event-key sheet timestamp data1 data2)
    (log:debug "Unhandled window event ~s." event-key)))

(defmethod handle-sdl2-window-event ((key (eql :close)) sheet stamp d1 d2)
  (log:info "Destroying a window.")
  (make-instance 'window-manager-delete-event :sheet sheet :timestamp stamp))

;;; FIXME introduce also resize-event? the handle-event method should also
;;; update the sheet transformation.
(defmethod handle-sdl2-window-event ((key (eql :size-changed)) sheet stamp d1 d2)
  (log:info "Window size changed ~s ~s" d1 d2)
  (make-instance 'window-configuration-event :sheet sheet :timestamp stamp
                                             :width d1 :height d2))

;;; Between pressing quit and the actual close the user may still use the
;;; window for a brief period, so i.e a window event may sneak in. The window
;;; event handler should ignore events to windows that are already destroyed.
(defmethod handle-sdl2-window-event ((key (eql :exposed)) sheet stamp d1 d2)
  (log:info "Exposing a window.")
  ;; The call to SDL-GET-WINDOW-SURFACE is for side the effect, namely to ensure
  ;; that the surface is allocated (to be able to call UPDATE-WINDOW).
  (let ((window (sdl2-window (window-id (sheet-mirror sheet)))))
    ;; FIXME check returned values for errors.
    (sdl2-ffi.functions:sdl-get-window-surface window)
    (sdl2-ffi.functions:sdl-update-window-surface window)))
