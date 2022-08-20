(in-package #:clim-sdl2)

(defclass sdl2-mirror ()
  ((window
    :initarg :window
    :reader window))
  (:default-initargs :window (alexandria:required-argument :window)))

(defun sdl2-drawable (object)
  (etypecase object
    (sheet  (sdl2-drawable (sheet-mirror object)))
    (medium (sdl2-drawable (medium-drawable object)))
    (sdl2-mirror (window object))
    ;; (xlib:drawable object)
    (null nil)))

(defun %set-window-name (window name)
  (setf (xlib:wm-name window) (%ensure-standard-characters name))
  (xlib:change-property window
                        :_NET_WM_NAME
                        (babel:string-to-octets name :encoding :utf-8)
                        :UTF8_STRING 8))

(defun %set-window-icon-name (window icon-name)
  (setf (xlib:wm-icon-name window) (%ensure-standard-characters icon-name))
  (xlib:change-property window
                        :_NET_WM_ICON_NAME
                        (babel:string-to-octets icon-name :encoding :utf-8)
                        :UTF8_STRING 8))

(defmethod port-set-mirror-name
    ((port sdl2-basic-port) (sheet mirrored-sheet-mixin) name)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl:set-window-title window name))))

(defmethod port-set-mirror-icon
    ((port sdl2-basic-port) (sheet mirrored-sheet-mixin) icon)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (log:warn "clim-sdl2:port-set-mirror-icon NIY...")))

(defmethod port-set-mirror-geometry
    ((port sdl2-basic-port) (sheet mirrored-sheet-mixin) region)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
      (with-bounding-rectangle* (ox1 oy1 :width ow :height oh) (sheet-mirror-geometry sheet)
        (let ((window (window mirror)))
          (when (or (/= x1 ox1) (/= y1 oy1))
            (sdl2:set-window-position (round-coordinate x1)(round-coordinate y1)))
          (when (or (/= w ow) (/= h oh))
            (sdl:set-window-size (round-coordinate w) (round-coordinate y)))))
      (values x1 y1 x2 y2))))

(defmethod destroy-mirror ((port sdl2-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl:destroy-window window))))

(defmethod raise-mirror ((port sdl2-basic-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (sdl2-ffi.functions:sdl-raise-window sdlwindow)))

(defmethod bury-mirror ((port sdl2-basic-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (log:warn "clim-sdl2:bury-mirror NIY...")))

(defmethod port-enable-sheet ((port sdl2-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl:show-window window))))

(defmethod port-disable-sheet ((port sdl2-basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl:hide-window window))))

(defmethod port-shrink-sheet ((port sdl2-basic-port) (sheet mirrored-sheet-mixin))
  ;; Not all x11 window-managers iconify windows (in particular many
  ;; tiling window-managers). In those window managers, after the call
  ;; of shrink-frame McClim think that the frame is iconified but it
  ;; is not. -- admich 2021-08-21
  (when-let ((window (window (sheet-direct-mirror sheet))))
    (let ((window (window mirror)))
      (sdl:minimize-window window))))
