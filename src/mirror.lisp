(in-package #:clim-sdl2)


(defun sdl2-drawable (object)
  (etypecase object
    (sheet  (sdl2-drawable (sheet-mirror object)))
    (medium (sdl2-drawable (medium-drawable object)))
    (sdl2-mirror (window object))
    (null nil)))

(defmethod port-set-mirror-name
    ((port sdl2-port) (sheet mirrored-sheet-mixin) name)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2:set-window-title window name))))

(defmethod port-set-mirror-icon
    ((port sdl2-port) (sheet mirrored-sheet-mixin) icon)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (log:warn "clim-sdl2:port-set-mirror-icon NIY...")))

;; (defmethod port-set-mirror-geometry
;;     ((port sdl2-port) (sheet mirrored-sheet-mixin) region)
;;   (when-let ((mirror (sheet-direct-mirror sheet)))
;;     (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
;;       (with-bounding-rectangle* (ox1 oy1 :width ow :height oh) (sheet-mirror-geometry sheet)
;;         (let ((window (window mirror)))
;;           (when (or (/= x1 ox1) (/= y1 oy1))
;;             (sdl2:set-window-position window (round-coordinate x1) (round-coordinate y1)))
;;           (when (or (/= w ow) (/= h oh))
;;             (sdl2:set-window-size window (round-coordinate w) (round-coordinate h)))))
;;       (values x1 y1 x2 y2))))

(defmethod port-set-mirror-geometry (port sheet region)
  (with-bounding-rectangle* (x y :width w :height h) region
    (change-window-size (sheet-direct-mirror sheet) x y w h)))

(define-sdl2-request change-window-size (window x y w h)
  (sdl2-ffi.functions:sdl-set-window-position window x y)
  (sdl2-ffi.functions:sdl-set-window-size window w h))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2:destroy-window window))))

(defmethod raise-mirror ((port sdl2-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2-ffi.functions:sdl-raise-window window))))

(defmethod bury-mirror ((port sdl2-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (log:warn "clim-sdl2:bury-mirror NIY...")))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2:show-window window))))

(defmethod port-disable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (sdl2:hide-window window))))

(defmethod port-shrink-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (window (sheet-direct-mirror sheet))))
    (let ((window (window mirror)))
      (sdl2:minimize-window window))))

(defmethod sheet-direct-mirror ((sheet sdl2-top-level-sheet-pane))
  ())
