(in-package #:clim-sdl2)


(defun sdl2-drawable (object)
  (etypecase object
    (sheet  (sdl2-drawable (sheet-mirror object)))
    (medium (sdl2-drawable (medium-drawable object)))
    (sdl2-mirror (window object))
    (null nil)))

(defun get-mirror-sheet (port window-id)
  (gethash window-id (sdl-port/window-id-to-sheet port)))

(defun set-mirror-sheet (port window-id window)
  (setf (gethash window-id (sdl-port/window-id-to-sheet port)) window))

(define-sdl2-request create-window (sheet title w h)
  (log:info "sheet: ~a, title: ~a, w: ~a, h: ~a" sheet title w h)
  (sdl2:create-window :title title
                      :w w
                      :h h
                      :flags (append '(:resizable)
                              (if (sheet-enabled-p sheet) '(:shown) nil))))

(defmethod realize-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (let* ((w (bounding-rectangle-width sheet))
         (h (bounding-rectangle-height sheet))
         (title (sheet-pretty-name sheet))
         (win (create-window sheet title w h :synchronize t))) ;;maybe we could pass a callback?
    (log:info "Created win: ~a" win)
    (setf (sdl-port-window port) win)
    (set-mirror-sheet port (sdl2:get-window-id win) sheet)
    ;;XXX Do we want to do this???
    (setf (port sheet) port)

    (make-instance 'sdl2-mirror :window win)
    ))

(comment


  )

(define-sdl2-request destroy-window (window)
  (sdl2:destroy-window window))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (destroy-window window))))

(define-sdl2-request change-window-size (window x y w h)
  (sdl2-ffi.functions:sdl-set-window-position window x y)
  (sdl2-ffi.functions:sdl-set-window-size window w h))

(defmethod port-set-mirror-geometry (port sheet region)
  (with-bounding-rectangle* (x y :width w :height h) region
    (when-let ((mirror (sheet-direct-mirror sheet)))
      (change-window-size mirror x y w h))))

(define-sdl2-request set-window-title (window name)
  (sdl2:set-window-title window name))

(defmethod port-set-mirror-name
    ((port sdl2-port) (sheet mirrored-sheet-mixin) name)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (set-window-title window name))))

(defmethod port-set-mirror-icon
    ((port sdl2-port) (sheet mirrored-sheet-mixin) icon)
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (log:warn "clim-sdl2:port-set-mirror-icon NIY...")))

(define-sdl2-request raise-window (window)
  (sdl2:raise-window window))

(defmethod raise-mirror ((port sdl2-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (let ((window (window mirror)))
      (raise-window window))))

(defmethod bury-mirror ((port sdl2-port) (sheet basic-sheet))
  (when-let ((mirror (sheet-mirror sheet)))
    (log:warn "clim-sdl2:bury-mirror NIY...")))

(define-sdl2-request show-window (window)
  (sdl2:show-window window))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (show-window window))))

(define-sdl2-request hide-window (window)
  (sdl2:hide-window window))

(defmethod port-disable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (let ((window (window mirror)))
      (hide-window window))))

(define-sdl2-request minimize-window (window)
  (sdl2:minimize-window window))

(defmethod port-shrink-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (window (sheet-direct-mirror sheet))))
    (let ((window (window mirror)))
      (minimize-window window))))

;; (defmethod sheet-direct-mirror ((sheet sdl2-top-level-sheet-pane))
;;   ())
