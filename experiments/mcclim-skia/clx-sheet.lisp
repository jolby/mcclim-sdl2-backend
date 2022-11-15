(in-package #:mcclim-clx-test)

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
  comments-by-example.

  Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
  silly macro, but used inside of other macros or code generation
  facilities it is very useful - you can see comments in the (one-time)
  macro expansion!\""
  (declare (ignore body)))

;; (defvar *glider*
;;   (make-pattern-from-bitmap-file
;;    (asdf:component-pathname
;;     (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defparameter *last-char-txt* "")

;; (defclass clx-test-top-level-sheet
;;     (top-level-sheet-mixin mirrored-sheet-mixin basic-sheet)
;;   ())

(defclass clx-app-sheet (;; repainting
                          immediate-repainting-mixin
                          ;; input
                          immediate-sheet-input-mixin
                          ;; output
                          permanent-medium-sheet-output-mixin
                          ;; standard-output-recording-stream
                          ;;temporary-medium-sheet-output-mixin
                          ;;sheet-with-medium-mixin
                          ;;sheet-mute-output-mixin
                          ;; geometry
                          sheet-identity-transformation-mixin
                          ;; genealogy
                          sheet-parent-mixin
                          sheet-leaf-mixin
                          ;; windowing
                          ;; clx-test-top-level-sheet
                         ;; windowing
                         top-level-sheet-mixin
                         mirrored-sheet-mixin
                         ;; the base class
                         basic-sheet)
  ()
  (:default-initargs ;; :icon *glider*
                     :pretty-name "McCLIM CLX Test Control Pane"
                     :region (make-rectangle* -200 -200 800 800)
                     ;; :region (make-rectangle* 0 0 1080 720)
                     ;; :buffering-p t
                     ))

;; (defclass clx-app-sheet (top-level-sheet-mixin
;;                          mirrored-sheet-mixin
;;                          ;; the base class
;;                          basic-pane)
;;   ()
;;   (:default-initargs ;; :icon *glider*
;;    :name "McCLIM CLX Test Control Sheet"
;;    :region (make-rectangle* -200 -200 800 800)
;;    ;; :region (make-rectangle* 0 0 1080 720)
;;    ;; :buffering-p t
;;    ))

(defun simple-draw (sheet)
  (let ((medium (sheet-medium sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2) sheet
      (log:info "SIMPLE-DRAW current-thread: ~a" (bt:current-thread))
      (log:info "bounding rect: x1: ~a, y1: ~a, x2: ~a, y2: ~a" x1 y1 x2 y2)
      (medium-clear-area medium x1 y1 x2 y2)
      (draw-circle* medium 50 50 25
                    :ink (alexandria:random-elt
                          (make-contrasting-inks 8)))
      (draw-point* medium 100 100 :ink +black+ :line-thickness 8)
      (draw-text* medium "(100,100)" 100 100
                  :text-style (make-text-style :sans-serif :roman 32)
                  :ink +red+)
      (draw-line* medium 50 50 500 500 :ink +red+ :line-thickness 4)
      (draw-point* medium 50 50 :ink +green+ :line-thickness 8)
      (draw-point* medium 500 500 :ink +red+ :line-thickness 8)
      ;; Test opacity- will we see the red line undneath??
      (draw-circle* medium 500 500 100
                    :ink (clim-internals::make-rgba-color
                          0.4 0.8 0.5 0.8))

      ;; Make a text header for inner card
      (draw-text* medium "Card Header" 700 60
                  :text-style (make-text-style :sans-serif :roman 48))
      (draw-text* medium (format nil "Last key: ~a" *last-char-txt*) 750 120
                  :text-style (make-text-style :sans-serif :roman 32))
      (medium-finish-output sheet))))

(defmethod handle-event ((sheet clx-app-sheet) event)
  (log:info "Unhandled event ~s has arrived." (class-name (class-of event))))

(defmethod handle-event ((sheet clx-app-sheet) (event window-configuration-event))

  (log:info "CLX handle-event: ~a" event)
  (let ((climi::*configuration-event-p* sheet))
    (resize-sheet sheet
                  (climi::window-configuration-event-width event)
                  (climi::window-configuration-event-height event))
    (repaint-sheet sheet +everywhere+)))

(defmethod handle-event ((sheet clx-app-sheet) (event window-manager-delete-event))
  (log:info "CLX handle-event: ~a" event)
  (destroy-mirror (port sheet) sheet))

(defmethod handle-event ((sheet clx-app-sheet) (event window-repaint-event))
  (log:info "CLX repaint-event: ~a" event)
  (handle-repaint sheet (window-event-region event)))

(defmethod handle-event ((sheet clx-app-sheet) (event clim:key-press-event))
  (log:info "GOT EVENT: ~a" event)
  (setf *last-char-txt* (string (keyboard-event-character event)))
  (handle-repaint sheet +everywhere+))

(defmethod handle-repaint ((sheet clx-app-sheet) region)
  (log:warn "Repainting a window (region ~s)." region)
  (simple-draw sheet))

(defparameter *clx-port* nil)
(defparameter *clx-mirror* nil)
(defparameter *clx-sheet* nil)
(defparameter *clx-medium* nil)

(defun %clx-window-manager-hack (sheet x y w h)
  (let ((window (clim-clx::window (sheet-direct-mirror sheet))))
    (setf (xlib:wm-normal-hints window)
          (xlib:make-wm-size-hints
           :user-specified-position-p (and x y)
           :x x :y y
           :width  (round w)
           :height (round y)
           :max-width (min 65535 (round w))
           :max-height (min 65535 (round h))
           :min-width (round w)
           :min-height (round y)))
    (xlib:map-window window)))

(defun open-clx-sheet (path &optional restartp)
  (let ((port (find-port :server-path path)))
    (when restartp
      (restart-port port))
    (setf *clx-port* port)
    (let* (;; Supplying :PORT here is a kludge in the core.
           (sheet (make-instance 'clx-app-sheet :port port))
           (graft (find-graft :port port))
           ;; (mirror (realize-mirror port sheet))
           )
      (sheet-adopt-child graft sheet)
      (setf *clx-mirror*  (realize-mirror port sheet)
            *clx-medium* (sheet-medium sheet)
            *clx-sheet* sheet)
      ;; (port-enable-sheet *clx-port* *clx-sheet*)
      ;; (xlib:map-window (clim-clx::window *clx-mirror*))
      ;; (raise-mirror *clx-port* *clx-sheet*)
      (%clx-window-manager-hack *clx-sheet* 400 400 800 800)
      (raise-mirror *clx-port* *clx-sheet*)
      *clx-sheet*)))

(defun close-clx-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet))

(defun nuke-state ()
  (when *clx-sheet* (destroy-mirror *clx-port* *clx-sheet*) (setf *clx-sheet* nil))
  (when *clx-mirror*  (setf *clx-mirror* nil))
  (when *clx-port* (destroy-port *clx-port*) (setf *clx-port* nil)))

(comment

  (open-clx-sheet :clx-ttf t)
  (close-clx-sheet *clx-sheet*)
  (nuke-state)
  (medium-finish-output *clx-medium*)
  (medium-force-output *clx-medium*)
  (let ((result (simple-draw *clx-sheet*)))
    (if (typep result 'condition)
        (error result)
        'done))
  )
