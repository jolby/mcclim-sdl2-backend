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
    (log:info "DESTROY-mirror sheet: ~a, mirror: ~a, window-id: ~a"
              sheet mirror window-id)
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
    (sdl2-ffi.functions:sdl-update-window-surface window)

    (make-instance 'window-repaint-event
                         :timestamp stamp
                         :sheet sheet
                         :region clim:+everywhere+) ))

(defun %make-window-boundary-event (event-type sheet stamp)
  (let ((window (sdl2-window (window-id (sheet-mirror sheet))))
        (pointer (port-pointer *sdl2-port*)))
    (cffi:with-foreign-objects ((xref :int)
                                (yref :int))
      (sdl2-ffi.functions:sdl-get-mouse-state xref yref)
      (multiple-value-bind (window-x window-y) (sdl2:get-window-position window)
        (let* ((x (cffi:mem-ref xref :int))
               (y (cffi:mem-ref yref :int))
               (graft-x (+ window-x x))
               (graft-y (+ window-y y)))
          (make-instance event-type
                         :sheet sheet :timestamp stamp
                         :pointer pointer
                         :button nil
                         :x x :y y
                         :graft-x graft-x :graft-y graft-y
                         :modifier-state nil
                         ))
        ))))

(defmethod handle-sdl2-window-event ((key (eql :focus-gained)) sheet stamp d1 d2)
  (log:info "Window focus gained ~a ~s ~s" sheet d1 d2)
  (make-instance 'window-manager-focus-event :sheet sheet :timestamp stamp))

(defmethod handle-sdl2-window-event ((key (eql :enter)) sheet stamp d1 d2)
  (log:info "Window entered ~a ~s ~s" sheet d1 d2)
  (%make-window-boundary-event 'pointer-enter-event sheet stamp))

(defmethod handle-sdl2-window-event ((key (eql :leave)) sheet stamp d1 d2)
  (log:info "Window leave ~a ~s ~s" sheet d1 d2)
  (%make-window-boundary-event 'pointer-exit-event sheet stamp))

;;XXX TODO Do real input state management
(defvar *key-modifiers* nil)

;;XXX DEBUG
(defvar *last-keydown-event* nil)

;;; Keyboard SDL2 event handlers
(defun dump-sdl2-keysym (keysym)
  (let* ((scancode (sdl2:scancode keysym))
         (mod-value (sdl2:mod-value keysym))
         (sym-value (sdl2:sym-value keysym))
         (scancode-name (sdl2:scancode-name scancode))
         (scancode-value (sdl2:scancode-value keysym))
         (scancode-symbol (sdl2:scancode-symbol scancode))
         (scancode-key (sdl2:get-key-from-scancode scancode))
         (scancode-key-name (sdl2:scancode-key-name scancode))
         (mod-keywords (sdl2:mod-keywords mod-value)))
    (log:info
     "scancode: ~a, scancode-name: ~a, scancode-value: ~a, scancode-symbol: ~a, scancode-key: ~a, scancode-key-name: ~a,mod-value: ~a, sym-value: ~a, mod-keywords: ~a"
     scancode scancode-name scancode-value scancode-symbol scancode-key scancode-key-name mod-value sym-value mod-keywords)))

(defun %make-key-event (event-type-sym sdl2-scancode sdl2-mod-code sheet timestamp)
  (multiple-value-bind (key-char key-code-keyword clim-mod-state)
      (key-press-event-values-from-sdl2-scancode sdl2-scancode sdl2-mod-code)
    ;; (log:info "sdl2-scancode: ~a, sdl2-mod-code: ~a, key-char: ~a, key-code-keyword: ~a, clim-mod-state: ~a"
    ;;           sdl2-scancode sdl2-mod-code key-char key-code-keyword clim-mod-state)
    (when (eql event-type-sym 'clim:key-press-event)
      (log:info "KP EVT values: key-char: ~a, key-code-keyword: ~a, clim-mod-state: ~a"
                key-char key-code-keyword clim-mod-state))
    (let ((kp-event (make-instance event-type-sym
                                   :key-name key-code-keyword
                                   :key-character key-char
                                   ;; :x 0 :y 0
                                   ;; :graft-x 0
                                   ;; :graft-y 0
                                   :sheet sheet
                                   :modifier-state clim-mod-state
                                   :timestamp timestamp)))
      (when (eql event-type-sym 'clim:key-press-event)
        (log:info "KP EVT: ~a" kp-event))
      kp-event)))

(define-sdl2-handler (ev :keydown) (window-id timestamp state repeat keysym)
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror)))
    (let* ((scancode (sdl2:scancode keysym))
           (scancode-value (sdl2:scancode-value keysym))
           (sdl2-mod-code (sdl2:mod-value keysym))
           (sdl2-key-code (sdl2:sym-value keysym)))
      ;; (dump-sdl2-keysym keysym)
      (%make-key-event 'key-press-event scancode-value sdl2-mod-code sheet timestamp))))

(define-sdl2-handler (ev :keyup) (window-id timestamp state repeat keysym)
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror)))
    (let* ((scancode (sdl2:scancode keysym))
           (scancode-value (sdl2:scancode-value keysym))
           (sdl2-mod-code (sdl2:mod-value keysym))
           (sdl2-key-code (sdl2:sym-value keysym)))
      ;; (dump-sdl2-keysym keysym)
      (%make-key-event 'key-release-event scancode-value sdl2-mod-code sheet timestamp))
    ;; (log:info "EVT keyup: ev: ~a, wid: ~a ts: ~a, state: ~a, repeat: ~a, keysym: ~a"
    ;;           ev window-id timestamp state repeat keysym)
    ))

;;;;Mouse SDL2 event handlers
(defun %make-mouse-button-event (event-type window-id timestamp x y button which state)
  (let* ((mirror (id->mirror *sdl2-port* window-id))
        (sheet (mirror-sheet mirror))
        (sdl2-win (sdl2-ffi.functions:sdl-get-window-from-id window-id))
        (pointer (port-pointer *sdl2-port*))
        (clim-button (sdl2-button-code->clim-button-code button))
        (clim-mod-state (sdl2-mod-state->clim-mod-state state)))
    (multiple-value-bind (window-x window-y) (sdl2:get-window-position sdl2-win)
      (let ((graft-x (+ window-x x))
            (graft-y (+ window-y y)))
        ;; update position in pointer object
        (setf (slot-value pointer 'x) x
              (slot-value pointer 'y) y)
        ;;XXX HACK! But I get error in distribute-event if pointer-sheet is null
        (unless (pointer-sheet pointer)
          (setf (pointer-sheet pointer) sheet))
        (make-instance event-type
                       :timestamp timestamp
                       :sheet sheet
                       :pointer pointer :button clim-button
                       :x x :y y :graft-x graft-x :graft-y graft-y
                       :modifier-state clim-mod-state)))))

(define-sdl2-handler (ev :mousebuttondown) (window-id timestamp x y button which state)
  (log:info "EVT mousebuttondown: ev: ~a, wid: ~a ts: ~a, x: ~a, y: ~a, btn: ~a, which: ~a state: ~a"
              ev window-id timestamp x y button which state)
    (%make-mouse-button-event 'pointer-button-press-event window-id timestamp x y button which state))

(define-sdl2-handler (ev :mousebuttonup) (window-id timestamp x y button which state)
  (log:info "EVT mousebuttonup: ev: ~a, wid: ~a ts: ~a, x: ~a, y: ~a, btn: ~a, which: ~a state: ~a"
              ev window-id timestamp x y button which state)
    (%make-mouse-button-event 'pointer-button-release-event window-id timestamp x y button which state))

(define-sdl2-handler (ev :mousemotion) (window-id timestamp x y xrel yrel which state)
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror))
                  (sdl2-win (sdl2-ffi.functions:sdl-get-window-from-id window-id))
                  (pointer (port-pointer *sdl2-port*))
                  (clim-mod-state (sdl2-mod-state->clim-mod-state state)))
    ;; (log:info "EVT mousemotion: ev: ~a, wid: ~a ts: ~a, x: ~a, y: ~a, xrel: ~a, yrel: ~a, which: ~a state: ~a"
    ;;           ev window-id timestamp x y xrel yrel which state)
    (multiple-value-bind (window-x window-y) (sdl2:get-window-position sdl2-win)
         (let ((graft-x (+ window-x x))
               (graft-y (+ window-y y)))
           ;; update position in pointer object
           (setf (slot-value pointer 'x) x
                 (slot-value pointer 'y) y)
           (make-instance 'pointer-motion-event
                          :timestamp timestamp
                          :sheet sheet
                          :pointer pointer
                          :button nil
                          :x x :y y
                          :graft-x graft-x :graft-y graft-y
                          :modifier-state clim-mod-state)))))

;; This also picks up touchpad
(define-sdl2-handler (ev :mousewheel) (window-id timestamp x y which direction)
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror))
                  (sdl2-win (sdl2-ffi.functions:sdl-get-window-from-id window-id))
                  (pointer (port-pointer *sdl2-port*)))
    (multiple-value-bind (window-x window-y) (sdl2:get-window-position sdl2-win)
         (let* ((graft-x (+ window-x x))
               (graft-y (+ window-y y))
               (old-x (or (slot-value pointer 'x) 0))
               (old-y (or (slot-value pointer 'y) 0))
               (delta-x (- x old-x))
               (delta-y (- y old-y)))
           ;; update position in pointer object
           (setf (slot-value pointer 'x) x
                 (slot-value pointer 'y) y)
           ;;XXX HACK! But I get error in distribute-event if pointer-sheet is null
           (unless (pointer-sheet pointer)
             (setf (pointer-sheet pointer) sheet))
           (log:info "EVT mousewheel/scroll: ts: ~a, x: ~a, y: ~a, dx: ~a, dy: ~a, which: ~a direction: ~a"
                     timestamp x y delta-x delta-y which direction)

           (make-instance 'climi::pointer-scroll-event
                          :timestamp timestamp
                          :sheet sheet
                          :pointer pointer
                          :button nil
                          :x x :y y
                          :graft-x graft-x :graft-y graft-y
                          :delta-x delta-x :delta-y delta-y)))))


;;XXX doesn't seem to work
(define-sdl2-handler (ev :touchfinger) (window-id timestamp x y which direction)
  (alx:when-let* ((mirror (id->mirror *sdl2-port* window-id))
                  (sheet (mirror-sheet mirror)))
    ;; Uncomment to test, but it SPEWS!!
    ;; (log:info "EVT touchfinger: ev: ~a, wid: ~a ts: ~a, x: ~a, y: ~a, which: ~a direction: ~a"
    ;;           ev window-id timestamp x y which direction)
    ))
;;XXX doesn't seem to work
(define-sdl2-handler (ev :multigesture) (touch-id timestamp x y d-theta d-dist num-fingers)
  ;; (log:info "EVT multigesture: touch-id: ~a, ts: ~a, x: ~a, y: ~a, d-theta: ~a, d-dist: ~a, num-fingers: ~a"
  ;;           touch-id timestamp x y d-theta d-dist num-fingers)
  )
