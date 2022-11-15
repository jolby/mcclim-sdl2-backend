(in-package #:mcclim-sdl2)

;;; Port pointer
(defclass sdl2-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

;;; SDL2 is always expected to run in a single thread. Moreover the same
;;; application may have only a single sdl context. Ergo SDL2 thread is a
;;; synchronized singleton object.

(defclass sdl2-port (basic-port)
  ((main-thread
    :allocation :class
    :initform nil
    :accessor main-thread)
   (main-thread-lock
    :allocation :class
    :initform (clim-sys:make-lock "McCLIM SDL2 lock")
    :reader main-thread-lock)
   (sdl2-clients
    :allocation :class
    :initform '()
    :accessor sdl2-clients)
   (id->mirror
    :allocation :class
    :initform (make-hash-table))
   (name->cursor
    :allocation :class
    :initform (make-hash-table :test #'eql))))

;;; This variable is bound in the context of event processing so it is
;;; possible to reference the port making it a requiered argument for
;;; clim-agnostic handlers.
(defvar *sdl2-port*)

(defun id->mirror (port window-id)
  (gethash window-id (slot-value port 'id->mirror)))

(defun (setf id->mirror) (new-mirror port window-id)
  (let ((table (slot-value port 'id->mirror)))
    (if (null new-mirror)
        (remhash window-id table)
        (setf (gethash window-id table) new-mirror))))

(defvar *sdl2-cursor-code-mapping*
  `((:busy ,sdl2-ffi:+sdl-system-cursor-wait+)
    (:default ,sdl2-ffi:+sdl-system-cursor-arrow+)
    (:i-beam ,sdl2-ffi:+sdl-system-cursor-ibeam+)
    (:crosshair ,sdl2-ffi:+sdl-system-cursor-crosshair+)
    (:hand ,sdl2-ffi:+sdl-system-cursor-hand+)
    (:no ,sdl2-ffi:+sdl-system-cursor-no+)
    (:sizeall ,sdl2-ffi:+sdl-system-cursor-sizeall+)
    (:sizenesw ,sdl2-ffi:+sdl-system-cursor-sizenesw+)
    (:sizens ,sdl2-ffi:+sdl-system-cursor-sizens+)
    (:sizenwse ,sdl2-ffi:+sdl-system-cursor-sizenwse+)
    (:sizewe ,sdl2-ffi:+sdl-system-cursor-sizewe+)
    (:waitarrow ,sdl2-ffi:+sdl-system-cursor-waitarrow+)))

(defun %sdl2-cursor-names ()
  (mapcar #'car *sdl2-cursor-code-mapping*))

(defun %create-sdl2-cursor (cursor-name)
    (let* ((code (second (or
                          (assoc cursor-name *sdl2-cursor-code-mapping*)
                          (assoc :default *sdl2-cursor-code-mapping*)))))
      (sdl2-ffi.functions:sdl-create-system-cursor code)))

(defun %destroy-sdl2-cursor (sdlcursor)
  (sdl2-ffi.functions:sdl-free-cursor sdlcursor))

(defun %set-sdl2-cursor (sdlcursor)
  (sdl2-ffi.functions:sdl-set-cursor sdlcursor))

(defun name->cursor (port cursor-name)
  (gethash cursor-name (slot-value port 'name->cursor)))

(defun (setf name->cursor) (new-cursor port cursor-name)
  (let ((table (slot-value port 'name->cursor)))
    (if (null new-cursor)
        (alx:if-let ((cursor (gethash cursor-name table)))
          (remhash cursor-name table)
          (%destroy-sdl2-cursor cursor))
        (setf (gethash cursor-name table) new-cursor))))

(defun %init-sdl2-cursors (port)
  (loop :for cursor-name :in (%sdl2-cursor-names)
        :do (setf (name->cursor port cursor-name) (%create-sdl2-cursor cursor-name))))

(defun %destroy-sdl2-cursors (port)
  (loop :for table := (slot-value port 'name->cursor)
        :for cursor-name being the hash-keys of table
        :using (hash-value sdl2-cursor)
        :do (progn
              (remhash cursor-name table)
              (%destroy-sdl2-cursor sdl2-cursor))))


(defmethod pointer-position ((pointer sdl2-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(clim-sys:defmethod* (setf pointer-position) (x y (pointer sdl2-pointer))
  (let* ((port (port pointer))
         (graft (graft port))
         ;; (mirror (sheet-mirror graft))
         ;; (window (window mirror))
         ;; (window-id (window-id mirror))
         )
    (setf (slot-value pointer 'x) x
          (slot-value pointer 'y) y)
    (multiple-value-bind (x y)
        (transform-position (sheet-native-transformation graft) x y)
      (sdl2-ffi.functions:sdl-warp-mouse-global (round x) (round y)))))

(defmethod set-sheet-pointer-cursor ((port sdl2-port) (sheet mirrored-sheet-mixin) cursor-name)
  (let* ((cursor (name->cursor port cursor-name))
         (mirror (sheet-direct-mirror sheet))
         (window-id (window-id mirror)))
    (when (and cursor mirror)
      (%set-sdl2-cursor cursor))))

(defmethod find-port-type ((port (eql :sdl2)))
  (values 'sdl2-port (constantly nil)))

(defmethod initialize-instance :after ((port sdl2-port) &key server-path)
  (declare (ignore server-path))
  (clim-sys:with-lock-held ((main-thread-lock port))
    (%init-port port)
    (push port (sdl2-clients port))
    (setf (slot-value port 'pointer)
          (make-instance 'sdl2-pointer :port port))
    (%init-sdl2-cursors port)
    (build-key-info-db)
    (log:info "NEW sdl2-port: ~a, port-pointer: ~a" port (port-pointer port))))

(defmethod destroy-port ((port sdl2-port))
  ;; Close the SDL2 loop only after the last port is destroyed.
  (when (member port (sdl2-clients port))
    (clim-sys:with-lock-held ((main-thread-lock port))
      (alx:removef (sdl2-clients port) port)
      (%destroy-sdl2-cursors port)
      (when (null (sdl2-clients port))
        (%quit-port port)))))


;; (defmethod pointer-button-state ((pointer sdl-pointer))
;;   nil)

;; (defmethod port-modifier-state ((port sdl-port))
;;   nil)

;; (defmethod synthesize-pointer-motion-event ((pointer sdl-pointer))
;;   nil)

;; (defmethod port-frame-keyboard-input-focus ((port sdl-port) frame)
;;   (frame-properties frame 'focus))

;; (defmethod (setf port-frame-keyboard-input-focus)
;;     (focus (port sdl-port) frame)
;;   (setf (frame-properties frame 'focus) focus))

;; (defmethod (setf port-keyboard-input-focus) (focus (port sdl-port))
;;   focus)

;; (defmethod port-keyboard-input-focus ((port sdl-port))
;;   nil)

(defmethod restart-port ((port sdl2-port))
  (clim-sys:with-lock-held ((main-thread-lock port))
    (%quit-port port)
    (%init-port port)))

(define-condition sdl2-clim () ())
(define-condition sdl2-error (sdl2-clim simple-error) ())
(define-condition sdl2-exit-port (sdl2-clim) ())

(defun maybe-funcall (fn)
  (and fn (funcall fn)))

(defmethod process-next-event ((port sdl2-port) &key wait-function timeout)
  (let ((*sdl2-port* port))
    (loop (multiple-value-bind (result reason)
              (%next-sdl2 wait-function timeout)
            (cond ((eventp result)
                   (distribute-event port result)
                   (return-from process-next-event
                     (values t reason)))
                  ((member reason '(:wait-function :timeout))
                   (return-from process-next-event
                     (values result reason))))))))

;;; SDL is started either in a thread-safe loop or in a single thread. In the
;;; latter case it is the user responsibility to call PROCESS-NEXT-EVENT
;;; (indirectly handled by higher level interfaces).

(defun %init-port (port)
  (if *multiprocessing-p*
      (unless (main-thread port)
        (setf (main-thread port)
              (clim-sys:make-process (alx:curry #'%loop-port port) :name "MAIN-CLIM-SDL2-LOOP"))
        (loop until *initialized-p*
              do (clim-sys:with-lock-held (*initialized-lock*)
                   (clim-sys:condition-wait *initialized-cv* *initialized-lock*))))
      (%init-sdl2)))

(defun %quit-port (port)
  (if *multiprocessing-p*
      (alx:when-let ((main (main-thread port)))
        (when (bt:thread-alive-p main)
          (sdl2-exit-port)
          (bt:join-thread main))
        (setf (main-thread port) nil))
      (%quit-sdl2)))

(defun %loop-port (port)
  (%init-sdl2)
  (unwind-protect
       (handler-bind ((sdl2-exit-port
                        (lambda (c)
                          (declare (ignore c))
                          (return-from %loop-port)))
                      #+ (or)
                      (error
                        (lambda (c)
                          (log:error "Ignoring the error ~s" c)
                          (invoke-restart (find-restart 'ignore c)))))
         (loop
           (with-simple-restart (ignore "Ignore error and continue.")
             (process-next-event port))))
    (%quit-sdl2)))


;;; User-requested exit - this event is usually signaled when the last window
;;; is closed (in addition to windowevent/close), but may be also prompted by
;;; the window manager or an interrupt. The application may ignore it.
(defvar *quit-stops-the-port-p* nil)
(define-sdl2-handler (event :quit) ()
  (log:info "Quit requested... ~a"
            (if *quit-stops-the-port-p* "signaling" "ignoring"))
  (when *quit-stops-the-port-p*
    (signal 'sdl2-exit-port)))

(define-sdl2-request sdl2-exit-port ()
  (signal 'sdl2-exit-port))

;;; This function should be called with synchronization as a timeout - it will
;;; return :pong only when the event is processed (so the loop is processing).
#+ (or) (sdl2-ping :synchronize 0.1)
(define-sdl2-request sdl2-ping ()
  :pong)

;;; This function is for testing.
;;; WARNING this will freeze the event loop for requested amount of time.
(define-sdl2-request sdl2-delay (ms)
  (sdl2:delay ms)
  :done)

;;; This function is for testing.
;;; WARNING this function will signal an error in the event loop.
(define-sdl2-request sdl2-error (message)
  (error message))
