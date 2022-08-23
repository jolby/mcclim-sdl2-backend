(in-package :clim-sdl2)


(defclass sdl2-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass sdl2-port (basic-port)
  ((id)
   (pointer            :accessor port-pointer :initform (make-instance 'sdl2-pointer))
   (window             :initform nil :accessor sdl-port-window)
   (sheet-to-window-id :initform (make-hash-table :test 'eql)
                       :reader sdl-port/sheet-to-window-id)
   (window-id-to-sheet :initform (make-hash-table :test 'eql)
                       :reader sdl-port/window-id-to-sheet)))

(defun parse-sdl2-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :sdl2 :port-type) 'sdl2-port)
(setf (get :sdl2 :server-path-parser) 'parse-sdl2-server-path)

(defmethod initialize-instance :after ((port sdl2-port) &rest initargs)
  (declare (ignore initargs))
  (log:info "SDL2 Port initialize-instance: ~a ~%" port)
  (setf (slot-value port 'id) (gensym "SDL2-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'sdl-frame-manager :port port)
        (slot-value port 'climi::frame-managers)))

(defmethod print-object ((object sdl2-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

;; This implements semantics of process-next-event but without distributing
;; the event - there is no need for the port argument.
(defun %next-sdl2 (wait-function timeout)
  (when (maybe-funcall wait-function)
    (return-from %next-sdl2 (values nil :wait-function)))
  (sdl2:with-sdl-event (event)
    (alx:if-let ((ev (%read-sdl2 event timeout)))
      (let ((event-type (sdl2:get-event-type event)))
        (values (handle-sdl2-event event-type event) event-type))
      (if (maybe-funcall wait-function)
          (values nil :wait-function)
          (values nil :timeout)))))


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

(defun %loop-port (port)
  (%init-sdl2)
  (unwind-protect
       (handler-bind ((sdl2-exit-port
                        (lambda (c)
                          (declare (ignore c))
                          (return-from %loop-port))))
         (loop
           (with-simple-restart (ignore "Ignore error and continue.")
             (process-next-event port))))
    (%quit-sdl2)))
