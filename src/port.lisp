(in-package :clim-sdl2)

(defvar *sdl2-port* nil)

(defun parse-sdl2-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :sdl2 :port-type) 'sdl2-port)
(setf (get :sdl2 :server-path-parser) 'parse-sdl2-server-path)

(defmethod initialize-instance :after ((port sdl2-port) &rest initargs)
  (declare (ignore initargs))
  (%sdl-port-id (gensym "SDL2-PORT-") port)
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'sdl2-frame-manager :port port)
        (slot-value port 'climi::frame-managers))
    (log:info "SDL2 Port initialize-instance: ~a ~%" port))

(defmethod print-object ((object sdl2-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (sdl-port-id object))))

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

(defvar *max-loops* 10)
(defvar *completed-loops* 0)
(defvar *sdl-loop-timeout* 500)


(defun %port-loop-step (port)
  (unwind-protect
       (progn
         (process-next-event port :timeout *sdl-loop-timeout*))
    (incf *completed-loops*)))

(defun %port-loop (port)
  (%init-sdl2)

  (tmt:with-body-in-main-thread ()
  ;; (sdl2:in-main-thread ()
    (unwind-protect
         (handler-bind ((error
                          (lambda (condition)
                            (trivial-backtrace:print-backtrace condition)
                            (return-from %port-loop)))
                        (sdl2-exit-port
                          (lambda (c)
                            (format t "sdl2-exit-port handler: ~a ~%" c)
                            (return-from %port-loop))))

           (setf *completed-loops* 0)
           (loop
             (with-simple-restart (ignore "Ignore error and continue.")
               (when (> *completed-loops* *max-loops*)
                 (signal 'sdl2-exit-port :report (format nil "completed max: ~a loops ~%" *completed-loops*)))
               (%port-loop-step port)
               )))
      (%quit-sdl2))))
