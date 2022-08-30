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

(defun run-event-handler (event event-type event-id)
  ;;Enclose the event-handler/future result in
  ;;an unwind-protect and collect any error to return as
  ;;a future result. Also, need to cleanup any user data
  ;;attached to the event.
  (unwind-protect
       (let ((handler-result nil)
             (error? nil))
         (handler-case
             (setf handler-result (handle-sdl2-event event-type event))
           (t (c)
             (log:error "~a" c)
             (setf handler-result c
                   error? t)))
         (when-let ((fr (%get-future-result-for-event event)))
           (let ((lock (fres-lock fr))
                 (cv (fres-completion-cv fr)))
             (setf (fres-value-provided? fr) t
                   (fres-state fr) (if error? :error :success)
                   (fres-value fr) handler-result)
             (bt:with-lock-held (lock)
               (bt:condition-notify cv))))
         (values handler-result event-type))
    ;;ensure we release any user data we may have attached to the event
    (when (and event-id
               (not (eq event-type :lisp-message)))
      (sdl2::free-user-data event-id)) ))

;; This implements semantics of process-next-event but without distributing
;; the event - there is no need for the port argument.
(defun %next-sdl2 (wait-function timeout)
  (when (maybe-funcall wait-function)
    (return-from %next-sdl2 (values nil :wait-function)))
  (sdl2:with-sdl-event (event)
    (if (%read-sdl2 event timeout)
        (let* ((event-type (sdl2:get-event-type event))
               (event-id (and (sdl2::user-event-type-p event-type)
                              (%get-user-event-id event))))
          (run-event-handler event event-type event-id))
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

(defvar *max-loops* 500)
(defvar *completed-loops* 0)
(defvar *keep-running* nil)
(defvar *is-running* nil)
(defvar *sdl-loop-timeout* 500)


(defun %port-loop-step (port)
  (unwind-protect
       (progn
         (process-next-event port :timeout *sdl-loop-timeout*))
    (incf *completed-loops*)))

(defun %port-loop (port)
  (unless port (error "Null port! Can't loop"))
    (unwind-protect
         (handler-bind ((error
                          (lambda (condition)
                            (trivial-backtrace:print-backtrace condition)
                            (return-from %port-loop)))
                        (sdl2-exit-port
                          (lambda (c)
                            (log:info "sdl2-exit-port handler: ~a " c)
                            (return-from %port-loop))))

           (setf *keep-running* t)
           (setf *is-running* t)
           (setf *completed-loops* 0)
           (loop
             (with-simple-restart (ignore "Ignore error and continue.")
               ;; (when (> *completed-loops* *max-loops*)
               ;;   (signal 'sdl2-exit-port :report (format nil "completed max: ~a loops ~%" *completed-loops*)))
               (unless *keep-running*
                 (signal 'sdl2-exit-port :report (format nil "keep-running nil. completed ~a loops ~%" *completed-loops*)))
               (%port-loop-step port))))
      (%quit-sdl2)
      (setf *is-running* nil)))

(defun run-port-loop-in-main-thread (port)
  (%init-sdl2)
  ;; XXX Figure out which one to use
  ;; (sdl2:in-main-thread ()
  (tmt:call-in-main-thread (lambda () (%port-loop port))))
