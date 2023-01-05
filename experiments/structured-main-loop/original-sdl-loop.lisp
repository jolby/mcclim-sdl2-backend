(in-package #:mcclim-sdl2)

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
              (clim-sys:make-process (alx:curry #'%loop-port port)))
        (loop until *initialized-p*
              do (clim-sys:with-lock-held (*initialized-lock*)
                   (clim-sys:condition-wait *initialized-cv* *initialized-lock*))))
      (%init-sdl2)))

(defun %quit-port (port)
  (if *multiprocessing-p*
      (%quit-loop port)
      (%quit-sdl2)))

(defun %quit-loop (port)
  (alx:when-let ((main (main-thread port)))
    (when (bt:thread-alive-p main)
      (sdl2-exit-port)
      (bt:join-thread main))
    (setf (main-thread port) nil)))

(defvar *ignore-loop-errors* t)

(defun %loop-port (port)
  (%init-sdl2)
  (unwind-protect
       (handler-bind ((sdl2-exit-port
                        (lambda (c)
                          (declare (ignore c))
                          (return-from %loop-port)))
                      (error
                        (lambda (c)
                          (when *ignore-loop-errors*
                            (log:error "Ignoring the error ~a" c)
                            (invoke-restart (find-restart 'ignore c))))))
         (loop
           (with-simple-restart (ignore "Ignore error and continue.")
             (process-next-event port))))
    (setf (main-thread port) nil)
    (%quit-sdl2)))
