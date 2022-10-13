(in-package #:mcclim-skia)

;;;
;;; DRAW COMMAND
;;;
(defstruct (draw-command
            (:constructor make-draw-command ())
            (:conc-name %draw-command-))
  (name nil)
  (arguments nil))

(defun update-draw-command (command name &rest arguments)
  (setf (%draw-command-name command) name
        (%draw-command-arguments command) arguments))

(defun invoke-draw-command (command)
  (log:info "CMD: ~a" (%draw-command-name command))
  (apply (%draw-command-name command) (%draw-command-arguments command)))

;;;
;;; COMMAND QUEUE
;;;
(defvar *command-queue-lock* (bt:make-lock))


(defun push-command (medium name &rest args)
  (bt:with-lock-held (*command-queue-lock*)
    (let* ((queue (medium-deferred-command-queue medium))
           (fp (fill-pointer queue)))
      (if (= fp (array-total-size queue))
          (vector-push-extend nil queue 128)
          (setf (fill-pointer queue) (1+ fp)))
      (let ((next-command (alx:if-let ((next-command (aref queue fp)))
                            next-command
                            (setf (aref queue fp) (make-draw-command)))))
        (apply #'update-draw-command next-command name args))) )
  (values))

(defun drain-draw-commands (medium)
  (bt:with-lock-held (*command-queue-lock*)
    (let ((array (medium-deferred-command-queue medium)))
      (log:info "fill pointer: ~a" (fill-pointer array))
      (unwind-protect
           (loop for command across array
                 do (invoke-draw-command command)
                    (update-draw-command command nil))
        (setf (fill-pointer array) 0)))) )

(defun discard-command-queue (medium)
  (setf (fill-pointer (medium-deferred-command-queue medium)) 0)
  (values))
