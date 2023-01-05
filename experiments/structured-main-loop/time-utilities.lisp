(in-package #:mcclim-sdl2)

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun time-diff-seconds (t1 t2)
  (/ (float (- (get-internal-run-time) t1)) internal-time-units-per-second))

(defmacro elapsed-time-seconds (&body body)
  `(sb-sys:without-interrupts
     (let ((t1 (get-internal-run-time)))
       (values
        (progn ,@body)
        (/ (float (- (get-internal-run-time) t1)) internal-time-units-per-second)))))

(defun now ()
  (get-internal-real-time))

(defun now-unix-time ()
  (universal-to-unix-time (now)))

(defun now-as-float-seconds ()
  (/ (float (get-internal-real-time)) internal-time-units-per-second))
