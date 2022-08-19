(in-package :clim-sdl)

(defvar *sdl-loop-count* 0)
(defvar *sdl-loop-pause* t)

(defun maybe-pause ()
  (when *sdl-loop-pause (sleep 0.5)))

(defun init-sdl (port)
  (sdl2:init :everything)
  (when clim-sys:*multiprocessing-p*
    (setf (climi::port-event-process port)
          (clim-sys:make-process (lambda ()
                                   (setf *sdl-loop-count* 0)
                                   (loop
                                     (with-simple-restart
                                         (restart-event-loop "Restart event loop")
                                       (loop
                                         do (process-next-event port)
                                         do (incf *sdl-loop-count*)
                                         do (maybe-pause)))))
                                 :name (format nil "SDL event process for port: ~s" port)))))

(defun quit-sdl (port)
  (declare (ignore port))
  (sdl2:quit))

(defun round-coordinate (x)
  (floor (+ x .5)))
