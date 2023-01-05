(in-package #:mcclim-sdl2)


(defclass scheduler ()
  ((timers :accessor timers :initarg :timers :initform '())))

(defclass timer ()
  ((name :accessor name :initarg :name)
   (interval :accessor interval :initarg :interval)
   (task :accessor task :initarg :task)))

(defgeneric add-timer (scheduler timer))

(defgeneric remove-timer (scheduler timer))

(defmethod process-timers ((scheduler scheduler))
  (dolist (timer (timers scheduler))
    (when (time-to-execute-p timer)
      (execute-task timer))))

(defmethod time-to-execute-p ((timer timer))
                                        ; Check if it is time to execute the task for the timer
  )

(defmethod execute-task ((timer timer))
                                        ; Execute the task for the timer
  )

(defmethod add-timer ((scheduler scheduler) (timer timer))
  (push timer (timers scheduler)))


(defmethod remove-timer ((scheduler scheduler) (timer timer))
  (setf (timers scheduler) (remove timer (timers scheduler))))

