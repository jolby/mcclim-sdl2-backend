(in-package #:mcclim-sdl2)

(defclass step-phase ()
  ((%name :accessor name :initarg :name)))

(defclass run-expired-timers (step-phase) ())
(defclass animate (step-phase) ())
(defclass update-geometry (step-phase) ())
(defclass draw (step-phase) ())
(defclass render (step-phase) ())

(defclass execution-manager ()
  ((%scheduler :accessor scheduler :initarg :scheduler)
   (%step-phases :accessor step-phases :initarg :step-phases :initform '())
   (%keep-running-p :accessor keep-running-p :initform 'nil)))

(defgeneric perform (step-phase))

(defgeneric setup (manager))

(defgeneric teardown (manager))

(defgeneric start (manager))

(defgeneric pause (manager))

(defgeneric stop (manager))

(defgeneric run-loop (manager &key outer-error-handler))

(defgeneric loop-step (manager))

(defmethod setup ((manager execution-manager))
  ;; Setup code goes here
  (log:info "~a " manager)
  )

(defmethod teardown ((manager execution-manager))
  ;; Cleanup code goes here
  (log:info "~a " manager)
  )

(defmethod perform ((step-phase run-expired-timers))
  ;; Perform the run-expired-timers step-phase
  )

(defmethod perform ((step-phase animate))
  ;; Perform the animate step-phase
  )

(defmethod perform ((step-phase update-geometry))
  ;; Perform the update-geometry step-phase
  )

(defmethod perform ((step-phase draw))
  ;; Perform the draw step-phase
  )

(defmethod perform ((step-phase render))
  ;; Perform the render step-phase
  )

(defmethod initialize :after ((manager execution-manager) &key phases)
  (setf (phases manager) phases))

(defmethod add-timer ((manager execution-manager) (timer timer))
  (add-timer (scheduler manager) timer))

(defmethod remove-timer ((manager execution-manager) (timer timer))
  (remove-timer (scheduler manager) timer))

(defmethod loop-step ((manager execution-manager))
  (dolist (phase (phases manager))
    (perform phase)))

(defvar *default-outer-error-handler* #'uiop:print-condition-backtrace)

(defvar *user-default-outer-error-handler* *default-outer-error-handler*)

(defmethod run-loop ((manager execution-manager)
                      &key (outer-error-handler *user-default-outer-error-handler*))
  (handler-bind ((error outer-error-handler))
    (unwind-protect
         (progn
           (setup manager)
           (loop
             (when (keep-running-p manager)
               (loop-step manager)))
           (teardown manager))
      (teardown manager))))

(defvar *execution-manager* nil)

(defun make-basic-execution-manager ()
  (make-instance 'execution-manager
                 :scheduler (make-instance 'scheduler)
                 :phases (list (make-instance 'run-expired-timers :name "Run Expired Timers")
                               (make-instance 'animate :name "Animate")
                               (make-instance 'update-geometry :name "Update Geometry")
                               (make-instance 'draw :name "Draw")
                               (make-instance 'render :name "Render"))))
