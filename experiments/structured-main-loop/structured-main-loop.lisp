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

(defclass metered-execution-manager (execution-manager cpu-time-meter) ())

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
  (setf (keep-running-p manager) t)
  (log:info "setup: ~a " manager)
  )

(defmethod setup :after ((manager metered-execution-manager))
  (log:info "setup: ~a " manager)
  (reset manager))

(defmethod teardown ((manager execution-manager))
  ;; Cleanup code goes here
  (log:info "teardown: ~a " manager))

(defmethod teardown :after ((manager metered-execution-manager))
  (log:info "teardown: ~a " manager)
  ;; We don't want to do this- we want to see the stats
  ;; (reset manager)
  )

(defmethod perform ((step-phase run-expired-timers))
  ;; Perform the run-expired-timers step-phase
  (log:info "~a" step-phase)
  (sleep .002)
  )

(defmethod perform ((step-phase animate))
  ;; Perform the animate step-phase
  (log:info "~a" step-phase)
  (sleep .001)

  )

(defmethod perform ((step-phase update-geometry))
  ;; Perform the update-geometry step-phase
  (log:info "~a" step-phase)
  (sleep .002)
  )

(defmethod perform ((step-phase draw))
  ;; Perform the draw step-phase
  (log:info "~a" step-phase)
  (sleep .004)
  )

(defmethod perform ((step-phase render))
  ;; Perform the render step-phase
  (log:info "~a" step-phase)
  (sleep .002)

  )

(defmethod initialize-instance :after ((manager execution-manager) &key step-phases)
  (setf (step-phases manager) step-phases))

(defmethod add-timer ((manager execution-manager) (timer timer))
  (add-timer (scheduler manager) timer))

(defmethod remove-timer ((manager execution-manager) (timer timer))
  (remove-timer (scheduler manager) timer))

(defmethod loop-step ((manager execution-manager))
  (dolist (phase (step-phases manager))
    (perform phase)))

(defmethod loop-step ((manager metered-execution-manager))
  (flet ((run-phases () (dolist (phase (step-phases manager))
                          (perform phase))))
    (invoke-with-meter manager #'run-phases)))

(defvar *default-outer-error-handler* #'uiop:print-condition-backtrace)

(defvar *user-default-outer-error-handler* *default-outer-error-handler*)

(defvar *max-epochs* 20)

(defmethod run-loop ((manager execution-manager)
                      &key (outer-error-handler *user-default-outer-error-handler*))
  (handler-bind ((error outer-error-handler))
      (unwind-protect
           (progn
             (setup manager)
             (loop
               :for epoch-count := 0 :then (incf epoch-count)
               :while (and (keep-running-p manager)
                           (< epoch-count *max-epochs*))
               :do (progn
                    (loop-step manager))))
        (teardown manager))))

(defvar *execution-manager* nil)

(defun make-basic-execution-manager ()
  (make-instance 'execution-manager
                 :scheduler (make-instance 'scheduler)
                 :step-phases (list (make-instance 'run-expired-timers :name "Run Expired Timers")
                               (make-instance 'animate :name "Animate")
                               (make-instance 'update-geometry :name "Update Geometry")
                               (make-instance 'draw :name "Draw")
                               (make-instance 'render :name "Render"))))

(defun make-metered-execution-manager ()
  (make-instance 'metered-execution-manager
                 :name "Metered Execution Manager"
                 :scheduler (make-instance 'scheduler)
                 :step-phases (list (make-instance 'run-expired-timers :name "Run Expired Timers")
                                    (make-instance 'animate :name "Animate")
                                    (make-instance 'update-geometry :name "Update Geometry")
                                    (make-instance 'draw :name "Draw")
                                    (make-instance 'render :name "Render"))))

;;;; (setf *execution-manager* (make-basic-execution-manager))
;;;; (setf *execution-manager* (make-metered-execution-manager))
;;;; (setf (keep-running-p *execution-manager*) t)
;;;; (setf (keep-running-p *execution-manager*) nil)
;;;; (run-loop *execution-manager*)
;;;; (progn (bt:make-thread #'(lambda () (run-loop *execution-manager*))))
