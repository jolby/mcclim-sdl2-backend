(cl:in-package #:mcclim-sdl2)

(defvar *domain-counter* nil)
(declaim (special *domain-counter*))

(defclass basic-atomic-domain-counter ()
  ((%domain->counter :reader domain->counter :initform (make-hash-table :test 'eq))
   (%allowed-domains :reader allowed-domains :initarg :allowed-domains)))

(defun %init-domain-counters (allowed-domains counter-ht)
  (loop :for domain-key :in allowed-domains
        :do (setf (gethash domain-key counter-ht (stmx.lang:make-atomic-counter))))

(defmethod initialize-instance :after ((domain-counter basic-atomic-domain-counter) &key allowed-domains)
  (unless (and allowed-domains
               (> (length allowed-domains) 0)
               (every #'keywordp allowed-domains))
    (error "allowed-domains param must be a non-null sequence of keywords."))
  (%init-domain-counters allowed-domains (domain->counter domain-counter))
  (loop :for domain-key :in allowed-domains
        :do (setf (gethash domain-key (domain->counter domain-counter)) (stmx.lang:make-atomic-counter))))

(defun domain-counter-incf (domain-key &key domain-counter (delta 1))
  (let ((counter (or domain-counter *domain-counter*)))
    (unless counter
      (error "No provided :domain-counter keyword arg, nor is the *domain-counter* special set"))
    (check-type domain-key keyword)
    (check-type counter basic-atomic-domain-counter)
    (multiple-value-bind (atomic-counter found-p) (gethash domain-key (domain->counter counter))
      (unless found-p (error (format nil "Unknown domain: ~a. Domain must be one of: ~a"
                                     domain-key (allowed-domains counter))))
      (stmx.lang:incf-atomic-counter atomic-counter delta))))

(defun domain-counter-value (domain-key &key domain-counter)
  (let ((counter (or domain-counter *domain-counter*)))
    (unless counter
      (error "No provided :domain-counter keyword arg, nor is the *domain-counter* special set"))
    (check-type domain-key keyword)
    (check-type counter basic-atomic-domain-counter)
    (multiple-value-bind (atomic-counter found-p) (gethash domain-key (domain->counter counter))
      (unless found-p (error (format nil "Unknown domain: ~a. Domain must be one of: ~a"
                                     domain-key (allowed-domains counter))))
      (stmx.lang:get-atomic-counter atomic-counter))))

(defun domain-counter-values-alist (&key domain-counter)
  (let ((counter (or domain-counter *domain-counter*)))
    (unless counter
      (error "No provided :domain-counter keyword arg, nor is the *domain-counter* special set"))
    (check-type counter basic-atomic-domain-counter)
    (let ((counter-alist ()))
      (maphash #'(lambda (key value)
                   (push (cons key (stmx.lang:get-atomic-counter value)) counter-alist))
               (domain->counter counter))
      counter-alist)))


(defun domain-counter-reset (domain-key &key counter))
(defun domain-counter-reset-all (&key counter))
