(in-package :muth)

(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock))
   (interrupted :initform nil)
   (item-available :initform (make-condition-variable))
   (queue :initform '())))

(defgeneric put-into (blocking-queue obj))
(defgeneric pop-from (blocking-queue))
(defgeneric interrupt (blocking-queue))

(define-condition interrupted (control-error)())

(defmethod put-into ((this blocking-queue) obj)
  (with-slots (lock item-available queue) this
    (with-recursive-lock-held (lock)
      (setf queue (nconc queue (list obj)))
      (condition-notify item-available)))
  obj)


(defmethod pop-from ((this blocking-queue))
  (with-slots (lock item-available queue interrupted) this
    (with-recursive-lock-held (lock)
      (loop for item = (first queue) while (null item) do
	   (restart-case (cond
			   (interrupted (setf interrupted nil)
					(error (make-condition 'interrupted)))
			   (t (condition-wait item-available lock)))
	     (continue-blocking-operation ()
	       (setf interrupted nil)))
	 finally (setf queue (rest queue))
	 finally (return item)))))


(defmethod interrupt ((this blocking-queue))
  (with-slots (lock item-available interrupted) this
    (with-recursive-lock-held (lock)
      (setf interrupted t)
      (condition-notify item-available))))
