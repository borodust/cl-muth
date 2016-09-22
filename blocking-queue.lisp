(in-package :cl-muth)

(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock))
   (interrupted-p :initform nil)
   (item-available :initform (make-condition-variable))
   (queue :initform '())))

(defun make-blocking-queue ()
  (make-instance 'blocking-queue))

(define-condition interrupted (control-error) ())

(defun put-into (blocking-queue fn)
  (check-type blocking-queue blocking-queue)
  (with-slots (lock item-available queue) blocking-queue
    (with-recursive-lock-held (lock)
      (setf queue (nconc queue (list fn)))
      (condition-notify item-available)))
  fn)


(defun pop-from (blocking-queue)
  (check-type blocking-queue blocking-queue)
  (with-slots (lock item-available queue interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (loop for item = (first queue) while (null item) do
	   (restart-case (cond
			   (interrupted-p (setf interrupted-p nil)
                                          (error (make-condition 'interrupted)))
			   (t (condition-wait item-available lock)))
	     (continue-blocking-operation ()
	       (setf interrupted-p nil)))
	 finally (setf queue (rest queue))
	 finally (return item)))))


(defun interrupt (blocking-queue)
  (check-type blocking-queue blocking-queue)
  (with-slots (lock item-available interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (setf interrupted-p t)
      (condition-notify item-available))))
