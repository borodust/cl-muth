(in-package :cl-muth)

(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock))
   (interrupted-p :initform nil)
   (item-available :initform (make-condition-variable))
   (queue :initform '())))


(define-condition interrupted (control-error) ())


(declaim (inline make-blocking-queue))
(defun make-blocking-queue ()
  (make-instance 'blocking-queue))


(declaim (ftype (function (blocking-queue *) *) put-into))
(defun put-into (blocking-queue item)
  (with-slots (lock item-available queue) blocking-queue
    (with-recursive-lock-held (lock)
      (setf queue (nconc queue (list item)))
      (condition-notify item-available)))
  item)


(declaim (ftype (function (blocking-queue) *) pop-from))
(defun pop-from (blocking-queue)
  (with-slots (lock item-available queue interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (loop for item = (first queue) while (null queue) do
	   (restart-case (cond
			   (interrupted-p (setf interrupted-p nil)
                                          (error (make-condition 'interrupted)))
			   (t (condition-wait item-available lock)))
	     (continue-blocking-operation ()
	       (setf interrupted-p nil)))
	 finally (setf queue (rest queue))
	 finally (return item)))))


(declaim (ftype (function (blocking-queue) *) interrupt))
(defun interrupt (blocking-queue)
  (with-slots (lock item-available interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (setf interrupted-p t)
      (condition-notify item-available))))
