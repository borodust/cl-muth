(in-package :cl-muth)


(define-constant +iterations-until-wait+ 1024)


(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock))
   (interrupted-p :initform nil)
   (max-size :initform nil :initarg :max-size)
   (state-changed :initform (make-condition-variable))
   (queue :initform '())))


(define-condition interrupted (control-error) ())


(declaim (inline %wait-interruptibly))
(defun %wait-interruptibly (this)
  (with-slots (lock state-changed interrupted-p) this
    (restart-case (cond
                    (interrupted-p
                     (condition-notify state-changed)
                     (error (make-condition 'interrupted)))
                    (t (condition-wait state-changed lock)))
      (continue-blocking-operation ()
        (setf interrupted-p nil)))))


(declaim (inline make-blocking-queue)
         (ftype (function (&optional (or null positive-integer)) *) make-blocking-queue))
(defun make-blocking-queue (&optional (max-size nil))
  (make-instance 'blocking-queue :max-size max-size))


(declaim (ftype (function (blocking-queue *) *) put-into))
(defun put-into (blocking-queue item)
  (with-slots (lock state-changed queue max-size) blocking-queue
    (with-recursive-lock-held (lock)
      (unless (null max-size)
        (loop until (< (length queue) max-size) do
             (%wait-interruptibly blocking-queue)))
      (setf queue (nconc queue (list item)))
      (condition-notify state-changed)))
  item)


(declaim (ftype (function (blocking-queue) *) pop-from))
(defun pop-from (blocking-queue)
  (with-slots (lock state-changed queue interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (loop with i = 0
         for item = (first queue) while (null queue)
         if (null queue) do (incf i) else do (setf i 0)
         when (> i +iterations-until-wait+) do
           (%wait-interruptibly blocking-queue)
	 finally
           (setf queue (rest queue))
           (condition-notify state-changed)
           (return item)))))


(declaim (ftype (function (blocking-queue) *) interrupt))
(defun interrupt (blocking-queue)
  (with-slots (lock state-changed interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (setf interrupted-p t)
      (condition-notify state-changed))))
