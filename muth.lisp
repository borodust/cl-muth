(in-package :muth)

(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock))
   (item-available :initform (make-condition-variable))
   (queue :initform '())))

(defgeneric put-into (blocking-queue obj))
(defgeneric pop-from (blocking-queue))


(defmethod put-into ((this blocking-queue) obj)
  (with-slots (lock item-available queue) this
    (with-recursive-lock-held (lock)
      (setf queue (nconc queue (list obj)))
      (condition-notify item-available)))
  obj)


(defmethod pop-from ((this blocking-queue))
  (with-slots (lock item-available queue) this
    (with-recursive-lock-held (lock)
      (loop for item = (first queue) while (null item) do
	   (condition-wait item-available lock)
	 finally (setf queue (rest queue))
	 finally (return item)))))



(defclass guarded-reference ()
  ((lock :initform (make-recursive-lock))
   (ref :initarg :reference :initform nil)))


(defun guard-object (object)
  (make-instance 'guarded-reference :reference object))


(defmacro with-guarded-object ((local-ref-name &optional global-ref-name) &body body)
  (with-gensyms (lock)
    (once-only ((ref (cond
		       (global-ref-name)
		       (t local-ref-name))))
      `(with-slots ((,lock lock) (,local-ref-name ref)) ,ref
	 (with-recursive-lock-held (,lock)
	   ,@body)))))
