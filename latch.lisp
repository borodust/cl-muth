(in-package :cl-muth)

(defclass simple-latch ()
  ((counter :initarg :count :initform 1)
   (state-changed :initform (bt:make-condition-variable :name "latch-state"))
   (lock :initform (bt:make-recursive-lock "latch-lock"))))

(defun make-latch (&optional (count 1))
  (make-instance 'simple-latch :count count))

(defun wait-for-latch (latch)
  (check-type latch simple-latch)
  (with-slots (state-changed lock counter) latch
    (bt:with-recursive-lock-held (lock)
      (loop while (> counter 0) do
	   (bt:condition-wait state-changed lock)))))

(defun open-latch (latch)
  (check-type latch simple-latch)
  (with-slots (state-changed lock counter) latch
    (bt:with-recursive-lock-held (lock)
      (when (> counter 0)
	(decf counter)
	(condition-notify state-changed)))))

(defmacro wait-with-latch ((latch-name &optional (count 1)) &body body)
  `(let ((,latch-name (make-latch ,count)))
     ,@body
     (wait-for-latch ,latch-name)))
