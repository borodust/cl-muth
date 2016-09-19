(in-package :cl-muth)

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
