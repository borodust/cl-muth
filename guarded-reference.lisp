(in-package :cl-muth)

(defclass guarded-reference ()
  ((lock :initform (make-recursive-lock))
   (ref :initarg :reference :initform nil)))


(declaim (inline make-guarded-reference))
(defun make-guarded-reference (object)
  (make-instance 'guarded-reference :reference object))


(defmacro with-guarded-reference ((local-ref-name &optional global-ref-name) &body body)
  (with-gensyms (lock)
    (once-only ((ref (cond
		       (global-ref-name)
		       (t local-ref-name))))
      `(with-slots ((,lock lock) (,local-ref-name ref)) ,ref
	 (with-recursive-lock-held (,lock)
	   ,@body)))))


(declaim (ftype (function (guarded-reference) *) guarded-value-of))
(defun guarded-value-of (guarded-ref)
  (with-guarded-reference (guarded-ref)
      guarded-ref))


(declaim (ftype (function (* guarded-reference) *) (setf guarded-value-of)))
(defun (setf guarded-value-of) (value guarded-ref)
  (with-guarded-reference (guarded-ref)
      (setf guarded-ref value)))
