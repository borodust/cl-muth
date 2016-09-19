(in-package :cl-muth)

(defclass latch () ())

(defgeneric open-latch (latch))
(defgeneric wait-for-latch (latch))

(defclass simple-latch (latch)
  ((counter :initarg :count :initform 1)
   (state-changed :initform (bt:make-condition-variable :name "latch-state"))
   (lock :initform (bt:make-recursive-lock "latch-lock"))))


(defmethod wait-for-latch ((this simple-latch))
  (with-slots (state-changed lock counter) this
    (bt:with-recursive-lock-held (lock)
      (loop while (> counter 0) do
	   (bt:condition-wait state-changed lock)))))

(defmethod open-latch ((this simple-latch))
  (with-slots (state-changed lock counter) this
    (bt:with-recursive-lock-held (lock)
      (when (> counter 0)
	(decf counter)
	(condition-notify state-changed)))))



