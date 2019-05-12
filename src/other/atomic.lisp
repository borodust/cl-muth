(cl:in-package :cl-muth)


(defun %make-atomic-counter (value)
  (make-guarded-reference value))


(defun %decrement-counter (counter)
  (with-guarded-reference (counter)
    (decf counter)))
