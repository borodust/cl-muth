(cl:in-package :cl-muth)


(defun %make-atomic-counter (value)
  (make-array 1 :element-type t :initial-element value))


(defun %decrement-counter (counter)
  (ccl::atomic-decf (svref counter 0)))
