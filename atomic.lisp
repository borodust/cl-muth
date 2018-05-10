(cl:in-package :cl-muth)


(defun make-atomic-counter (&optional (value 0))
  (%make-atomic-counter value))


(defun decrement-atomic-counter (counter)
  "Returns new value"
  (%decrement-counter counter))
