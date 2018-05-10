(cl:in-package :cl-muth)


(defstruct (atomic-counter
             (:constructor %%make-atomic-counter (value)))
  (value 0 :type atomic-fixnum))


(defun %make-atomic-counter (value)
  (%%make-atomic-counter (coerce value 'atomic-fixnum)))


(defun %decrement-counter (counter)
  (1- (sb-ext:atomic-decf (atomic-counter-value counter))))
