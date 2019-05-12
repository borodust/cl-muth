(cl:in-package :cl-muth.tests)

(defun %current-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))
