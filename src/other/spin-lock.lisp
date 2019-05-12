(cl:in-package :cl-muth)


(defun %make-spin-lock ()
  (make-lock "spinny-whinny-fallback-lock"))


(defun %acquire-spin-lock (spin-lock)
  (acquire-lock spin-lock t))


(defun %release-spin-lock (spin-lock)
  (release-lock spin-lock))
