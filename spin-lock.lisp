(cl:in-package :cl-muth)


(defun make-spin-lock ()
  (%make-spin-lock))


(defun acquire-spin-lock (spin-lock)
  (%acquire-spin-lock spin-lock))


(defun release-spin-lock (spin-lock)
  (%release-spin-lock spin-lock))


(defmacro with-spin-lock-held ((spin-lock) &body body)
  (once-only (spin-lock)
    `(unwind-protect
          (progn
            (acquire-spin-lock ,spin-lock)
            ,@body)
       (release-spin-lock ,spin-lock))))
