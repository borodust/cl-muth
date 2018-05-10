(cl:in-package :cl-muth)

(defmacro %compare-and-swap (place old new)
  "Tries to atomically set a value of PLACE to be NEW if it
was EQ to OLD, returning non-nil if successful."
  (once-only (old)
    `(eq ,old (sb-ext:compare-and-swap ,place ,old ,new))))


(defstruct (spin-lock
            (:constructor %%make-spin-lock ()))
  (owned nil :type t))


(defun %make-spin-lock ()
  (%%make-spin-lock))


(defun %acquire-spin-lock (spin-lock)
  (loop until (%compare-and-swap (spin-lock-owned spin-lock) nil t)
     do (sleep +cas-sleep+)))


(defun %release-spin-lock (spin-lock)
  (loop until (%compare-and-swap (spin-lock-owned spin-lock) t nil)
     do (sleep +cas-sleep+)))
