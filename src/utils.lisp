(cl:in-package :cl-muth)

(define-constant +cas-sleep+ (/ 1 internal-time-units-per-second))


(deftype atomic-fixnum ()
  `(unsigned-byte #+X86-64 64
                  #-X86-64 32))


(defun current-seconds ()
  (float (/ (get-internal-real-time)
            internal-time-units-per-second)
         0d0))
