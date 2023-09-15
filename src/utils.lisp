(cl:in-package :cl-muth)

(define-constant +cas-sleep+ (/ 1 internal-time-units-per-second))


(deftype atomic-fixnum ()
  `(unsigned-byte #+(:or :x86-64 :x86_64 :arm64 :aarch64 :ppc64) 64
                  #-(:or :x86-64 :x86_64 :arm64 :aarch64 :ppc64) 32))


(defun current-seconds ()
  (float (/ (get-internal-real-time)
            internal-time-units-per-second)
         0d0))
