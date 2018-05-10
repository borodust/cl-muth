(cl:in-package :cl-muth)


(deftype atomic-fixnum ()
  `(unsigned-byte #+X86-64 64
                  #-X86-64 32))
