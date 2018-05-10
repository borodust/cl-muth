(cl:defpackage :cl-muth
  (:nicknames :mt :muth)
  (:use :cl :alexandria :bordeaux-threads)
  (:export make-guarded-reference
	   with-guarded-reference
           guarded-value-of

	   ; fifo
	   make-blocking-queue
	   interrupted
	   interrupt
	   put-into
           try-put-into
           try-put-replacing
	   pop-from

	   make-latch
           wait-with-latch
	   wait-for-latch
	   open-latch

           make-thread-pool
           open-pool
           push-to-pool
           close-pool
           pool-alive-p
           within-pool

           make-atomic-counter
           decrement-atomic-counter

           make-spin-lock
           acquire-spin-lock
           release-spin-lock
           with-spin-lock-held))
