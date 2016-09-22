(in-package :cl-muth.definition)

(defpackage :cl-muth
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
	   pop-from

	   make-looper
	   start-looper
	   stop-looper
	   add-to-loop
	   within-loop

	   make-latch
           wait-with-latch
	   wait-for-latch
	   open-latch))
