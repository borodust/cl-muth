(in-package :cl-muth.definition)

(defpackage :cl-muth
  (:nicknames :mt :muth)
  (:use :cl :alexandria :bordeaux-threads)
  (:export guard-object
	   with-guarded-object

	   ; lifo
	   blocking-queue
	   interrupted
	   
	   interrupt
	   put-into
	   pop-from

	   default-looper

	   start-looper
	   stop-looper
	   add-to-loop
	   within-loop

	   simple-latch
	   wait-for-latch
	   open-latch))
