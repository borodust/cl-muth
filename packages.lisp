(in-package :cl-user)

(defpackage :muth
  (:use :cl :alexandria :bordeaux-threads)
  (:export guard-object
	   with-guarded-object

	   ; lifo
	   blocking-queue
	   
	   put-into
	   pop-from))
