(in-package :cl-muth)


;;;
;;;
;;;
(defclass simple-looper ()
  ((thread :initform nil)
   (lock :initform (bt:make-recursive-lock "looper-lock"))
   (queue :initform (make-instance 'blocking-queue))
   (invoker :initarg :invoker)))


(declaim (inline make-looper))
(defun make-looper (&optional (invoker (lambda (cb) (funcall cb))))
  (make-instance 'simple-looper :invoker invoker))


(declaim (ftype (function (simple-looper) *) start-looper))
(defun start-looper (looper)
  (with-slots (thread invoker queue lock) looper
    (with-recursive-lock-held (lock)
      (when (null thread)
	(setf thread
	      (make-thread #'(lambda ()
			       (log:trace "looper thread started")
			       (loop
				  (with-recursive-lock-held (lock)
				    (unless thread
				      (return)))
				  (handler-case
				      (funcall invoker (pop-from queue))
				    (interrupted () (log:trace "looper interrupted"))
				    (t (e) (log:error "Unhandled invoker error: ~a" e))))
			       (log:trace "looper thread exiting"))
			   :name "looping-thread"))))))


(declaim (ftype (function (simple-looper) *) stop-looper))
(defun stop-looper (looper)
  (with-slots (thread lock queue) looper
    (let ((th nil))
      (with-recursive-lock-held (lock)
	(unless (null thread)
	  (setf th thread)
	  (setf thread nil)
	  (interrupt queue)))
      (when th
	(join-thread th)))))


(declaim (ftype (function (simple-looper (function () *)) *) add-to-loop))
(defun add-to-loop (looper fn)
  (with-slots (queue lock thread) looper
    (with-recursive-lock-held (lock)
      (when (null thread)
	(error "looper inactive")))
    (put-into queue fn)))


(defmacro within-loop ((looper) &body body)
  `(add-to-loop ,looper #'(lambda () ,@body)))
