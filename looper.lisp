(in-package :cl-muth)


;;;
;;;
;;;
(defclass default-looper ()
  ((thread :initform nil)
   (lock :initform (bt:make-recursive-lock "looper-lock"))
   (queue :initform (make-instance 'blocking-queue))
   (invoker :initarg :invoker)))

(defun make-looper (&optional (invoker (lambda (cb) (funcall cb))))
  (make-instance 'default-looper :invoker invoker))

(defun start-looper (looper)
  (check-type looper default-looper)
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


(defun stop-looper (looper)
  (check-type looper default-looper)
  (with-slots (thread lock queue) looper
    (let ((th nil))
      (with-recursive-lock-held (lock)
	(unless (null thread)
	  (setf th thread)
	  (setf thread nil)
	  (interrupt queue)))
      (when th
	(join-thread th)))))


(defun add-to-loop (looper fn)
  (check-type looper default-looper)
  (with-slots (queue lock thread) looper
    (with-recursive-lock-held (lock)
      (when (null thread)
	(error "looper inactive")))
    (put-into queue fn)))

(defmacro within-loop ((looper) &body body)
  `(add-to-loop ,looper #'(lambda () ,@body)))
