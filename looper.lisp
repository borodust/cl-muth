(in-package :cl-muth)


(defclass looper () ())

(defgeneric start-looper (looper))
(defgeneric stop-looper (looper))
(defgeneric add-to-loop (looper callback))

(defmacro within-loop ((looper) &body body)
  `(add-to-loop ,looper #'(lambda () ,@body)))


(defclass default-looper (looper)
  ((thread :initform nil)
   (lock :initform (bt:make-recursive-lock "looper-lock"))
   (queue :initform (make-instance 'blocking-queue))
   (invoker :initarg :invoker :initform (lambda (cb) (funcall cb)))))


(defmethod start-looper ((this default-looper))
  (with-slots (thread invoker queue lock) this
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


(defmethod stop-looper ((this default-looper))
  (with-slots (thread lock queue) this
    (let ((th nil))
      (with-recursive-lock-held (lock)
	(unless (null thread)
	  (setf th thread)
	  (setf thread nil)
	  (interrupt queue)))
      (when th
	(join-thread th)))))




(defmethod add-to-loop ((this default-looper) callback)
  (with-slots (queue lock thread) this
    (with-recursive-lock-held (lock)
      (when (null thread)
	(error "looper inactive")))
    (put-into queue callback)))
