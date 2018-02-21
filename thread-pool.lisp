(in-package :cl-muth)

(defstruct (thread-pool
             (:conc-name tp-)
             (:constructor make-thread-pool (pool-size)))
  (pool-size 1 :read-only t)
  (lock (make-lock "thread-pool-lock") :read-only t)
  (enabled-p nil)
  (workers nil)
  (blocking-queue nil))


(defun %make-thread-pool-worker (pool name)
  (flet ((run ()
           (tagbody start
              (handler-bind ((interrupted (lambda (c)
                                            (declare (ignore c))
                                            (go end)))) ; just exit the thread
                (restart-case
                    (loop for item = (pop-from (tp-blocking-queue pool)) when (functionp item) do
                      (funcall item))
                  (continue-execution () (go start))))
            end)))
    (make-thread #'run :name name)))


(declaim (ftype (function (thread-pool &optional string) *) open-pool))
(defun open-pool (pool &optional (name "thread-pool-worker"))
  (with-lock-held ((tp-lock pool))
    (when (tp-enabled-p pool)
      (error "Pool already active"))
    (setf (tp-blocking-queue pool) (make-blocking-queue)
          (tp-enabled-p pool) t
          (tp-workers pool) (loop for i from 0 below (tp-pool-size pool)
                               collecting (%make-thread-pool-worker pool name)))
    pool))


(declaim (ftype (function (thread-pool (function () *) &optional blocking-queue-item-priority) *)
                push-to-pool))
(defun push-to-pool (pool fn &optional (priority :medium))
  (with-lock-held ((tp-lock pool))
    (unless (tp-enabled-p pool)
      (error "Pool inactive"))
    (put-into (tp-blocking-queue pool) fn priority)))


(declaim (ftype (function (thread-pool) *) close-pool))
(defun close-pool (pool)
  (with-lock-held ((tp-lock pool))
    (unless (tp-enabled-p pool)
      (error "Pool already inactive"))
    (interrupt (tp-blocking-queue pool))
    (setf (tp-enabled-p pool) nil)))


(declaim (ftype (function (thread-pool) (values t)) pool-alive-p))
(defun pool-alive-p (pool)
  (loop for worker in (tp-workers pool) thereis (thread-alive-p worker)))


(defmacro within-pool ((pool-place &optional (priority :medium)) &body body)
  `(push-to-pool ,pool-place (lambda () ,@body) ,priority))
