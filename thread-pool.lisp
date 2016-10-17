(in-package :cl-muth)

(defstruct (thread-pool
             (:conc-name tp-)
             (:constructor make-thread-pool (pool-size)))
  (pool-size 1 :read-only t)
  (lock (make-lock "thread-pool-lock") :read-only t)
  (enabled-p nil)
  (blocking-queue nil))


(defun %make-thread-pool-worker (pool)
  (make-thread
   (lambda ()
     (handler-case
         (loop for item = (pop-from (tp-blocking-queue pool)) when (functionp item) do
              (funcall item))
       (interrupted ()))) ; just exit the thread
   :name "thread-pool-worker"))


(declaim (ftype (function (thread-pool) *) open-pool))
(defun open-pool (pool)
  (with-lock-held ((tp-lock pool))
    (when (tp-enabled-p pool)
      (error "Pool already active"))
    (setf (tp-blocking-queue pool) (make-blocking-queue)
          (tp-enabled-p pool) t)
    (loop for i from 0 below (tp-pool-size pool) collecting
         (%make-thread-pool-worker pool))))


(declaim (ftype (function (thread-pool (function () *)) *) push-to-pool))
(defun push-to-pool (pool fn)
  (with-lock-held ((tp-lock pool))
    (unless (tp-enabled-p pool)
      (error "Pool inactive"))
    (put-into (tp-blocking-queue pool) fn)))


(declaim (ftype (function (thread-pool) *) close-pool))
(defun close-pool (pool)
  (with-lock-held ((tp-lock pool))
    (unless (tp-enabled-p pool)
      (error "Pool already inactive"))
    (interrupt (tp-blocking-queue pool))
    (setf (tp-enabled-p pool) nil)))


(defmacro within-pool ((pool-place) &body body)
  `(push-to-pool ,pool-place (lambda () ,@body)))
