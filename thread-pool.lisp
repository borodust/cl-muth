(in-package :cl-muth)

(defstruct (thread-pool
             (:conc-name tp-)
             (:constructor make-thread-pool (pool-size)))
  (pool-size 1 :read-only t)
  (active-p nil)
  (lock (make-lock "thread-pool-lock") :read-only t)
  (blocking-queue (make-blocking-queue) :read-only t))


(defun %make-thread-pool-worker (pool)
  (make-thread
   (lambda ()
     (loop for item = (pop-from (tp-blocking-queue pool)) until (null item) do
          (funcall item)))
   :name "thread-pool-worker"))


(declaim (ftype (function (thread-pool) *) open-pool))
(defun open-pool (pool)
  (with-lock-held ((tp-lock pool))
    (when (tp-active-p pool)
      (error "Pool already active"))
    (loop for i from 0 below (tp-pool-size pool) collecting
         (%make-thread-pool-worker pool))
    (setf (tp-active-p pool) t)))


(declaim (ftype (function (thread-pool (function () *)) *) push-to-pool))
(defun push-to-pool (pool fn)
  (with-lock-held ((tp-lock pool))
    (unless (tp-active-p pool)
      (error "Pool inactive"))
    (put-into (tp-blocking-queue pool) fn)))


(declaim (ftype (function (thread-pool) *) close-pool))
(defun close-pool (pool)
  (with-lock-held ((tp-lock pool))
    (unless (tp-active-p pool)
      (error "Pool already inactive"))
    (loop for i from 0 below (tp-pool-size pool) do
         (put-into (tp-blocking-queue pool) nil))
    (setf (tp-active-p pool) nil)))


(defmacro within-pool ((pool-place) &body body)
  `(push-to-pool ,pool-place (lambda () ,@body)))
