(cl:in-package :cl-muth)


(declaim (special *unscheduled-p*))


(define-constant +double-float-drift-time-correction+ 0.0000001d0
  :test #'=)

;;;
;;; Scheduler
;;;

(defclass scheduler ()
  ((enabled-p :initform nil)
   (queue :initform (make-blocking-timed-queue))
   (lock :initform (bt:make-recursive-lock "scheduler-lock"))))


(defun schedule (scheduler task wait-sec &optional interval-sec)
  (with-slots (queue) scheduler
    (let* ((expected-time (+ (current-seconds) wait-sec))
           (item (cons task (cons expected-time interval-sec))))
      (blocking-timed-queue-push queue item wait-sec))))


(defun unschedule ()
  (setf *unscheduled-p* t))


(defun %reschedule-task (scheduler task interval expected-time current-time)
  (let* ((adjusted-interval (- interval
                               (- current-time expected-time
                                  +double-float-drift-time-correction+)))
         (rescheduled-wait (if (< adjusted-interval 0)
                               (mod adjusted-interval interval)
                               adjusted-interval)))
    (schedule scheduler task rescheduled-wait interval)))


(defun process-next-scheduler-event (scheduler)
  (with-slots (queue) scheduler
    (let ((item (blocking-timed-queue-pop queue)))
      (destructuring-bind (task expected-time . interval) item
        (let ((*unscheduled-p* nil)
              (current-time (current-seconds)))
          (funcall task)
          (when (and interval (not *unscheduled-p*))
            (%reschedule-task scheduler task interval expected-time current-time)))))))


(defun make-scheduler ()
  (make-instance 'scheduler))


(defun start-scheduler (scheduler &key (background t))
  (with-slots (lock enabled-p) scheduler
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (error "Scheduler already started"))
      (setf enabled-p t)
      (flet ((run ()
               (loop while enabled-p
                     do (process-next-scheduler-event scheduler))))
        (if background
            (bt:make-thread #'run)
            (run))))
    scheduler))


(defun stop-scheduler (scheduler)
  (with-slots (lock enabled-p queue) scheduler
    (bt:with-recursive-lock-held (lock)
      (unless enabled-p
        (error "Scheduler already stopped"))
      (setf enabled-p nil)
      (blocking-timed-queue-interrupt queue))
    scheduler))
