(cl:in-package :cl-muth)

;;;
;;;
;;;
(defstruct blocking-timed-queue
  (condivar (bt:make-condition-variable :name "blocking-timed-queue"))
  (lock (bt:make-recursive-lock "blocking-timed-queue"))
  (heap (bodge-heap:make-binary-heap :key #'car)))


(defun blocking-timed-queue-interrupt (queue)
  (bt:condition-notify (blocking-timed-queue-condivar queue)))


(defun blocking-timed-queue-push (queue value wait-sec)
  (let ((heap (blocking-timed-queue-heap queue))
        (lock (blocking-timed-queue-lock queue))
        (queue-item (cons (float (+ (current-seconds) wait-sec) 0d0) value)))
    (bt:with-recursive-lock-held (lock)
      (bodge-heap:binary-heap-push heap queue-item))
    (blocking-timed-queue-interrupt queue))
  value)


(defun %blocking-timed-queue-peek-time (queue)
  (car (bodge-heap:binary-heap-peek (blocking-timed-queue-heap queue))))


(defun %blocking-timed-queue-pop (queue)
  (let ((lock (blocking-timed-queue-lock queue))
        (condivar (blocking-timed-queue-condivar queue))
        (heap (blocking-timed-queue-heap queue)))
    (bt:with-recursive-lock-held (lock)
      (let ((timeout (when-let ((next-time (%blocking-timed-queue-peek-time queue)))
                       (- next-time (current-seconds)))))
        (when (or (null timeout) (> timeout 0))
          (bt:condition-wait condivar lock :timeout timeout)))
      (let* ((current-time (current-seconds))
             (expected-time (%blocking-timed-queue-peek-time queue)))
        (when (and expected-time (<= expected-time current-time))
          (bodge-heap:binary-heap-pop heap))))))


(defun blocking-timed-queue-pop (queue)
  (loop for item = (%blocking-timed-queue-pop queue)
        until item
        finally (return (cdr item))))
