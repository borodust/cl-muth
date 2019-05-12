(cl:in-package :cl-muth.tests)
(5am:in-suite :cl-muth.tests)


(defun %make-test-array ()
  (let ((array (make-array 100)))
    (loop for i below (length array)
          do (setf (aref array i) i))
    array))


(5am:test pairing-heap
  (let* ((heap (muth::make-pairing-heap))
         (sorted (%make-test-array))
         (shuffled (alexandria:copy-array sorted))
         control
         result)
    (sort sorted #'<)
    (alexandria:shuffle shuffled)

    (loop for item across shuffled
          do (muth::%pairing-heap-push heap item))

    (setf result (loop for item = (muth::%pairing-heap-pop heap)
                       while item
                       collect item)
          control (loop for item across sorted
                        collect item))

    (5am:is (equal result control))))


(5am:test scheduling
  (let* ((scheduler (start-scheduler (make-scheduler)))
         (start (%current-seconds))
         (ticks 0))
    (wait-with-latch (latch)
      (flet ((%tick ()
               (incf ticks)
               (when (= 3 ticks)
                 (unschedule)
                 (open-latch latch))))
        (schedule scheduler #'%tick 0.05 0.1)))
    (stop-scheduler scheduler)
    (5am:is (>= 0.3 (- (%current-seconds) start) 0.25))
    (5am:is (= ticks 3))))
