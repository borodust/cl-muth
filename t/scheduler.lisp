(cl:in-package :cl-muth.tests)
(5am:in-suite :cl-muth.tests)


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
