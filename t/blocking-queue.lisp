(cl:in-package :cl-muth.tests)
(5am:in-suite :cl-muth.tests)

(5am:test fifo
  (let ((fifo (muth::make-fifo)))
    (loop for i below 10
          do (muth::fifo-push fifo i))
    (multiple-value-bind (result control)
        (loop for i below 11
              collect (muth::fifo-pop fifo) into result
              collect (unless (= i 10) i) into control
              finally (return (values result control)))
      (5am:is (equal result control)))))


(defun fill-priority-list (priority-list)
  (loop for (value priority) in '((0 :low)
                                  (1 :high)
                                  (2 :low)
                                  (3 :medium)
                                  (4 :lowest)
                                  (5 :highest))
        do (muth::priority-list-push priority-list value priority))
  priority-list)


(5am:test priority-list
  (let ((priority-list (fill-priority-list (muth::make-priority-list))))
    (5am:is (not (muth::priority-list-empty-p priority-list)))
    (5am:is (= (muth::priority-list-length priority-list) 6))
    (5am:is (= (muth::priority-list-peek-least-priority priority-list)
               (muth::priority->index :lowest)))
    (5am:is (equal (loop for item = (muth::priority-list-pop priority-list)
                         while item
                         collect item)
                   '(5 1 3 0 2 4)))
    (5am:is (muth::priority-list-empty-p priority-list))))


(5am:test unbounded-blocking-queue
  (let ((queue (muth:make-blocking-queue))
        result)
    (muth:wait-with-latch (latch)
      (flet ((%fill-queue ()
               (loop for (value priority) in '((0 :low)
                                               (1 :high)
                                               (2 :low)
                                               (3 :medium)
                                               (4 :lowest)
                                               (5 :highest))
                     do (muth:put-into queue value priority)))
             (%consume-queue ()
               (sleep 0.1)
               (setf result (loop for i upto 5
                                  collect (muth:pop-from queue)))
               (muth:open-latch latch)))
        (bt:make-thread #'%fill-queue)
        (bt:make-thread #'%consume-queue)))
    (5am:is (equal result '(5 1 3 0 2 4)))))


(5am:test bounded-blocking-queue
  (let ((queue (muth:make-blocking-queue 1))
        (was-blocked-p nil)
        result)
    (muth:wait-with-latch (latch)
      (flet ((%fill-queue ()
               (sleep 0.01)
               (loop for (value priority) in '((0 :low)
                                               (1 :high)
                                               (2 :low)
                                               (3 :medium)
                                               (4 :lowest)
                                               (5 :highest))
                     do (unless (muth:try-put-into queue value priority)
                          (setf was-blocked-p t)
                          (muth:put-into queue value priority))))
             (%consume-queue ()
               (setf result (loop for i upto 5
                                  collect (muth:pop-from queue)))
               (muth:open-latch latch)))
        (bt:make-thread #'%fill-queue)
        (bt:make-thread #'%consume-queue)))
    (5am:is (eq was-blocked-p t))
    (5am:is (equal result '(0 1 2 3 4 5)))))


(5am:test blocking-queue-replacing
  (let ((queue (muth:make-blocking-queue 4))
        result)
    (muth:wait-with-latch (latch)
      (flet ((%fill-queue ()
               (loop for (value priority) in '((0 :low)
                                               (1 :medium)
                                               (2 :low)
                                               (3 :low))
                     do (muth:put-into queue value priority))
               (muth:try-put-replacing queue 4 :highest)
               (muth:try-put-replacing queue 5 :low))
             (%consume-queue ()
               (sleep 0.1)
               (setf result (loop for i below 4
                                  collect (muth:pop-from queue)))
               (muth:open-latch latch)))
        (bt:make-thread #'%fill-queue)
        (bt:make-thread #'%consume-queue)))
    (5am:is (equal result '(4 1 3 5)))))
