# cl-muth

Various multithreading and concurrency utilities.

### thread-pool

Simple and conventional thread pool.

### guarded-reference

A value holder with access to it guarded by a lock.

### latch
A synchronization primitive with implicit counter for blocking further execution
of threads waiting the latch to be open - when its counter reaches zero.

### blocking-queue

A synchronization primitive for thread blocking reads and writes from/into a
queue.

### scheduler

Simple mechanism for scheduling tasks for execution at specified time or
intervals.

Example:
```common_lisp
(ql:quickload '(:cl-muth :log4cl))

(defparameter *scheduler* (muth:make-scheduler))
(defparameter *unscheduled-p* nil)

(muth:start-scheduler *scheduler*)

;; Run %report function every second starting after 0.5 seconds until
;; *unscheduled-p* is set to 't
(labels ((%time ()
           (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))
         (%report ()
           (if *unscheduled-p*
               (muth:unschedule)
               (log:info "Hello there: ~A" (%time)))))
  (muth:schedule *scheduler* #'%report 0.5 1))

;; Evaluate to stop %report from being called every second
; (setf *unscheduled-p* t)
```

### atomic-counter
_Efficient only on `SBCL` and `CCL` implementations. Fallbacks to blocking
operations on others._

Thread safe counter with atomic increment and decrement operations.


### spin-lock
_Efficient only on `SBCL` and `CCL` implementations. Fallbacks to blocking
operations on others._

Simple non-blocking spin lock implementation.


## Tests

```common_lisp
(ql:quickload :cl-muth/tests) (5am:run! :cl-muth.tests)
```
