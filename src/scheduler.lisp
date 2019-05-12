(cl:in-package :cl-muth)


(declaim (special *unscheduled-p*))


(define-constant +double-float-drift-time-correction+ 0.0000001d0
  :test #'=)

;;;
;;; Pairing Heap
;;;
;;; JOHN T. STASKO and JEFFREY SCOTT VlllER
;;; Pairing Heaps: Experiments and Analysis
;;;
;;; https://pdfs.semanticscholar.org/b6f7/c16f9877b2a27396217296be200e46fb42ea.pdf
;;;
(defstruct (pairing-heap-node
            (:constructor make-pairing-heap-node (value))
            (:conc-name phn-))
  child
  left-sibling
  right-sibling
  value)


(defstruct (pairing-heap
            (:constructor %make-pairing-heap (key-extractor)))
  root
  key-extractor)


(defun make-pairing-heap (&key (key #'identity))
  (%make-pairing-heap key))


(defun %leftmost-sibling-p (node)
  (let ((left-sibling (phn-left-sibling node)))
    (or (null left-sibling) (eq node (phn-child left-sibling)))))


(defun %phn-comparison-link (this-node that-node key-extractor)
  (labels ((%phn-key (node)
             (funcall key-extractor (phn-value node)))
           (%extract-subtree (root)
             (when-let ((left-sibling (phn-left-sibling root)))
               ;; check if leftmost child
               (if (eq root (phn-child left-sibling))
                   (setf (phn-child left-sibling) (phn-right-sibling root))
                   (setf (phn-right-sibling left-sibling) (phn-right-sibling root))))
             (when-let ((right-sibling (phn-right-sibling root)))
               (setf (phn-left-sibling right-sibling) (phn-left-sibling root)))
             root)
           (%add-child (parent child)
             (setf (phn-right-sibling child) (phn-child parent)
                   (phn-left-sibling child) parent
                   (phn-child parent) child)
             (when-let ((right-child-sibling (phn-right-sibling child)))
               (setf (phn-left-sibling right-child-sibling) child))
             parent))
    (if (or (null this-node) (null that-node))
        (or this-node that-node)
        (if (> (%phn-key this-node) (%phn-key that-node))
            (%add-child that-node (%extract-subtree this-node))
            (%add-child this-node (%extract-subtree that-node))))))


(defun %pairing-heap-push (heap value)
  (let* ((new-node (make-pairing-heap-node value))
         (root (pairing-heap-root heap))
         (key-extractor (pairing-heap-key-extractor heap)))
    (setf (pairing-heap-root heap) (if root
                                       (%phn-comparison-link new-node
                                                             root
                                                             key-extractor)
                                       new-node)))
  heap)


(defun %pairing-heap-peek (pairing-heap)
  (when-let ((root (pairing-heap-root pairing-heap)))
    (phn-value root)))


(defun %pairing-heap-pop (heap)
  (when-let ((root (pairing-heap-root heap)))
    (let ((key-extractor (pairing-heap-key-extractor heap)))
      (flet ((first-pass (first-node)
               (loop with current-node = first-node
                     and prev-node = nil
                     while current-node
                     do (let* ((next-node (phn-right-sibling current-node))
                               (linked-node (%phn-comparison-link current-node
                                                                  next-node
                                                                  key-extractor)))
                          (setf prev-node linked-node
                                current-node (phn-right-sibling linked-node)))
                     finally (return prev-node)))
             (second-pass (last-node)
               (when last-node
                 (loop with current-node = last-node
                       for prev-node = (phn-left-sibling current-node)
                       until (eq (phn-child prev-node) current-node)
                       do (setf current-node (%phn-comparison-link prev-node
                                                                   current-node
                                                                   key-extractor))
                       finally (return current-node)))))
        (let ((new-root (when-let ((root-child (phn-child root)))
                          (let ((linked-node (second-pass (first-pass root-child))))
                            (setf (phn-left-sibling linked-node) nil)
                            linked-node))))
          (setf (pairing-heap-root heap) new-root))))
    (phn-value root)))

;;;
;;; Scheduler priority queue
;;;

(defun make-scheduler-queue ()
  (make-pairing-heap :key #'car))


(defun scheduler-queue-push (queue time task interval)
  (%pairing-heap-push queue (cons (float time 0d0) (cons task (float interval 0d0)))))


(defun scheduler-queue-peek-time (queue)
  (car (%pairing-heap-peek queue)))


(defun scheduler-queue-pop (queue)
  (when-let ((value (cdr (%pairing-heap-pop queue))))
    (destructuring-bind (task . interval) value
      (values task interval))))


;;;
;;; Scheduler
;;;

(defclass scheduler ()
  ((enabled-p :initform nil)
   (condivar :initform (bt:make-condition-variable :name "scheduler-condivar"))
   (lock :initform (bt:make-recursive-lock "scheduler-lock"))
   (queue :initform (make-scheduler-queue))))


(defun %schedule (scheduler task wait-sec interval-sec)
  (with-slots (queue) scheduler
    (scheduler-queue-push queue (+ (current-seconds) wait-sec) task interval-sec)))


(defun schedule (scheduler task wait-sec &optional interval-sec)
  (with-slots (queue condivar lock) scheduler
    (bt:with-recursive-lock-held (lock)
      (%schedule scheduler task wait-sec interval-sec))
    (bt:condition-notify condivar)))


(defun unschedule ()
  (setf *unscheduled-p* t))


(defun %reschedule-task (scheduler task interval expected-time current-time)
  (let* ((adjusted-interval (- interval
                               (- current-time expected-time
                                  +double-float-drift-time-correction+)))
         (rescheduled-wait (if (< adjusted-interval 0)
                               (mod adjusted-interval interval)
                               adjusted-interval)))
    (%schedule scheduler task rescheduled-wait interval)))


(defun process-next-scheduler-event (scheduler)
  (with-slots (queue condivar lock) scheduler
    (bt:with-recursive-lock-held (lock)
      (let* ((current-time (current-seconds))
             (expected-time (scheduler-queue-peek-time queue)))
        (when (and expected-time (<= expected-time current-time))
          (multiple-value-bind (task interval) (scheduler-queue-pop queue)
            (let ((*unscheduled-p* nil))
              (funcall task)
              (when (and interval (not *unscheduled-p*))
                (%reschedule-task scheduler task interval expected-time current-time)))))
        (let ((timeout (when-let ((next-time (scheduler-queue-peek-time queue)))
                         (- next-time (current-seconds)))))
          (when (or (null timeout) (> timeout 0))
            (bt:condition-wait condivar lock :timeout timeout)))))))


(defun make-scheduler ()
  (make-instance 'scheduler))


(defun start-scheduler (scheduler &key (background t))
  (with-slots (lock enabled-p condivar queue) scheduler
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
  (with-slots (lock enabled-p condivar) scheduler
    (bt:with-recursive-lock-held (lock)
      (unless enabled-p
        (error "Scheduler already stopped"))
      (setf enabled-p nil)
      (bt:condition-notify condivar))
    scheduler))
