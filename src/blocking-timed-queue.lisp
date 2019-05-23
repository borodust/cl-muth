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
;;;
;;;
(defstruct blocking-timed-queue
  (condivar (bt:make-condition-variable :name "blocking-timed-queue"))
  (lock (bt:make-recursive-lock "blocking-timed-queue"))
  (heap (make-pairing-heap :key #'car)))


(defun blocking-timed-queue-push (queue value wait-sec)
  (let ((heap (blocking-timed-queue-heap queue))
        (lock (blocking-timed-queue-lock queue))
        (queue-item (cons (float (+ (current-seconds) wait-sec) 0d0) value)))
    (bt:with-recursive-lock-held (lock)
      (%pairing-heap-push heap queue-item)))
  value)


(defun %blocking-timed-queue-peek-time (queue)
  (car (%pairing-heap-peek (blocking-timed-queue-heap queue))))


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
          (%pairing-heap-pop heap))))))


(defun blocking-timed-queue-pop (queue)
  (loop for item = (%blocking-timed-queue-pop queue)
        until item
        finally (return (cdr item))))
