(in-package :cl-muth)


(define-constant +iterations-until-wait+ 1024)


(deftype blocking-queue-item-priority ()
  '(member :lowest :low :medium :high :highest))


(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock "blocking-queue-lock"))
   (interrupted-p :initform nil)
   (max-size :initform nil :initarg :max-size)
   (state-changed :initform (make-condition-variable :name "blocking-queue-condition"))
   (queue :initform (list (list :highest)
                          (list :high)
                          (list :medium)
                          (list :low)
                          (list :lowest))
          :reader %queue-of)))


(define-condition interrupted (control-error) ())


(defun %next (queue)
  (let ((queue (%queue-of queue)))
    (loop for group in queue
       while (null (cdr group))
       finally (return (pop (cdr group))))))


(defun %length (queue)
  (let ((queue (%queue-of queue)))
    (loop with len = 0
       for group in queue do
         (incf len (length (cdr group)))
       finally (return len))))


(defun %emptyp (queue)
  (let ((queue (%queue-of queue)))
    (loop for group in queue
         for empty-p = (null (cdr group))
         while empty-p
         finally (return empty-p))))


(defun %add (queue item priority)
  (let ((queue (%queue-of queue)))
    (nconcf (cdr (assoc priority queue)) (list item))))


(declaim (inline %wait-interruptibly))
(defun %wait-interruptibly (this)
  (with-slots (lock state-changed interrupted-p) this
    (restart-case (cond
                    (interrupted-p (condition-notify state-changed)
                                   (error (make-condition 'interrupted)))
                    (t (condition-wait state-changed lock)))
      (continue-blocking-operation ()
        (setf interrupted-p nil)))))


(declaim (inline make-blocking-queue)
         (ftype (function (&optional (or null positive-integer)) *) make-blocking-queue))
(defun make-blocking-queue (&optional (max-size nil))
  (make-instance 'blocking-queue :max-size max-size))


(declaim (ftype (function (blocking-queue * &optional blocking-queue-item-priority) *) put-into))
(defun put-into (blocking-queue item &optional (priority :medium))
  (with-slots (lock state-changed max-size) blocking-queue
    (with-recursive-lock-held (lock)
      (unless (null max-size)
        (loop until (< (%length blocking-queue) max-size) do
             (%wait-interruptibly blocking-queue)))
      (%add blocking-queue item priority)
      (condition-notify state-changed)))
  item)


(declaim (ftype (function (blocking-queue) *) pop-from))
(defun pop-from (blocking-queue)
  (with-slots (lock state-changed interrupted-p) blocking-queue
    (loop for i = 0 then (1+ i) do
         (with-recursive-lock-held (lock)
           (unless (%emptyp blocking-queue)
             (condition-notify state-changed)
             (return (%next blocking-queue)))
           (when (> i +iterations-until-wait+)
             (%wait-interruptibly blocking-queue)
             (setf i 0))))))


(declaim (ftype (function (blocking-queue) *) interrupt))
(defun interrupt (blocking-queue)
  (with-slots (lock state-changed interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (setf interrupted-p t)
      (condition-notify state-changed))))
