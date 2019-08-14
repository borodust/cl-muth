(cl:in-package :cl-muth)


(define-constant +iterations-until-wait+ 1024)
(define-constant +minimal-queue-extension+ 10)


(deftype blocking-queue-item-priority ()
  '(member :lowest :low :medium :high :highest))

;;;
;;; FIFO
;;;
(defstruct (fifo
            (:constructor make-fifo ()))
  (queue (make-array +minimal-queue-extension+ :initial-element nil))
  (head-idx -1 :type fixnum)
  (tail-idx -1 :type fixnum))


(defun prev-idx (fifo idx)
  (if (> idx 0)
      (1- idx)
      (length (fifo-queue fifo))))


(defun next-idx (fifo idx)
  (if (< idx (1- (length (fifo-queue fifo))))
      (1+ idx)
      0))


(defun %fifo-extend (fifo)
  (let* ((old-queue (fifo-queue fifo))
         (new-queue (make-array (+ (length old-queue) +minimal-queue-extension+)
                                :initial-element nil))
         (head-idx (fifo-head-idx fifo))
         (tail-idx (fifo-tail-idx fifo)))
    (if (= head-idx 0)
        (setf (subseq new-queue 0) old-queue)
        (let* ((subhead-len (- (length old-queue) head-idx))
               (head-view (make-array subhead-len :displaced-to old-queue
                                                  :displaced-index-offset head-idx))
               (tail-view (make-array (1+ tail-idx) :displaced-to old-queue)))
          (setf (subseq new-queue 0) head-view
                (subseq new-queue subhead-len) tail-view)))
    (setf (fifo-queue fifo) new-queue
          (fifo-head-idx fifo) 0
          (fifo-tail-idx fifo) (1- (length old-queue)))))


(defun fifo-push (fifo item)
  (let* ((head-idx (fifo-head-idx fifo))
         (tail-idx (fifo-tail-idx fifo)))
    (cond
      ((= tail-idx -1) (setf (fifo-head-idx fifo) 0))
      ((= (next-idx fifo tail-idx) head-idx) (%fifo-extend fifo)))
    (setf (fifo-tail-idx fifo) (next-idx fifo (fifo-tail-idx fifo))
          (aref (fifo-queue fifo) (fifo-tail-idx fifo)) item)))


(defun fifo-pop (fifo)
  (let ((head-idx (fifo-head-idx fifo))
        (tail-idx (fifo-tail-idx fifo)))
    (unless (= -1 tail-idx)
      (prog1 (aref (fifo-queue fifo) head-idx)
        (let ((new-head-idx (next-idx fifo head-idx)))
          (if (> new-head-idx tail-idx)
              (setf (fifo-head-idx fifo) -1
                    (fifo-tail-idx fifo) -1)
              (setf (fifo-head-idx fifo) (next-idx fifo head-idx))))))))


(defun fifo-peek (fifo)
  (let ((head-idx (fifo-head-idx fifo)))
    (unless (= -1 head-idx)
      (aref (fifo-queue fifo) head-idx))))


;;;
;;; Priority List
;;;
(declaim (inline priority->index))
(defun priority->index (priority)
  (ecase priority
    (:medium 2)
    (:highest 0)
    (:lowest 4)
    (:low 3)
    (:high 1)))


(defstruct (priority-list
            (:constructor %make-priority-list (buckets))
            (:conc-name pl-))
  (buckets nil :read-only t)
  (length 0))


(defun make-priority-list ()
  (let* ((bucket-count (1+ (priority->index :lowest)))
         (buckets (make-array bucket-count)))
    (loop for i below bucket-count
          do (setf (aref buckets i) (make-fifo)))
    (%make-priority-list buckets)))


(defun priority-list-length (queue)
  (pl-length queue))


(defun priority-list-empty-p (queue)
  (= (pl-length queue) 0))


(defun priority-list-push (queue item priority)
  (let ((fifo (aref (pl-buckets queue) (priority->index priority))))
    (prog1 (fifo-push fifo item)
      (incf (pl-length queue)))))


(defun priority-list-pop (queue)
  (loop with priority-buckets = (pl-buckets queue)
        for fifo across priority-buckets
        for item = (fifo-pop fifo)
        until item
        finally (when item
                  (decf (pl-length queue))
                  (return item))))


(defun priority-list-pop-least-important (queue)
  (loop with priority-buckets = (pl-buckets queue)
        for i from (1- (length priority-buckets)) downto 0
        for fifo = (aref priority-buckets i)
        for item = (fifo-pop fifo)
        until item
        finally (when item
                  (decf (pl-length queue))
                  (return item))))


(defun priority-list-peek-least-priority (queue)
  (loop with priority-buckets = (pl-buckets queue)
        for i from (1- (length priority-buckets)) downto 0
        for fifo = (aref priority-buckets i)
        for item = (fifo-peek fifo)
        until item
        finally (when item (return i))))

;;;
;;; Blocking queue
;;;
(defclass blocking-queue ()
  ((lock :initform (make-recursive-lock "blocking-queue-lock"))
   (interrupted-p :initform nil)
   (max-size :initform nil :initarg :max-size)
   (state-changed :initform (make-condition-variable :name "blocking-queue-condition"))
   (queue :initform (make-priority-list) :reader %queue-of)))


(define-condition interrupted (control-error) ())


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


(defun %pq-length (blocking-queue)
  (with-slots (queue) blocking-queue
    (priority-list-length queue)))


(defun %pq-empty-p (blocking-queue)
  (with-slots (queue) blocking-queue
    (priority-list-empty-p queue)))


(defun %pq-push (blocking-queue item priority)
  (with-slots (state-changed queue) blocking-queue
    (prog1 (priority-list-push queue item priority)
      (condition-notify state-changed))))


(defun %pq-pop (blocking-queue)
  (with-slots (state-changed queue) blocking-queue
    (prog1 (priority-list-pop queue)
      (condition-notify state-changed))))


(defun %pq-less-or-same-priority-present (blocking-queue priority)
  (with-slots (queue) blocking-queue
    (>= (priority-list-peek-least-priority queue) (priority->index priority))))


(defun %pq-pop-least-important (blocking-queue)
  (with-slots (queue) blocking-queue
    (priority-list-pop-least-important queue)))


(declaim (ftype (function (blocking-queue * &optional blocking-queue-item-priority) *) put-into))
(defun put-into (blocking-queue item &optional (priority :medium))
  (with-slots (lock max-size) blocking-queue
    (with-recursive-lock-held (lock)
      (unless (null max-size)
        (loop until (< (%pq-length blocking-queue) max-size) do
          (%wait-interruptibly blocking-queue)))
      (%pq-push blocking-queue item priority)))
  item)


(defun try-put-into (blocking-queue item &optional (priority :medium))
  (with-slots (lock max-size) blocking-queue
    (with-recursive-lock-held (lock)
      (when (or (null max-size) (< (%pq-length blocking-queue) max-size))
        (%pq-push blocking-queue item priority)
        t))))


(defun try-put-replacing (blocking-queue item &optional (priority :medium))
  (with-slots (lock max-size) blocking-queue
    (with-recursive-lock-held (lock)
      (cond
        ((or (null max-size) (< (%pq-length blocking-queue) max-size))
         (prog1 t
           (%pq-push blocking-queue item priority)))
        ((%pq-less-or-same-priority-present  blocking-queue priority)
         (prog1 t
           (%pq-pop-least-important blocking-queue)
           (%pq-push blocking-queue item priority)))
        (t nil)))))


(declaim (ftype (function (blocking-queue) *) pop-from))
(defun pop-from (blocking-queue)
  (with-slots (lock state-changed interrupted-p) blocking-queue
    (loop for i = 0 then (1+ i)
          do (with-recursive-lock-held (lock)
               (unless (%pq-empty-p blocking-queue)
                 (return (%pq-pop blocking-queue)))
               (when (> i +iterations-until-wait+)
                 (%wait-interruptibly blocking-queue)
                 (setf i 0))))))


(declaim (ftype (function (blocking-queue) *) interrupt))
(defun interrupt (blocking-queue)
  (with-slots (lock state-changed interrupted-p) blocking-queue
    (with-recursive-lock-held (lock)
      (setf interrupted-p t)
      (condition-notify state-changed))))
