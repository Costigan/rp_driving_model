;;;
;;; Simple queue that allows removal of events
;;;
;;; Operations
;;; ENQUEUE
;;; DEQUEUE
;;; EMPTY-QUEUE?
;;; PEEK-QUEUE
;;; REMOVE-FROM-QUEUE
;;; MATCHING-EVENTS

(defclass QUEUE ()
  ((events :accessor queue-events :initform '())
   (time-accessor :accessor queue-time-accessor :initarg :time-accessor)))

(defun MAKE-EMPTY-QUEUE (accessor)
  (make-instance 'queue :time-accessor accessor))

(defmethod ENQUEUE ((q QUEUE) event)
  (let* ((a (queue-time-accessor q))
	 (atime (funcall a event))
	 (evts (queue-events q)))
    (cond ((or (null evts)
	       (< (funcall a event) (funcall a (first evts))))
	   (push event (queue-events q)))
	  (t
	   (loop for last = (queue-events q) then cons
	      for cons = (cdr (queue-events q)) then (cdr cons)
	      do (progn
		   ;(format t "last=~a cons=~a~%" last cons)
		   (when (or (null cons)
			     (< atime (funcall a (car cons))))
		     (setf (cdr last) (cons event cons))
		     (return)))))))
  nil)

(defmethod DEQUEUE ((q QUEUE))
  (pop (queue-events q)))

(defmethod PEEK-QUEUE ((q QUEUE))
  (first (queue-events q)))

(defmethod EMPTY-QUEUE? ((q QUEUE))
  (null (queue-events q)))

(defmethod MATCHING-EVENTS ((q QUEUE) pred)
  (loop for e in (queue-events q)
       when (funcall pred e)
       collect e))

(defmethod REMOVE-FROM-QUEUE ((q QUEUE) pred)
  (setf (queue-events q)
	(delete-if pred (queue-events q))))
