;;;
;;; Discrete Event Simulator
;;;

;;; This breaks the file layering
(defvar *rover*)
(defvar *driver*)
(defvar *time*)

;;;
;;; Debugging and Tracing
;;;

(defvar *TRACE-LEVEL* 0)

(defmacro TRACE! (level fmt &rest args)
  `(if (>= *trace-level* ,level)
       (progn
	 (format t "(~8,3f ~8,3f ~a ~a " *time* (rover-position *rover*) (state *rover*) (state *driver*))
	 (format t ,fmt ,@args)
	 (format t ")~%"))))

(defmacro TRACE-WHEN (level &rest statements)
  `(if (>= *trace-level* ,level)
       (progn ,@statements)))

;;;
;;;
;;;

(defvar *EVENTS*)			;needed later

;;;
;;; Variables
;;;

(defvar *VARIABLES* '())

(defmacro DEFINE-INPUT (name value)
  `(eval-when (compile load eval)
     (defvar ,name)
     (setf ,name ,value)
     (setf (get ',name :variable-type) :input)
     (setf (get ',name :default) ,value)	;note second evaluation
     (pushnew ',name *variables*)))

(defmacro DEFINE-INTERMEDIATE (name value)
  `(eval-when (compile load eval)
     (defvar ,name)
     (setf ,name ,value)
     (setf (get ',name :variable-type) :intermediate)
     (setf (get ',name :default) ,value)	;note second evaluation
     (pushnew ',name *variables*)))

(defmacro DEFINE-OUTPUT (name value)
  `(eval-when (compile load eval)
     (defvar ,name ,value)
     (setf (get ',name :variable-type) :output)
     (pushnew ',name *variables*)))

(defmacro DEFINE-INTERNAL (name value)
  `(eval-when (compile load eval)
     (defvar ,name)
     (setf ,name ,value)
     (setf (get ',name :variable-type) :internal)
     (setf (get ',name :default) ,value)	;note second evaluation
     (pushnew ',name *variables*)))

(defun VARIABLES-OF-TYPE (type)
  (loop for v in *variables* when (or (eql type :all) (eql type (get v :variable-type))) collect v))

(defun SORTED-VARIABLES-OF-TYPE (type)
  (sort (variables-of-type type) #'(lambda (a b) (string-lessp (string a) (string b)))))

;;;
;;; Time
;;;

(defvar *TIME* 0)
(defvar *START-TIME* 0)
(defvar *MAXIMUM-TIME* (* 3600 24 30))	;30 days


;;;
;;; Actors
;;;
;;; Actors receive timed messages and have a state.  They receive
;;; calls to TICK every 0.1 sec, and they receive messages at the time
;;; the sender wished.

(defclass ACTOR ()
  ((state :accessor state :initarg :state :initform nil)
   (last-tick :accessor last-tick :initform 0)))

;;; Ticks are used to aggregate the effects of being in a state over
;;; time.  They're sent right before any message is delivered to to an
;;; actor so it can add up the cumulative effect of being in some
;;; state since the last tick, and then change state if needed.
;;; Actors can send themselves a WAKEUP message which normally does
;;; nothing in itself but will cause the tick method to be called.

(defmethod TICK ((a ACTOR) state) (declare (ignore state)))

(defmethod PRINT-OBJECT ((a ACTOR) stream)
  (format stream "{~a state=~a}" (class-name (class-of a)) (state a)))

(defmethod SET-STATE ((a ACTOR) state)
  (trace! 2 ":set-state :actor ~a :state ~a" (type-of a) state)
  (setf (state a) state))


;;;
;;; EVENT: base class for everything time-related
;;;
;;; Messages are currently the only child class of EVENT, and there
;;; will be many kinds of messages.  One distinguished subclass is
;;; ACTIONs, which cause a closure to run at some time.  The behavior
;;; associated with messages is specified by RECEIVE methods on the
;;; various kinds of actors.
;;;

(defclass EVENT ()
  ((time :accessor receive-time :initarg :time)))

(defmethod DISPATCH ((e EVENT)))

(defclass MESSAGE (event)
  ((from :accessor message-from :initarg :from)
   (to :accessor message-to :initarg :to)))

(defmethod PRINT-OBJECT ((m MESSAGE) stream)
  (format stream "{~a :from ~a :to ~a :delivery ~8,3f}" (type-of m) (type-of (message-from m)) (type-of (message-to m)) (receive-time m)))

(defmethod DISPATCH ((msg MESSAGE))
  (let ((receiver (message-to msg)))
    (setq *time* (receive-time msg))
    (tick receiver (state receiver))
    (setf (last-tick receiver) *time*)
    (receive receiver msg)))

(defclass WAKEUP (MESSAGE) ())

;;; Send takes a from actor, a to actor, a partially constructed
;;; message, and a time specification.  It finishes setting instance
;;; variables in the message and then queues it for delivery.

(defmethod SEND ((from ACTOR) (to ACTOR) (m MESSAGE) &key time delay)
  (let ((receive-time (cond (time time)
			    (delay (+ *time* delay))
			    (t *time*))))
    (setf (message-from m) from)
    (setf (message-to m) to)
    (setf (receive-time m) receive-time)
    (trace! 2 ":send :from ~a :to ~a :msg ~a :delivery ~a" (type-of from) (type-of to) (type-of m) receive-time)
    (enqueue *events* m)))

(defmethod RECEIVE ((a ACTOR) (msg MESSAGE))) ;do nothing by default

(defmethod SEND-WAKEUP ((a ACTOR) &key time delay)
  (let ((receive-time (cond (time time)
			    (delay (+ *time* delay))
			    (t *time*))))
    (send a a (make-instance 'wakeup) :time receive-time)))

(defclass ACTION (event)
  ((closure :accessor action-closure :initarg :closure)
   (actor :accessor action-actor :initarg :actor)))

(defmethod PRINT-OBJECT ((a ACTION) stream)
  (format stream "{~a :at ~8,3f}" (type-of a) (receive-time a)))

(defmethod DISPATCH ((a ACTION))
  (setq *time* (receive-time a))
  (funcall (action-closure a)))

(defmethod RECEIVE ((a ACTOR) (msg ACTION))
  (funcall (action-closure msg)))

(defmacro DO-AT-TIME (actor time &body statements)
  `(enqueue *events* (make-instance 'action :actor ,actor :time ,time :closure #'(lambda () ,@statements))))

(defmacro DO-LATER (actor delta &body statements)
  `(do-at-time ,actor (+ *time* ,delta) ,@statements))


;;;
;;; The Event Queue
;;;

(setq *EVENTS* (make-empty-queue #'receive-time)) ;needs to be after receive-time is defined

(defun DESCRIBE-EVENT-QUEUE ()
  (format t "Event queue ---~%")
  (dolist (x (queue-events *events*))
    (describe x))
  (format t "----~%"))

;;; Event queue filtering

(defmethod ABANDON-PLANS ((q QUEUE) agent)
  (setf (queue-events q)
	(delete-if #'(lambda (e)
		       (cond ((typep e 'message)
			      (and (eql (message-to e) agent)
				   (eql (message-from e) agent)))
			     ((typep e 'action)
			      (eql (action-actor e) agent))
			     (t
			      nil)))
		   (queue-events q))))

;;;
;;; The simulation top level
;;;

(defun RUN-SIMULATION (&key terminate interruption tick)
  (loop until (or (empty-queue? *events*)
		  (if (null terminate) 
		      (> *time* *maximum-time*)
		      (funcall terminate)))
       for msg = (dequeue *events*)
       do (progn
	    (if (typep msg 'message)
		(trace! 4 ":dispatch ~a to ~a" msg (type-of (message-to msg)))
		(trace! 4 ":dispatch ~a" msg))
	    (when tick (funcall tick))
	    (when interruption (funcall interruption))
	    (dispatch msg)))
  (when (empty-queue? *events*)
    (format t "Stopping due to empty queue"))
  )

;;;
;;; Utility Functions
;;;

(defun PRINT-VARIABLE-VALUE-TABLE (variables)
  (printing-table
   (printing-table-header 
    ()
    (printing-table-cell (princ "Variable"))
    (printing-table-cell (princ "Value")))
   (dolist (var variables)
     (printing-table-row 
      ()
      (printing-table-cell (princ var))
      (printing-table-cell (princ (symbol-value var)))))))

(defclass MONTE-CARLO ()
  ((model :accessor monte-carlo-model :initarg :model)
   (inputs :accessor monte-carlo-inputs :initarg :inputs)
   (outputs :accessor monte-carlo-outputs :initarg :outputs)
   (variables :accessor monte-carlo-variables :initarg :variables)
   (enum-specs :accessor monte-carlo-enum-specs :initarg :enum-specs)
   (step-specs :accessor monte-carlo-step-specs :initarg :step-specs)
   (random-specs :accessor monte-carlo-random-specs :initarg :random-specs)
   (trials :accessor monte-carlo-trials :initarg :trials)
   (results :accessor monte-carlo-results :initarg :results)))

(defmethod PRINT-OBJECT ((m monte-carlo) stream)
  (format stream "{MONTE-CARLO result-count=~d}" (length (monte-carlo-results m))))


