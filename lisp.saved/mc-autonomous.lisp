;;;
;;; Support for exploring a trade space and monte-carlo runs
;;;

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

;; sample specs are a list of sexprs that look like this
;; (enum <variable> <value1> <value2> <value3> ...)
;; (steps <variable> <low> <high> <step>)
;; (random <variable> <low> <high>)

(defun RUN-MONTE-CARLO (model sample-specs trials)
  (setf *random-state* (make-random-state t))
  (let* ((vars (sorted-variables-of-type :all))
	 (enums (reverse (loop for spec in sample-specs when (eql (first spec) 'enum) collect spec)))
	 (randoms (reverse (loop for spec in sample-specs when (eql (first spec) 'random) collect spec)))
	 (steps (reverse (loop for spec in sample-specs when (eql (first spec) 'step) collect spec)))
	 (results '())
	 (count 0))
    (labels ((top-level (model)
	       (apply-case model)
	       (prepare-enum-inputs enums))
	     (PREPARE-ENUM-INPUTS (enums)
	       (if (null enums) 
		   (prepare-step-inputs steps)
		   (loop with enum = (first enums)
		      for val in (cddr enum)
		      do (progn
			   (when val (apply-enum-case (second enum) val))
			   (prepare-enum-inputs (cdr enums))))))
	     (PREPARE-STEP-INPUTS (specs)
	       (if (null specs) 
		   (prepare-random-inputs randoms)
		   (loop with spec = (first specs)
		      for val from (third spec) to (fourth spec) by (fifth spec)
		      do (progn
			   (setf (symbol-value (second spec)) val)
			   (prepare-step-inputs (cdr specs))))))
	     (PREPARE-RANDOM-INPUTS (randoms)
	       (dotimes (i trials)
		 (loop for spec in randoms for var = (second spec) for low = (third spec) for high = (fourth spec)
		    do (setf (symbol-value var) (+ low (random (float (- high low))))))
		 (format t "Starting run ~s " (incf count))
		 (finish-output t)
		 (run-model)
		 (format t "*time*=~s~%" *time*)
		 (finish-output t)
		 (collect-data)))
	     (COLLECT-DATA ()
	       (push (mapcar #'symbol-value vars) results)))
      (top-level model))
    (make-instance 'monte-carlo
		   :model model
		   :variables vars
		   :enum-specs enums
		   :step-specs steps
		   :random-specs randoms
		   :trials trials
		   :results results)))

(defun APPLY-ENUM-CASE (var val)
  (cond ((symbolp var)
	 (when val
	   (setf (symbol-value var) val)))
	((and (listp var) (listp val))
	 (mapc #'apply-enum-case var val))
	(t (error "malformed enum"))))

(defun WRITE-MONTE-CARLO (r filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (print (monte-carlo-variables r) out)
    (print (append (monte-carlo-enum-specs r) (monte-carlo-step-specs r) (monte-carlo-random-specs r)) out)
    (print (monte-carlo-trials r) out)
    (print (length (monte-carlo-results r)) out)
    (dolist (result (monte-carlo-results r))
      (print result out))))

(defvar *60-HOURS* '((enum *downlink-rate* 100000 400000)
		     (enum *lookahead-distance-day* 4.5 10)
		     (enum *lookahead-distance-night* 4.5 10)
		     (enum *driver-decision-time-day* 10 30)
		     ;;(enum *path-multiplier* 4 5 6)
		     (enum *onboard-processing-time* 10 55)
		     (enum *hazard-trigger-rate* 0.04 0.02 0.01 0)
		     (enum *bumper-trigger-rate* 0.04 0.02 0.01 0)
		     ))

(defun EXPLORE-PROCESSING-TIME (&optional (n 10))
	(run-scatter :cases '(kitchen-sink sink-low-band sink-high-lat) 
							 :specs '((enum *onboard-processing-time* 1 5 10 15 20 25 30 40 50 60 70 80 100))
							 :trials n 
							 :filename "processing-time.out")
	)
	



;(defun EXPLORE-BASELINE-TRIALS (&optional (n 100))
;  (run-scatter :cases '(baseline) :trials n :filename (format nil "a-explore-baseline-trials-~a.out" n)))
;
;(defun EXPLORE-ALL-CASES-TRIALS (&optional (n 30))
;  (run-scatter :cases *cases* :trials n :filename (format nil "a-explore-all-cases-trials-~a.out" n)))
;
;(defun EXPLORE-TRIGGERS ()
;  (run-scatter :cases '(baseline kitchen-sink lidars-galore)
;	       :specs '((enum *hazard-trigger-rate* 0.04 0.02 0.01 0.005 0.003 0.002 0.001  0.0001 0.00001 0)
;			(enum *bumper-trigger-rate* 0.04 0.02 0.01 0.005 0.003 0.002 0.001  0.0001 0.00001 0))
;	       :filename "a-explore-triggers.out"
;	       :trials 5))
;
;(defun EXPLORE-TRIGGER-EVAL-TIMES ()
;  (run-scatter :cases '(baseline SINK-HIGH-LAT lidars-galore)
;	       :specs '((enum *hazard-eval-time* nil 0 60 90 120 240 300 500)
;			(enum *bumper-eval-time* nil 0 60 90 120 240 300 500))
;	       :filename "a-explore-trigger-eval-times.cases"
;	       :trials 5))
;
;(defun EXPLORE-PATH-LENGTH ()
;  (run-scatter :cases '(BASELINE SINK-HIGH-LAT LIDARS-GALORE)
;	       :specs '((enum *path-multiplier* nil 3 4 5 6))
;	       :filename "a-explore-path-length.cases"
;	       :trials 5))
;
;(defun EXPLORE-PATH-LENGTH-DYNAMIC-RANGE ()
;  (run-scatter-print
;   (loop for dynamic-range from 0 to 2 by 0.2
;      for *variable-override-function* = #'(lambda () (setf *path-multiplier* (* dynamic-range (/ (- *path-multiplier* 3) 3))))
;      appending (run-scatter-cases :cases *cases* :trials 5))
;   :filename "a-explore-path-length-dynamic-range.cases"))
;
;(defun EXPLORE-LOOKAHEAD-AND-DECISION-TIMES ()
;  (run-scatter :cases '(baseline kitchen-sink lidars-galore)
;		:specs '((enum *hazard-trigger-rate* 0)
;			 (enum *bumper-trigger-rate* 0)
;			 (enum *lookahead-distance-day* 4 5 6 7 8 9 10)
;			 (enum *driver-decision-time-day* 10 20 30 40 50))
;		:filename "a-explore-lookahead-and-decision-times.out"
;		:trials 1))		;trials are determanistic if the triggers are zeroed
;
;(defun EXPLORE-ONBOARD-PROCESSING-AND-TRAVERSE ()
;  (run-scatter :cases '(KITCHEN-SINK SINK-HIGH-LAT SINK-LOW-BAND)
;	       :specs '((enum *length-of-autonomous-traverse* 4.5 5 6 7 8 10 12 15)
;			(enum *onboard-processing-time* 1 5 10 15 20 25 30 40 50 60))
;	       :filename "a-explore-onboard-processing-and-traverse.cases"
;	       :trials 1))
;
;(defun EXPLORE-ONBOARD-PROCESSING-AND-TRAVERSEa ()
;  (run-scatter :cases '(kitchen-sink)
;	       :specs '((enum *hazard-trigger-rate* 0.02)
;			(enum *bumper-trigger-rate* 0.002)
;			(enum *length-of-autonomous-traverse* 4.5 5 5.5 6 6.5 7 7.5 8)
;			(enum *autonomous-waypoint-distance* 4.5 8 10 12 14 16 18 20 22 24)
;			(enum *onboard-processing-time* 5 10 15 20 25 30 40 50 60 70))
;	       :filename "a-explore-onboard-processing-and-traverse.cases"))
;
;(defun EXPLORE-DOWNLINK-RATE-LATENCY ()
;  (run-scatter :cases *cases*
;	       :specs '((enum *DOWNLINK-LATENCY* 10 20 30 60 120)
;			(enum *DOWNLINK-RATE* 60000 100000 200000 400000 600000 800000))
;	       :filename "a-explore-downlink-rate-latency.cases"))
;
;(defun EXPLORE-LOOKAHEAD ()
;  (run-scatter :cases '(baseline)
;	       :specs '((enum *hazard-trigger-rate* 0)
;			(enum *bumper-trigger-rate* 0)
;			(enum *DOWNLINK-LATENCY* 10)
;			(enum *DOWNLINK-RATE* 60000 400000)
;			(enum (*LOOKAHEAD-DISTANCE-DAY* *LOOKAHEAD-DISTANCE-NIGHT*) (4.5 4.5) (6 6) (8 8) (10 10) (12 12) (14 14) (16 16))
;			)
;	       :filename "a-explore-lookahead.cases"))
;
;(defun EXPLORE-COMBINED-DOWNLINK-RATE-LATENCY ()
;  (run-scatter :cases *cases*
;	       :specs '((enum (*DOWNLINK-RATE* *DOWNLINK-LATENCY*) (60000 10) (80000 10) (100000 10) (128000 12) (150000 15) (200000 20) (300000 25) (400000 30))
;			)
;	       :filename "a-explore-combined-downlink-rate-latency.cases"))
;
(defun SCATTER-TOGETHER ()
  (run-scatter-print
   (append (run-scatter-cases :cases '(LIDARS-GALORE BAL-LOW-BAND)
			      :specs '((enum *DOWNLINK-LATENCY* 10 20 30 60 120)
				       (enum *DOWNLINK-RATE* 60000 400000 800000)))
	   (run-scatter-cases :cases '(SINK-HIGH-LAT KITCHEN-SINK SINK-LOW-BAND)
			      :specs '((enum *DOWNLINK-LATENCY* 10 20 30 60 120)
				       (enum *DOWNLINK-RATE* 60000 400000 800000)
				       (enum *AUTONOMOUS-WAYPOINT-DISTANCE* 10 30 50)
				       (enum *LENGTH-OF-AUTONOMOUS-TRAVERSE* 4.5 8 11)))
	   (run-scatter-cases :cases '(BAL-LOW-BAND BALANCED BASELINE BAL-HIGH-LAT)
			      :specs '((enum *DOWNLINK-LATENCY* 10 20 30 60 120)
				       (enum *DOWNLINK-RATE* 60000 400000 800000)
				       (enum *LOOKAHEAD-DISTANCE-DAY* 4.5 10)
				       (enum *LOOKAHEAD-DISTANCE-NIGHT* 4.5 10)
				       (enum *DRIVER-DECISION-TIME-DAY* 10 30))))
   :filename "a-scatter-together.trials"))

(defun SCATTER-COMPARE ()
  (run-scatter :cases *cases*
	       :filename "a-scatter-compare.cases"))

(defun RUN-SCATTER (&key (cases *cases*) (trials 1) (specs '()) (filename "a.runs"))
  (run-scatter-print
   (run-scatter-cases :cases cases :trials trials :specs specs)
   :filename filename :trials trials :specs specs))

(defun RUN-SCATTER-CASES (&key (cases *cases*) (trials 1) (specs '()))
  (loop for model in cases
     for mc = (run-monte-carlo model specs trials)
     appending (monte-carlo-results mc)))

(defun RUN-SCATTER-PRINT (runs &key (trials 1) (specs '()) (filename "a.runs"))
  (let* ((mc (make-instance 'monte-carlo
			    :model nil
			    :variables (sorted-variables-of-type :all)
			    :enum-specs (reverse (loop for spec in specs when (eql (first spec) 'enum) collect spec))
			    :step-specs (reverse (loop for spec in specs when (eql (first spec) 'step) collect spec))
			    :random-specs (reverse (loop for spec in specs when (eql (first spec) 'random) collect spec))
			    :trials trials
			    :results runs)))
    (format t "Writing ~s results.~%" (length runs))
    (finish-output t)
    (write-monte-carlo mc filename)))

;;; Threads seem to not work reliably on windows
(defun pRUN-SCATTER (&key (cases *cases*) (trials 1) (specs '()) (filename "a.runs") (process-count 8))
  (setf *random-state* (make-random-state t))
  (let* ((task-lock (make-lock 'task-lock))
	 (results-lock (make-lock 'results-lock))
	 (tasks (make-runargs cases trials specs))
	 (vars (sorted-variables-of-type :all))
	 (results '()))
    (labels ((worker-function (i)
	       (loop for task = (with-lock-grabbed (task-lock) (pop tasks))
		  while task
		  do (progn
		       (format t "Running ~a in ~a~%" (first task) i)
		       (finish-output t)
		       (apply-case (first task))
		       (loop for var in vars for val in (cdr task) do (when val (setf (symbol-value var) val)))
		       (run-model)
		       (with-lock-grabbed (results-lock)
			 (push (cons (car task) (mapcar #'symbol-value vars)) results))))))
      (format t "There are ~a cases to run.~%" (length tasks))
      (finish-output t)
      (let ((processes '())
	    mc)
	(dotimes (i process-count)
	  (push (process-run-function (format nil "worker-~a" i) #'worker-function i) processes))
	(dolist (p processes)
	  (join-process p))
	(setq mc (make-instance 'monte-carlo
				:model nil
				:inputs (sorted-variables-of-type :input)
				:outputs (sorted-variables-of-type :output)
				:variables (append (sorted-variables-of-type :input)(sorted-variables-of-type :output))
				:enum-specs (reverse (loop for spec in specs when (eql (first spec) 'enum) collect spec))
				:step-specs (reverse (loop for spec in specs when (eql (first spec) 'step) collect spec))
				:random-specs (reverse (loop for spec in specs when (eql (first spec) 'random) collect spec))
				:trials trials
				:results results))
	(format t "Writing ~s results.~%" (length results))
	(finish-output t)
	(write-monte-carlo mc filename)))))

(defun MAKE-RUNARGS (cases trials specs)
  (let* ((vars (sorted-variables-of-type :all))
	 (enums (reverse (loop for spec in specs when (eql (first spec) 'enum) collect spec)))
	 (randoms (reverse (loop for spec in specs when (eql (first spec) 'random) collect spec)))
	 (steps (reverse (loop for spec in specs when (eql (first spec) 'step) collect spec)))
	 (runargs1 '()))
    (labels ((PREPARE-ENUM-INPUTS (enums)
	       (if (null enums) 
		   (prepare-step-inputs steps)
		   (loop with enum = (first enums)
		      for val in (cddr enum)
		      do (progn
			   (when val (setf (symbol-value (second enum)) val))
			   (prepare-enum-inputs (cdr enums))))))
	     (PREPARE-STEP-INPUTS (specs)
	       (if (null specs) 
		   (prepare-random-inputs randoms)
		   (loop with spec = (first specs)
		      for val from (third spec) to (fourth spec) by (fifth spec)
		      do (progn
			   (setf (symbol-value (second spec)) val)
			   (prepare-step-inputs (cdr specs))))))
	     (PREPARE-RANDOM-INPUTS (randoms)
	       (dotimes (i trials)
		 (loop for spec in randoms for var = (second spec) for low = (third spec) for high = (fourth spec)
		    do (setf (symbol-value var) (+ low (random (float (- high low))))))
		 (push (mapcar #'symbol-value vars) runargs1))))
      (prepare-enum-inputs enums))
    (let ((runargs '()))
      (dolist (c cases)
	(dolist (r runargs1)
	  (push (cons c r) runargs)))
      runargs)))

