;;;
;;; Trade Cases
;;;

(defvar *CASES* '())
(defvar *CASE-KEYWORDS* '())
(defvar *KEYWORD-TO-VARIABLE-MAPPING* '())

(defmacro DEFINE-CASE (name docstring &rest key-val-pairs)
  `(define-case1 ',name ,docstring ',(loop for v in key-val-pairs collect (if (eql v '?) ''? v))))

(defun DEFINE-CASE1 (name docstring key-val-pairs)
  (pushnew name *cases*)
  (setf (get name :case) t)
  (setf (get name :docstring) docstring)
  (labels ((record-value (key val)
	     (pushnew key *case-keywords*)
	     (unless (get key :variable)
	       (setf (get key :variable) (get-variable-from-keyword key)))
	     (setf (get name key) val)
	     (set (get key :variable) val)))
    (record-value :design-case-name name)
    (loop for (key form) on key-val-pairs by #'cddr
       for val = (eval form)
       do (record-value key val))))

(defun GET-VARIABLE-FROM-KEYWORD (symbol)
  (intern (concatenate 'string "*" (string symbol) "*")))

(defun APPLY-CASE (name &aux temp)
  (unless (member name *cases*)
    (error "There is no case named ~a" name))
  (let ((plist (symbol-plist name)))
    (dolist (keyword *case-keywords*) 
      (let ((var (get keyword :variable))
	    (val (get name keyword)))
	(cond (val 
	       (set var val))
	      ((setq temp (member keyword plist))
	       (set var (second temp)))
	      (t
	       (set var (get (get keyword :variable) :default))))))))

(defvar *VARIABLE-OVERRIDE-FUNCTION* nil)

(defun RUN-CASE (name)
  (apply-case name)
  (when *variable-override-function* 
    (funcall *variable-override-function*))
  (run-model))

(defun RUN-MODELS (models)
  (let ((vars (sorted-variables-of-type :all)))
    (dolist (var vars) (setf (get var :values) '())))
  (dolist (m models)
    (apply-case m)
    (format t "~s => ~s~%" m (run-model))))

(defun COMPARE-MODELS (models &key (sort-by-speed-made-good t))
  ;; Print the large table
  (let ((vars (sorted-variables-of-type :all)))
    (dolist (var vars) (setf (get var :values) '()))
    (dolist (m models)
      (trace-when 1 
		  (format *terminal-io* "Running ~a~%" m)
		  (finish-output nil))
      (run-case m)
      (dolist (var vars)
	(push (cons m (symbol-value var)) (get var :values))))
    (dolist (var vars)
      (setf (get var :values)
	    (reverse (get var :values)))) ;Now the values are in the same order as the models were run
    (present-model-comparison
     (if sort-by-speed-made-good
	 (sort-models models '*speed-made-good*)
	 models))))

(defun SORT-MODELS (models variable)
  (mapcar #'cdr
	  (sort (loop with row = (get variable :values)
		   for m in models
		   for val = (cdr (assoc m row))
		   collect (cons val m))
		#'(lambda (a b) (> (car a) (car b))))))

;;; Not used
(defun GET-PERMUTATION-LIST (var)
  (mapcar #'cdr
	  (sort (loop for i from 0
		   for val in (reverse (get var :values))
		   collect (cons val i))
		#'(lambda (a b) (> (car a) (car b))))))

(defun PERMUTE-LIST (permutation list)
  (loop for i in permutation collect (nth i list)))

(defun PRESENT-MODEL-COMPARISON (models)
  (let ((vars (sorted-variables-of-type :all)))
    (printing-table
     (printing-table-header 
      ()
      (printing-table-cell (princ "Variable"))
      (dolist (m models) 
	(printing-table-cell (princ m))))
     (dolist (var vars)
       (printing-table-row 
	()
	(printing-table-cell (princ var))
	(let ((row (get var :values)))
	  (dolist (m models)
	    (printing-table-cell (princ-value (cdr (assoc m row)))))))))
    ;; provide data for a speed-made-good bar chart
    (format t "~%Speed made good bar chart~%")
    (format t "	Speed made good~%")
    (loop with row = (get '*speed-made-good* :values)
       for m in models
       for speed-made-good = (cdr (assoc m row))
       do (format t "~a	~12,5f~%" m speed-made-good))
    ;; provide data for a speed-made-good-along-path bar chart
    (format t "~%Speed made good along path bar chart~%")
    (format t "	Speed made good along path~%")
    (loop with row = (get '*speed-made-good-along-path* :values)
       for m in models
       for speed-made-good = (cdr (assoc m row))
       do (format t "~a	~12,5f~%" m speed-made-good))))

(defun PRINC-VALUE (v)
  (typecase v
    (float (format t "~12,5f" v))
    (t (format t "~12@a" v))))

(defun WRAP-PRINT-SYMBOL (symbol width)	;not used, currently
  (let* ((string (string symbol))
	 (tokens (loop for start = 0 then (1+ finish)
		   for finish = (position #\- string :start start)
		   collecting (subseq string start (if (numberp finish) (1+ finish) finish))
		   until (null finish)))
	(print-column 0))
    (dolist (tok tokens)
      (when (> (+ print-column (length tok)) width)
	(terpri))
      (princ tok)
      (incf print-column (length tok)))))

;;;
;;; The cases themselves
;;;

(define-case AoA2 "AoA2 case"
  :design-case				11
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 1024 1024 12) ;one full image
  :belly-bits				0
  :lidar-bits				0
  :bumper-bits				0

  :driving-method			:stop-and-go
  :default-sensor-payload		'navcam-payload
  :downlink-rate			100000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 1) 1 6) ;not-stereo, no-subframing, compression=6 (appropriate for humans)
  :hazcam-payload			0  ; (/ (* *hazcam-bits* 1) 1 6) ; forward and backward navcams, use one at a time; no separate hazcams
  :belly-payload			0
  :lidar-payload			0
  :bumper-payload			0
  
;; Consider breaking this into (a) distance that's safe to drive and (b) distance that one can see

  :lookahead-distance-day		4 ; lunokhod was 4.3m w better lighting
  :lookahead-distance-night		4 ; think about these again (night driving=>lower dynamic range=>better quality images
  :driver-decision-time-day		90 ; 70   ; 2.5*90 = a*1.5 + b
  :driver-decision-time-night		89 
  :rt-science-consultation-time		180 ; reconsider
  :rt-science-consultation-rate		0  ;  FOR NOW (/ 1.0 50) ;1 per 50 m

;; We need to think about a proper fudge factor to account for the difference between rails and science stations

  :hazard-trigger-rate			(/ 1.0 25) ; (/ 1.0 10)	   ;1 per 25 m (mean free path)
  :hazard-eval-payload			(* 2 *navcam-bits*);
  :hazard-eval-time			240

  :bumper-trigger-rate			0 ;2 per 250 m
  :bumper-eval-payload			(+ (* 6 2 *hazcam-bits*) (* 5 2 *navcam-bits*)) ;11 stereo pairs
  :bumper-eval-time			300

  :hazard-encounter-trigger-rate         (/ 1.0 1000)
  :hazard-encounter-eval-time            3600

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			5
  )

(define-case BASELINE "2 Nav cams w/2 structured lights on mast; 2 Haz cams w/2 structured lights under chassis"
  :design-case				1 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 1024 1024 12) ;one full image
  :belly-bits				0
  :lidar-bits				0
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:stop-and-go
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		30
  :driver-decision-time-night		45 
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 25)	   ;1 per 25 m
  :hazard-eval-payload			(* 4 2 *navcam-bits*) ;4 stereo pairs
  :hazard-eval-time			120

  :bumper-trigger-rate			(/ 2.0 250) ;2 per 250 m
  :bumper-eval-payload			(+ (* 6 2 *hazcam-bits*) (* 5 2 *navcam-bits*)) ;11 stereo pairs
  :bumper-eval-time			300

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			5
  )

(define-case BALANCED "2 Nav cams on mast; 4 Haz cams w/4 structured lights on chassis; 1 Fish eye lens under chassis; 3 flood lights"
  :design-case				2 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				(* 1024 1024 12) ;one full image
  :lidar-bits				0
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:continuous ;stop-and-go-continuous?
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		15
  :driver-decision-time-night		30 
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 25) ;1 per 25 m
  :hazard-eval-payload			(* 3 2 *navcam-bits*) ;3 stereo pairs  (WHY FEWER THAN BASELINE?)
  :hazard-eval-time			120

  :bumper-trigger-rate			(/ 1.0 250) ;1 per 250 m
  :bumper-eval-payload			(+ (* 4 2 *hazcam-bits*) (* 7 2 *navcam-bits*) *belly-bits*) ;What is FEL?
  :bumper-eval-time			150 ;WHY LESS THAN BASELINE?

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			4 ;WHY?
  )

(define-case KITCHEN-SINK "2 Nav cams on mast; 8 Haz cams w/8 structured lights on chassis; 1 Fish eye lens under chassis; 3 flood lights"
  :design-case				3 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				(* 1024 1024 12)
  :lidar-bits				0
  :bumper-bits				(* 2 20 4 8) ;NOT FILLED  IN

  :driving-method			:autonomous
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			0
  :bumper-payload			*bumper-bits* ;NOT FILLED IN
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		15 ;NOT FILLED IN
  :driver-decision-time-night		30 ;NOT FILLED IN
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 25) ;1 per 25 m  (NOT FILLED IN)
  :hazard-eval-payload			(* 3 2 *navcam-bits*) ;4 stereo pairs (WHY LESS)
  :hazard-eval-time			120

  :bumper-trigger-rate			(/ 2.0 250) ;2 per 250 m
  :bumper-eval-payload			(+ (* 4 2 *hazcam-bits*) (* 7 2 *navcam-bits*) *belly-bits*) ;NOT FILLED IN
  :bumper-eval-time			150

  :onboard-processing-time		55
  :length-of-autonomous-traverse	4.5
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			4 ;WHY?
  )

(define-case LIDARS-GALORE "2 Nav cams, 1 LIDAR on mast; 4 LIDARs on chassis; 1 Fish eye lens under chassis"
  :design-case			4 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 128 128 24) ;one full image
  :belly-bits				(* 1024 1024 12) ;one full image
  :lidar-bits				(* 128 128 24)
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:continuous
  :default-sensor-payload		'lidar-payload
  :downlink-rate			400000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		11
  :lookahead-distance-night		11
  :driver-decision-time-day		1
  :driver-decision-time-night		1
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 200) ;1 per 25 m
  :hazard-eval-payload			(* 2 2 *navcam-bits*) ;2 stereo pairs  ?? CHECK W MARK
  :hazard-eval-time			90

  :bumper-trigger-rate			(/ 1.0 500) ;1 per 500 m
  :bumper-eval-payload			(+ (* 1 *belly-bits*) (* 4 *lidar-bits*))
  :bumper-eval-time			400

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			5
  )

(define-case BALANCED2 "2 Nav cams on mast; 4 Haz cams w/4 structured lights on chassis; 1 Fish eye lens under chassis"
  :design-case				5 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				(* 1024 1024 12)
  :lidar-bits				(* 128 128 24) ;WHY DIFFERENT 
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:stop-and-go
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			10 ;NOT IN SPREADSHEET
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ *hazcam-bits* 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			0
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		45
  :driver-decision-time-night		60 
  :rt-science-consultation-time		180
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 20)	  ;1 per 20 m
  :hazard-eval-payload			(+ (* 4 2 *navcam-bits*) (* 4 *hazcam-bits*) *belly-bits*)
  :hazard-eval-time			240

  :bumper-trigger-rate			(/ 2.0 250) ;2 per 250 m
  :bumper-eval-payload			(+ (* 7 2 *navcam-bits*) (* 4 *navcam-bits*) *belly-bits*)
  :bumper-eval-time			400

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			4
  )


(define-case BAL-HIGH-LAT "2 Nav cams on mast; 4 Haz cams w/4 structured lights on chassis; 1 Fish eye lens under chassis"
  :design-case				5 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				(* 1024 1024 12)
  :lidar-bits				(* 128 128 24) ;WHY DIFFERENT 
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:stop-and-go
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			120 ;NOT IN SPREADSHEET
  :uplink-latency			120

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ *hazcam-bits* 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			0
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		45
  :driver-decision-time-night		60 
  :rt-science-consultation-time		180
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 20)	  ;1 per 20 m
  :hazard-eval-payload			(+ (* 4 2 *navcam-bits*) (* 4 *hazcam-bits*) *belly-bits*)
  :hazard-eval-time			240

  :bumper-trigger-rate			(/ 2.0 250) ;2 per 250 m
  :bumper-eval-payload			(+ (* 7 2 *navcam-bits*) (* 4 *navcam-bits*) *belly-bits*)
  :bumper-eval-time			400

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			4
  )

(define-case SINK-HIGH-LAT "2 Nav cams, 1 LIDAR on mast; 8 Haz cams w/8 structured lights on chassis; 1 Fish eye lens under chassis; 6 flood lights (mast, each haz cam pair, belly cam)"
  :design-case				6 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				(* 1024 1024 12)
  :lidar-bits				(* 128 128 24)
  :bumper-bits				(* 4 20 4 8) ;4 structured light bumpers, 20 pts each

  :driving-method			:autonomous
  :default-sensor-payload		'lidar-payload
  :downlink-rate			400000
  :downlink-latency			120
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 1 6) ;stereo, subframing=1, compression=6
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		8
  :lookahead-distance-night		8
  :driver-decision-time-day		1
  :driver-decision-time-night		1
  :rt-science-consultation-time		180
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 50) ;1 per 50 m
  :hazard-eval-payload			(+ (* 7 2 *navcam-bits*) (* 8 *hazcam-bits*) *belly-bits* (* 7 *lidar-bits*)) ;WHY BELLY?
  :hazard-eval-time			225

  :bumper-trigger-rate			(/ 1.0 500)
  :bumper-eval-payload			(+ (* 7 2 *navcam-bits*) (* 8 *hazcam-bits*) *belly-bits* (* 7 *lidar-bits*))
  :bumper-eval-time			60

  :onboard-processing-time		20
  :length-of-autonomous-traverse	8
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			3
  )

(define-case BUMPER-CAR "2 Nav cams w/2 structured lights on mast; 2 Haz cams under chassis; 2 flood lights (mast and haz cams)"
  :design-case				7 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 1024 1024 12) ;one full image
  :belly-bits				0
  :lidar-bits				0
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:continuous
  :default-sensor-payload		'navcam-payload
  :downlink-rate			400000
  :downlink-latency			120
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 1 4) ;stereo, subframing=1, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 1 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		45
  :driver-decision-time-night		60 
  :rt-science-consultation-time		180
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 12.5)
  :hazard-eval-payload			(* 22 *navcam-bits*) ;off of spreadsheet
  :hazard-eval-time			240

  :bumper-trigger-rate			(/ 2.0 250) ;2 per 250 m
  :bumper-eval-payload			(* 22 *navcam-bits*)
  :bumper-eval-time			500

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			6
  )

(define-case SINK-LOW-BAND "2 Nav cams on mast; 8 Haz cams w/8 structured lights on chassis; 1 Fish eye lens under chassis"
  :design-case			8 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 521 12) ;one full image
  :belly-bits				(* 1024 1024 12) ;one full image
  :lidar-bits				0
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:autonomous
  :default-sensor-payload		'navcam-payload
  :downlink-rate			60000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 6) ;stereo, subframing=2, compression=6
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			*belly-bits*
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		4.5
  :lookahead-distance-night		4.5 
  :driver-decision-time-day		30
  :driver-decision-time-night		45 
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 50)	   ;1 per 50 m
  :hazard-eval-payload			(+ (* 3 2 *navcam-bits*) (* 8 *hazcam-bits*))
  :hazard-eval-time			120

  :bumper-trigger-rate			(/ 1.0 500) ;1 per 500 m
  :bumper-eval-payload			(+ (* 2 *hazcam-bits*) *belly-bits*)
  :bumper-eval-time			150

  :onboard-processing-time		55
  :length-of-autonomous-traverse	4.5
  :post-autonomy-payload		(* 2 *navcam-bits*)
  :path-multiplier			3
  )

(define-case BAL-LOW-BAND "2 Nav cams (not used for driving), 1 LIDAR on mast; 4 Haz cams w/4 structured lights on chassis; 1 Fish eye lens under chassis"
  :design-case				9 
  :navcam-bits				(* 1024 1024 12) ;one full image
  :hazcam-bits				(* 512 512 12) ;one full image
  :belly-bits				0
  :lidar-bits				(* 128 128 16)
  :bumper-bits				(* 2 20 4 8)

  :driving-method			:continuous
  :default-sensor-payload		'lidar-payload
  :Downlink-rate			60000
  :downlink-latency			10
  :uplink-latency			10

  :navcam-payload			(/ (* *navcam-bits* 2) 2 4) ;stereo, subframing=2, compression=4
  :hazcam-payload			(/ (* *hazcam-bits* 2) 4) ;stereo, subframing=1, compression=4
  :belly-payload			(/ *belly-bits* 4)
  :lidar-payload			(/ *lidar-bits* 2) ;compression=2
  :bumper-payload			*bumper-bits*
  
  :lookahead-distance-day		9
  :lookahead-distance-night		9
  :driver-decision-time-day		1
  :driver-decision-time-night		1 
  :rt-science-consultation-time		60
  :rt-science-consultation-rate		(/ 1.0 100) ;1 per 100 m

  :hazard-trigger-rate			(/ 1.0 100)	   ;1 per 100 m
  :hazard-eval-payload			(* 4 2 *navcam-bits*) ;4 stereo pairs
  :hazard-eval-time			105

  :bumper-trigger-rate			(/ 1.0 400) ;1 per 400 m
  :bumper-eval-payload			(+ (* 6 2 *hazcam-bits*) (* 5 2 *navcam-bits*)) ;11 stereo pairs
  :bumper-eval-time			150

  :onboard-processing-time		0 
  :length-of-autonomous-traverse	0 
  :post-autonomy-payload		0
  :path-multiplier			3
  )

(defun RECONSTRUCT-CASE ()
  `(define-case ,*design-case-name* "Reconstructed case"
       ,@(loop for keyword in  *case-keywords*
	      collect keyword
	      collect (list 'quote (symbol-value (get-variable-from-keyword keyword))))))

(defun OUTPUT-MODEL (&key (model 'baseline) (filename "trace.out"))
  (apply-case model)
  (with-open-file (out filename :direction :output)
    (let ((*standard-output* out)
	  (*trace-output* out)
	  (*trace-level* 2))
      (run-model))))

(defun OUTPUT-MODELS (models)
  (dolist (model models)
    (output-model :model model :filename (format nil "~a.out" model))))

(defun OUTPUT-ALL-MODELS ()
  (output-models *cases*))

(defvar *PCOMPARE-RESULTS* '())

;;; Parallel version
(defun PCOMPARE-MODELS (models &key (sort-by-speed-made-good t))
  (setq *pcompare-results* '())
  (setq models (reverse models))
  ;; Print the large table
  (let* ((lock (make-lock 'model-lock))
	 (vars (sorted-variables-of-type :all))
	 (initial-bindings (loop for var in vars collect (cons var `(symbol-value ,var)))))
    (let ((processes
	   (loop for model in models 
	      for p = (make-process (string model) :initial-bindings initial-bindings)
	      collect (progn
			(process-preset p #'(lambda (model)
					      (run-case model)
					      (with-lock-grabbed (lock)
						(push (cons model (mapcar #'symbol-value vars)) *pcompare-results*)))
					model)
			(process-enable p)
			p))))
      ;; Wait for all of the processes to end
      (loop while processes
	 do (if (process-exhausted-p (first processes))
		(pop processes)
		(sleep 1)))
      ;; The results are in *pcompare-results*.  Convert them to the
      ;; form that COMPARE-MODELS uses.
      (dolist (var vars) (setf (get var :values) '()))
      (dolist (m models)
	(let ((values (cdr (assoc m *pcompare-results*))))
	  (when (null values)
	    (error "shouldn't get here"))
	  (loop for var in vars
	     for val in values
	     do (push (cons m val) (get var :values)))))
      (present-model-comparison
       (if sort-by-speed-made-good
	   (sort-models models '*speed-made-good*)
	   models)))))

(defun COMPARE-ALL-MODELS (&optional overrides)
  (let ((*variable-override-function* #'(lambda () (apply-model-overrides overrides))))
    (compare-models (sort (copy-list *cases*) #'(lambda (a b) (string-lessp (string a) (string b)))))))

(defun PCOMPARE-ALL-MODELS (&optional overrides)
  (let ((*variable-override-function* #'(lambda () (apply-model-overrides overrides))))
    (pcompare-models (sort (copy-list *cases*) #'(lambda (a b) (string-lessp (string a) (string b)))))))

(defun APPLY-MODEL-OVERRIDES (overrides)
  (loop for (var val) in overrides
       do (setf (symbol-value var) val)))

;;;
;;; 100m bandwidth usage
;;;

(defun RUN-100M ()
  (let ((*variable-override-function* #'(lambda ()
					  (setq *driving-distance* 100)
					  (setq *path-multiplier* 1)
					  )))
    (run-case 'baseline)
    (values
     *total-downlink*
     (/ (* *total-downlink* 1000000 8) *final-time* ))))

(defun RUN-100M-STOCHASTICALLY (&optional (count 10))
  (/ (apply #'+ (loop for x from 1 to count collect (run-100m)))
     (float count)))
