;;;
;;; RP Model
;;;

;;; Inputs

;;; This will be treated specially
(define-input *DESIGN-CASE* 1)				; no units
(define-input *DESIGN-CASE-NAME* 'baseline)		;

;;; Fixed driving constants
(defvar *DRIVING-DISTANCE* 1000)		        ; m
(defvar *DRIVING-SPEED* 0.1)				; m/s
(defvar *PSR-1-ENTRY* 500)				; m
(defvar *PSR-1-LENGTH* 30)				; m
(defvar *PSR-2-ENTRY* 900)				; m
(defvar *PSR-2-LENGTH* 100)				; m
(defvar *SIMULATION-TIME-CUTOFF* (* 3600 24 10))       ;10 days in sec
(defvar *SIMULATION-DISTANCE-CUTOFF* 999999999)	       ; m

;;; Driving variables
(define-input *DRIVING-METHOD* :stop-and-go)		; enumeration :stop-and-go :continuous :autonomous
(define-input *DEFAULT-SENSOR-PAYLOAD* 'nav-payload)	; determines which type of message is sent
(define-input *PATH-MULTIPLIER* 5)			; factor

;;; COMM
(define-input *DOWNLINK-RATE* 400000)			; bps
(define-input *DOWNLINK-LATENCY* 10)			; sec
(define-input *UPLINK-LATENCY* 10)			; sec

;;; Images
(define-input *NAVCAM-BITS* 0)				;bits
(define-input *HAZCAM-BITS* 0)				;bits
(define-input *BELLY-BITS* 0)				;bits
(define-input *LIDAR-BITS* 0)				;bits
(define-input *BUMPER-BITS* 0)				;bits

(define-input *NAVCAM-PAYLOAD* 0)				;bits
(define-input *HAZCAM-PAYLOAD* 0)			;bits
(define-input *BELLY-PAYLOAD* 0)			;bits
(define-input *LIDAR-PAYLOAD* 0)			;bits
(define-input *BUMPER-PAYLOAD* 0)			;bits

(define-input *ONBOARD-PROCESSING-TIME* 0)		; sec

;;; Other Sensor-related parameters

(define-input *LOOKAHEAD-DISTANCE-DAY* 4.5)		; meters
(define-input *LOOKAHEAD-DISTANCE-NIGHT* 4.5)		; meters

;;; Ground Processing
(define-input *GROUND-PROCESSING-TIME* 2)		; sec

;;; Ops Team
(define-input *DRIVER-DECISION-TIME-DAY* 10)		; sec
(define-input *DRIVER-DECISION-TIME-NIGHT* 10)		; sec
(define-input *RT-SCIENCE-CONSULTATION-TIME* 30)	; sec
(define-input *RT-SCIENCE-CONSULTATION-RATE* .01) ; consultations / meter
(define-input *ENTER-PSR-DECISION-TIME* (* 1 60))	  ; 1 minute (almost takes this out of the simulation)

;;; Hazards
(define-input *HAZARD-TRIGGER-RATE* (/ 1.0 25))	    ; triggers / meter
(define-input *HAZARD-EVAL-PAYLOAD* 0)		    ; bits
(define-input *HAZARD-EVAL-TIME* 300)		    ; sec
(define-input *BUMPER-TRIGGER-RATE* (/ 1.0 100))    ; triggers / meter
(define-input *BUMPER-EVAL-PAYLOAD* 0)		    ; no units
(define-input *BUMPER-EVAL-TIME* 300)		    ; time

(define-input *HAZARD-ENCOUNTER-TRIGGER-RATE* 0)
(define-input *HAZARD-ENCOUNTER-EVAL-TIME* (* 60 60)) ;1 hour

;;; Autonomy
(define-input *LENGTH-OF-AUTONOMOUS-TRAVERSE* 4.5)	; meters
(define-input *POST-AUTONOMY-PAYLOAD* 10)		;defined by cases
(define-INPUT *AUTONOMOUS-WAYPOINT-DISTANCE* 50)	; 50 m


;;; Outputs
(define-output *ROVER-WAITING-TIME* nil)     ; hours
(define-output *ROVER-DRIVING-TIME* nil)     ; hours
(define-output *DUTY-CYCLE* nil)	     ; fraction
(define-output *SPEED-MADE-GOOD* nil)	     ; m/s
(define-output *SPEED-MADE-GOOD-ALONG-PATH* nil) ; m/s
(define-output *TIME-TO-DRIVE-SCENARIO* nil)	 ; hours
(define-output *AVERAGE-CYCLE-TIME* nil) ; seconds (time between commands)
(define-output *FINAL-TIME* nil)	 ; seconds
(define-output *FINAL-ROVER-POSITION* nil) ; m
(define-output *TOTAL-DOWNLINK* 0)	   ; MB

;;; Intermediate variables that can be plotted
(define-input *DRIVE-DISTANCE-ALONG-PATH* 0)


;;; Intermediate variables that can't be plotted
;;; None

;;;
;;; Class instances
;;;

(defvar *ROVER*)
(defvar *DRIVER*)
(defvar *REALTIME*)
(defvar *GDS*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Message sent at intervals to cause the rover to capture sensor data.
(defclass WAKEUP-SENSORS (message) ())
(defclass WAKEUP-AUTONOMY (message) ())

;;; Represents one or more packets from sensors that are sent together
(defclass SENSOR-PAYLOAD (message)
  ((time-sent :accessor sensor-payload-time-sent :initarg :time-sent :initform 0)
   (position :accessor sensor-payload-position :initarg :position :initform 0)))

(defmethod PRINT-OBJECT ((p SENSOR-PAYLOAD) s)
  (format s "{~a :sent ~a :pos ~a :pld ~a}" (class-name (class-of p)) (sensor-payload-time-sent p) (sensor-payload-position p) (sensor-payload-bits p)))

(defmethod SENSOR-PAYLOAD-SAMPLING-DELAY ((p sensor-payload)) 0) ;always 0 for now

(defmethod TIME-TO-DOWNLINK-SENSOR-PAYLOAD ((p sensor-payload)) ;time from first bit out of the radio to last bit out
  (/ (sensor-payload-bits p) (float *downlink-rate*)))

(defmethod SENSOR-PAYLOAD-LATENCY ((p sensor-payload))
  (+ (time-to-downlink-sensor-payload p) *downlink-latency*))

(defclass NAVCAM-PAYLOAD (sensor-payload) ())
(defmethod SENSOR-PAYLOAD-BITS ((n NAVCAM-PAYLOAD)) *navcam-payload*)

(defclass LIDAR-PAYLOAD (sensor-payload) ())
(defmethod SENSOR-PAYLOAD-BITS ((n LIDAR-PAYLOAD)) *lidar-payload*)

(defclass HAZARD-EVAL-PAYLOAD (sensor-payload) ())
(defmethod SENSOR-PAYLOAD-BITS ((n HAZARD-EVAL-PAYLOAD)) *hazard-eval-payload*)

(defclass BUMPER-EVAL-PAYLOAD (sensor-payload) ())
(defmethod SENSOR-PAYLOAD-BITS ((n BUMPER-EVAL-PAYLOAD)) *bumper-eval-payload*)

(defclass POST-AUTONOMY-PAYLOAD (sensor-payload)
  ((position :accessor waypoint-reached-position :initarg :position)))
(defmethod SENSOR-PAYLOAD-BITS ((p POST-AUTONOMY-PAYLOAD)) *post-autonomy-payload*)

(defclass HAZARD-EVAL-PAYLOAD (sensor-payload) ())
(defmethod SENSOR-PAYLOAD-BITS ((p HAZARD-EVAL-PAYLOAD)) *hazard-eval-payload*)

(defclass DRIVE-COMMAND (message)
  ((target :accessor drive-command-target :initarg :target)))

(defmethod PRINT-OBJECT ((d drive-command) stream)
  (format stream "{DRIVE-COMMAND :target ~a}" (drive-command-target d)))

(defclass WAYPOINT-COMMAND (drive-command) ())

(defmethod PRINT-OBJECT ((d waypoint-command) stream)
  (format stream "{WAYPOINT-COMMAND :target ~a}" (drive-command-target d)))

(defclass AUTHORIZED-TO-ENTER-PSR (message) ())
(defclass POST-AUTONOMY-EVAL (message) ())
(defclass SEND-ROVER-COMMAND (message) ())
(defclass SEND-HAZARD-PAYLOAD (message) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; DRIVER
;;;

(defclass DRIVER (actor)
  ((image :accessor driver-image :initarg :image :initform nil)
   (image-under-evaluation :accessor driver-image-under-evaluation :initform nil)
   (image-eval-target-time :accessor driver-image-eval-target-time :initform 0)
   (commands-sent :accessor driver-commands-sent :initform 0)
   ))

(defmethod PRINT-OBJECT ((d DRIVER) stream)
  (format stream "{~a: :state ~a" (class-name (class-of d)) (state d))
  (when (driver-image d)
    (format stream " :image-time ~f" (receive-time (driver-image d))))
  (when (eql (state d) :evaluating-image)
    (format stream  " :eval-target ~f" (driver-image-eval-target-time d)))
  (format stream "}"))

(defmethod RECEIVE ((d DRIVER) (image SENSOR-PAYLOAD) &aux (state (state d)))
  (case state
    (:IDLE
     (cond ((hazard-encounter-interrupt? image)
	    (setf (state d) :evaluating-hazard-encounter)
	    (send d *rover* (make-instance 'send-hazard-payload) :delay *uplink-latency*))
	   ((hazard-interrupt? image)
	    (setf (state d) :evaluating-hazard)
	    (send d *rover* (make-instance 'send-hazard-payload) :delay *uplink-latency*))
	   (t
	    (let ((decision-time (if (in-psr? (sensor-payload-position image)) *driver-decision-time-night* *driver-decision-time-day*)))
	      (setf (driver-image d) image)
	      (setf (driver-image-under-evaluation d) image)
	      (setf (driver-image-eval-target-time d) (+ *time* decision-time))
	      (set-state d :evaluating-image)
	      (send-wakeup d :delay decision-time)))))
    ((:EVALUATING-IMAGE
      :EVALUATING-HAZARD
      :EVALUATING-HAZARD-ENCOUNTER) ;When there is already an image, don't change the driver's attention
     (setf (driver-image d) image))
    (:DRIVING-TO-PSR-EDGE
     (when (in-psr? (sensor-payload-position image))
       ;; Should get a panorama, but I'm just going to take longer to evaluate it.
       (send d d (make-instance 'authorized-to-enter-psr) :delay *enter-psr-decision-time*)
       (set-state d :waiting-for-authorization-to-enter-psr)))
    (:WAITING-FOR-AUTHORIZATION-TO-ENTER-PSR)))

(defmethod RECEIVE ((d DRIVER) (image HAZARD-EVAL-PAYLOAD))
  (do-later d (if (eql (state d) :evaluating-hazard-encounter) *hazard-encounter-eval-time* *hazard-eval-time*)
	    (setf (state d) :idle)
	    (send d d (make-instance 'sensor-payload :time-sent (sensor-payload-time-sent image) :position (sensor-payload-position image)) :delay 0)))

(defmethod RECEIVE ((d DRIVER) (authorization AUTHORIZED-TO-ENTER-PSR) &aux (state (state d)))
  (case state
    (:WAITING-FOR-AUTHORIZATION-TO-ENTER-PSR
     (set-state d :idle)) ;release to go after examining the next image pair
    (otherwise
     (format T "received authorized to enter while in ~a state~%" (state d))
     (set-state d :idle))))

(defmethod TICK ((d DRIVER) state)
  (case state
    (:IDLE)
    ((:EVALUATING-IMAGE
      :BUMPER-HALT)
     (when (>= *time* (driver-image-eval-target-time d)) ;; Are we finished evaluating an image?
       (let* ((target (+ (sensor-payload-position (driver-image-under-evaluation d))
			 (drive-command-distance d)))
	      (will-enter (will-drive-enter-psr? target (driver-image-under-evaluation d))))
	 (send d *rover* (make-instance 'drive-command
					:target (if will-enter (clip-drive-to-psr-edge target) target))
	       :delay *uplink-latency*)
	 (incf (driver-commands-sent d))
	 (cond (will-enter
		(set-state d :driving-to-psr-edge))
	       ((and (eql (driver-image d) (driver-image-under-evaluation d)) ;no new images have come
		     (not (member (state d) '(:driving-to-psr-edge :waiting-for-authorization-to-enter-psr)))) ;we're not waiting to enter a PSR
		(set-state d :idle))			;we're done
	       (t
		(set-state d :evaluating-image)
		(setf (driver-image-under-evaluation d) (driver-image d))
		(setf (driver-image-eval-target-time d)
		      (+ *time* *driver-decision-time-day*)))))))
    ((:DRIVING-TO-PSR-EDGE
      :EVALUATING-HAZARD
      :EVALUATING-HAZARD-ENCOUNTER
      :WAITING-FOR-AUTHORIZATION-TO-ENTER-PSR))))

(defmethod DRIVE-COMMAND-DISTANCE ((d DRIVER))
  (let ((pos (sensor-payload-position (driver-image-under-evaluation d))))
    (if (in-psr? pos) *lookahead-distance-night* *lookahead-distance-day*)))

;; Will this proposed drive transition from outside to inside a PSR?
(defun WILL-DRIVE-ENTER-PSR? (target image)
  (and (in-psr? target)
       (not (in-psr? (sensor-payload-position image)))))

(defun CLIP-DRIVE-TO-PSR-EDGE (target)
  (let ((d (/ target *path-multiplier*)))
    (cond ((> d *psr-2-entry*)
	   (1+ (* *psr-2-entry* *path-multiplier*)))
	((> d *psr-1-entry*)
	 (1+ (* *psr-1-entry* *path-multiplier*)))
	(t
	 target))))

;;;
;;; AUTONOMY-DRIVER
;;;

(defclass AUTONOMY-DRIVER (actor)
  ((latest-image :accessor driver-latest-image :initarg :image :initform nil)
   (commands-sent :accessor driver-commands-sent :initform 0)
   (post-autonomy-payload-evaluated? :accessor post-autonomy-payload-evaluated? :initform nil)
   ))

(defmethod PRINT-OBJECT ((d AUTONOMY-DRIVER) stream)
  (format stream "{~a: :state ~a" (class-name (class-of d)) (state d))
  (when (driver-latest-image d)
    (format stream " :image-time ~f" (receive-time (driver-latest-image d))))
  (format stream "}"))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (image SENSOR-PAYLOAD))) ;Ignore normal sensor data

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (image POST-AUTONOMY-PAYLOAD) &aux (state (state d)))
  (setf (driver-latest-image d) image)
  (case state
    (:IDLE
     (cond ((hazard-interrupt? image)
	    (setf (state d) :evaluating-hazard)
	    (send d *rover* (make-instance 'send-hazard-payload) :delay *uplink-latency*))
	   (t
	    (let ((decision-time (if (in-psr? (sensor-payload-position image)) *driver-decision-time-night* *driver-decision-time-day*)))
	      (set-state d :evaluating-image)
	      (send d d (make-instance 'post-autonomy-eval) :delay decision-time)))))
    (:EVALUATING-IMAGE
     :EVALUATING-HAZARD)
    (:DRIVING-TO-PSR-EDGE
     (let ((decision-time (if (in-psr? (sensor-payload-position image)) *driver-decision-time-night* *driver-decision-time-day*)))
       (set-state d :evaluating-image)
       (send d d (make-instance 'post-autonomy-eval) :delay decision-time))
     (when (in-psr? (sensor-payload-position image))
       ;; Should get a panorama, but I'm just going to take longer to evaluate it.
       (send d d (make-instance 'authorized-to-enter-psr) :delay *enter-psr-decision-time*)
       (set-state d :waiting-for-authorization-to-enter-psr)))
    (:WAITING-FOR-AUTHORIZATION-TO-ENTER-PSR)
    (:WAITING-FOR-POST-AUTONOMY-EVAL)))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (image HAZARD-EVAL-PAYLOAD))
  (do-later d *hazard-eval-time*
	    (setf (state d) :idle)
	    (send d d (make-instance 'POST-AUTONOMY-PAYLOAD :time-sent (sensor-payload-time-sent image) :position (sensor-payload-position image)) :delay 0)))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (msg POST-AUTONOMY-EVAL) &aux (state (state d)))
  (setf (post-autonomy-payload-evaluated? d) t)
  (case state
    (:idle)
    ((:evaluating-image :waiting-for-post-autonomy-eval)
     (send d d (make-instance 'send-rover-command) :delay 0))
    (:driving-to-psr-edge)
    (:waiting-for-authorization-to-enter-psr)))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (authorization AUTHORIZED-TO-ENTER-PSR) &aux (state (state d)))
  (case state
    (:idle)
    (:evaluating-image)
    (:driving-to-psr-edge)
    (:waiting-for-authorization-to-enter-psr
     (cond ((post-autonomy-payload-evaluated? d)
	    (send d d (make-instance 'send-rover-command) :delay 0))	    
	   (t
	    (set-state d :waiting-for-post-autonomy-eval))))
    (:waiting-for-post-autonomy-eval)))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (msg SEND-ROVER-COMMAND))
  (unless (driver-latest-image d)
    (error "shouldn't get here 1"))
  (setf (post-autonomy-payload-evaluated? d) nil)
  (let* ((target (+ (sensor-payload-position (driver-latest-image d)) *autonomous-waypoint-distance*))
	 (will-enter (will-drive-enter-psr? target (driver-latest-image d))))
    (send d *rover* (make-instance 'waypoint-command
				   :target (if will-enter (clip-drive-to-psr-edge target) target))
	  :delay *uplink-latency*)
    (incf (driver-commands-sent d))
    (cond (will-enter
	   (set-state d :driving-to-psr-edge))
	  (t
	   (set-state d :idle)))))

;;; Not using this
(defmethod TICK ((d AUTONOMY-DRIVER) state) (declare (ignore state)))


;;;
;;; GDS & RTSCI
;;;

(defclass GDS (actor)
  ((total-downlink-bits :accessor gds-total-downlink-bits :initarg :total-downlink :initform 0)))

(defmethod RECEIVE ((g GDS) (msg SENSOR-PAYLOAD))
  ;(format t "gds received ~a~%" msg)
  (incf (gds-total-downlink-bits g) (sensor-payload-bits msg))
  ;; decide whether realtime is involved.
  (send g *realtime* msg :delay *ground-processing-time*))

(defclass REALTIME (actor)
  ((consultation-count :accessor realtime-consultation-count :initform 0)
   (ignoring-images :accessor realtime-ignoring-images :initform nil)))

(defmethod RECEIVE ((r REALTIME) (image SENSOR-PAYLOAD))
  (unless (realtime-ignoring-images r)
    (cond ((requires-consultation *realtime* image)
	   (setf (realtime-ignoring-images r) t)
	   (send r *driver* image :delay *rt-science-consultation-time*)
	   (do-later r
	       *rt-science-consultation-time*
	     (setf (realtime-ignoring-images r) nil)
	     (incf (realtime-consultation-count r))))
	  (t
	   (send r *driver* image)))))

;;; Require a consultation every *rt-science-consultation-rate* meters
(defmethod REQUIRES-CONSULTATION ((r REALTIME) (image SENSOR-PAYLOAD))
  (>= (* (sensor-payload-position image) *rt-science-consultation-rate*) 
      (realtime-consultation-count r)))


;;;
;;; ROVER
;;;

(defclass ROVER (actor)
  ((position :accessor rover-position :initarg :position :initform 0)
   (target :accessor rover-target :initarg :target :initform 0)
   (driving-time :accessor rover-driving-time :initform 0)
   (waiting-time :accessor rover-waiting-time :initform 0)
   (sensor-queue :accessor rover-sensor-queue  :initform nil)
   (default-sensor-payload :accessor rover-default-sensor-payload :initarg :default-sensor-payload :initform *default-sensor-payload*)
   ))

(defmethod RECEIVE ((r ROVER) (msg WAKEUP-SENSORS))
  (let* ((pld (make-instance (next-sensor-payload-type r) :position (rover-position r) :time-sent *time*))
	 (transmission-time (time-to-downlink-sensor-payload pld))
	 (downlink-latency (sensor-payload-latency pld)))
    (do-later r
	(sensor-payload-sampling-delay pld) ;delay by the time it takes to capture the sensor data.  Always 0 for now.
	(send r *gds* pld :delay downlink-latency)
	(send r r (make-instance 'wakeup-sensors) :delay (max transmission-time 1.0)))))

(defmethod RECEIVE ((r ROVER) (msg DRIVE-COMMAND))
  (let ((delta (max 0 (- (drive-command-target msg) (rover-position r)))))
    (cond ((> delta 0)
	   (set-state r :driving)
	   (setf (rover-target r) (drive-command-target msg))
	   (send r r (make-instance 'wakeup) :delay (/ delta *driving-speed*))) ;wakeup when we expect to get there
	  (t
	   (set-state r :waiting)))))

(defmethod TICK ((r ROVER) state &aux delta)
  (unless (eql 0 (setq delta (- *time* (last-tick r))))
    (case state
      (:driving
       (incf (rover-driving-time r) delta)
       (incf (rover-position r) (* delta *driving-speed*))
       (when (>= (rover-position r) (rover-target r))
	 (set-state r :waiting)))
      (:waiting
       (incf (rover-waiting-time r) delta)))))

(defmethod NEXT-SENSOR-PAYLOAD-TYPE ((r ROVER))
  (if (null (rover-sensor-queue r))
      (rover-default-sensor-payload r)
      (pop (rover-sensor-queue r))))

(defmethod PRINT-OBJECT ((r ROVER) stream)
  (format stream "{~a: :state ~a :pos ~f :target ~f}" (class-name (class-of r)) (state r) (rover-position r) (rover-target r)))

(defmethod RECEIVE ((r ROVER) (msg SEND-HAZARD-PAYLOAD))
  (push 'hazard-eval-payload (rover-sensor-queue r)))

;;;
;;; Autonomous Rover
;;;

(defclass AUTONOMOUS-ROVER (rover)
  ((waypoint :accessor rover-waypoint :initform nil)))	;if not nil, then we're driving autonomously

;; This doesn't send all of the images down.  Reconsider this NOTE!!!!!!!!!!!!
(defmethod RECEIVE ((r AUTONOMOUS-ROVER) (msg WAKEUP-SENSORS))
  (let* ((pld (make-instance (next-sensor-payload-type r) :position (rover-position r) :time-sent *time*))
	 (transmission-time (time-to-downlink-sensor-payload pld))
	 (downlink-latency (sensor-payload-latency pld)))
    (do-later r
	(sensor-payload-sampling-delay pld) ;delay by the time it takes to capture the sensor data.  Always 0 for now.
      (when (not (typep pld 'navcam-payload))
	(send r *gds* pld :delay downlink-latency))
      (send r r (make-instance 'wakeup-sensors) :delay (max transmission-time 1.0)))))

(defmethod RECEIVE ((r AUTONOMOUS-ROVER) (msg WAKEUP-AUTONOMY))
  (when (rover-waypoint r)
    (send r r (make-instance 'navcam-payload :position (rover-position r) :time-sent *time*) :delay *onboard-processing-time*)))

(defmethod RECEIVE ((r AUTONOMOUS-ROVER) (msg NAVCAM-PAYLOAD))
  (when (rover-waypoint r)
    (send r r (make-instance 'drive-command :target (+ (sensor-payload-position msg) *length-of-autonomous-traverse*))
	  :delay 0)
    (send r r (make-instance 'wakeup-autonomy) :delay 0)))

(defmethod RECEIVE ((r AUTONOMOUS-ROVER) (msg WAYPOINT-COMMAND))
  (setf (rover-waypoint r) (drive-command-target msg))
  (send r r (make-instance 'wakeup-autonomy) :delay 0))

(defmethod TICK :after ((r AUTONOMOUS-ROVER) state)
  (declare (ignore state))
  (when (and (rover-waypoint r)
	     (>= (rover-position r) (rover-waypoint r)))
    (setf (rover-waypoint r) nil)
    (let ((pld (make-instance 'post-autonomy-payload :position (rover-position r))))
      (send r *driver* pld :delay (sensor-payload-latency pld))))) ;straight to the driver.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interruptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *HAZARDS* '())
(defvar *BUMPER-INTERRUPTS* '())
(defvar *HAZARD-ENCOUNTER* '())

(defun GENERATE-HAZARDS ()
  (let* ((d (* *driving-distance* *path-multiplier*))
	 (num-hazards (floor (* d *hazard-trigger-rate*)))
	 (num-bumps (floor (* d *bumper-trigger-rate*)))
	 (num-hazard-encounter (floor (* d *hazard-encounter-trigger-rate*))))
    (setq *hazards* (sort (loop for i from 1 to num-hazards collect (random d)) #'<))
    (setq *bumper-interrupts* (sort (loop for i from 1 to num-bumps collect (random d)) #'<))
    (setq *hazard-encounter* (sort (loop for i from 1 to num-hazard-encounter collect (random d)) #'<))
    (trace-when 1
		(format t "  num-hazards=~s num-bumps=~s~%" num-hazards num-bumps)
		(format t "  *hazards*=~s~%" *hazards*)
		(format t "  *bumper-interrupts*=~s~%" *bumper-interrupts*)
		(format t "  *hazard-encounter*=~s~%" *hazard-encounter*)
		(finish-output nil))
    ))

(defun HAZARD-INTERRUPT? (sensor-payload)
  (cond ((null *hazards*) nil)
	((>= (sensor-payload-position sensor-payload) (first *hazards*))
	 (pop *hazards*))
	(t nil)))

(defun BUMPER-INTERRUPT? ()
  (cond ((null *bumper-interrupts*) nil)
	((>= (rover-position *rover*) (first *bumper-interrupts*))
	 (pop *bumper-interrupts*))
	(t nil)))

(defun HAZARD-ENCOUNTER-INTERRUPT? (sensor-payload)
  (cond ((null *hazard-encounter*) nil)
	((>= (sensor-payload-position sensor-payload) (first *hazard-encounter*))
	 (pop *hazard-encounter*))
	(t nil)))

(defun CHECK-FOR-INTERRUPTION ()
  (when (bumper-interrupt?)
    (send *rover* *rover* (make-instance 'BUMPER-INTERRUPT))))

(defclass HAZARD-INTERRUPT (message)
  ((hazard-position :accessor hazard-position :initarg :position)))

(defclass BUMPER-INTERRUPT (message)
  ((bump-position :accessor bump-position :initarg :position)))

(defmethod RECEIVE ((r ROVER) (msg BUMPER-INTERRUPT))
  (abandon-plans *events* r)
  (set-state r :bumper-halt)
  (let ((m (make-instance 'bumper-eval-payload :position (rover-position r))))
    (send r *gds* m :delay (sensor-payload-latency m))
    (do-later r (time-to-downlink-sensor-payload m) (set-state r :idle))
    (send r r (make-instance 'wakeup-sensors) :delay (+ (time-to-downlink-sensor-payload m) 0.1))))

(defmethod RECEIVE ((r AUTONOMOUS-ROVER) (msg BUMPER-INTERRUPT))
  (abandon-plans *events* r)
  (set-state r :bumper-halt)
  (let ((m (make-instance 'bumper-eval-payload :position (rover-position r))))
    (send r *gds* m :delay (sensor-payload-latency m))
    (do-later r (time-to-downlink-sensor-payload m) (set-state r :idle))
    (send r r (make-instance 'wakeup-sensors) :delay (+ (time-to-downlink-sensor-payload m) 0.1))))

(defmethod RECEIVE ((g GDS) (msg BUMPER-EVAL-PAYLOAD))
  ;(format t "gds received ~a~%" msg)
  (incf (gds-total-downlink-bits g) (sensor-payload-bits msg)) ;overrides method where msg is SENSOR-PAYLOAD
  (send g *driver* msg :delay *ground-processing-time*))

(defmethod RECEIVE ((d DRIVER) (msg BUMPER-EVAL-PAYLOAD))
  (set-state d :bumper-halt)
  (abandon-plans *events* d)
  (setf (driver-image-eval-target-time d) (+ *time* *bumper-eval-time*))
  (setf (driver-image-under-evaluation d) msg)
  (setf (driver-image d) msg)
  (send d d (make-instance 'wakeup) :delay *bumper-eval-time*))

(defmethod RECEIVE ((d AUTONOMY-DRIVER) (msg BUMPER-EVAL-PAYLOAD))
  (set-state d :bumper-halt)
  (abandon-plans *events* d)
  (setf (driver-latest-image d) msg)
  (send d d (make-instance 'send-rover-command) :delay *bumper-eval-time*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object properties based on rover position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Is this position in one of the two PSRs?
(defmethod IN-PSR? (position)
  (let ((d (/ position *path-multiplier*)))
    (or (and (>= d *psr-1-entry*)
	     (<= d (+ *psr-1-entry* *psr-1-length*)))
	(and (>= d *psr-2-entry*)
	     (<= d (+ *psr-2-entry* *psr-2-length*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model Top Level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun RUN-MODEL ()
  (let ((*events* (make-empty-queue #'receive-time))
	(*drive-distance-along-path* (* *driving-distance* *path-multiplier*))
	(*hazards* '())
	(*bumper-interrupts* '())
	(*time* 0)
	(*rover* nil)
	(*driver* nil)
	(*realtime* nil)
	(*gds* nil))
    (generate-hazards)
    ;; Preparation for running the discrete event simulation
    (cond ((eql *driving-method* :autonomous)
	   (setf *rover* (make-instance 'autonomous-rover :state :waiting :default-sensor-payload *default-sensor-payload*))
	   (push 'post-autonomy-payload (rover-sensor-queue *rover*))
	   (setf *driver* (make-instance 'autonomy-driver :state :idle)))
	  (t
	   (setq *rover* (make-instance 'rover :state :waiting :default-sensor-payload *default-sensor-payload*))
	   (setf *driver* (make-instance 'driver :state :idle))))
    (setf *realtime* (make-instance 'realtime))
    (setf *gds* (make-instance 'gds))
    (send *rover* *rover* (make-instance 'wakeup-sensors))
    (run-simulation :terminate (if (fboundp 'termination-condition)
				   (symbol-function 'termination-condition)
				   #'(lambda () (or (>= (rover-position *rover*) *drive-distance-along-path*)
						    (>= (rover-position *rover*) *simulation-distance-cutoff*)
						    (>= *time* *simulation-time-cutoff*))))
		    :interruption 'check-for-interruption)
    (setf *time-to-drive-scenario* (/ *time* 60.0))
    (setf *speed-made-good* (/ *driving-distance* *time*))
    (setf *speed-made-good-along-path* (/ (rover-position *rover*) *time*))
    (setf *rover-driving-time* (rover-driving-time *rover*))
    (setf *rover-waiting-time* (rover-waiting-time *rover*))
    (setf *duty-cycle* (/ (rover-driving-time *rover*) (+ (rover-driving-time *rover*) (rover-waiting-time *rover*))))
    (setf *time-to-drive-scenario* (/ *time* 3600.0))
    (setf *average-cycle-time* (if (zerop (driver-commands-sent *driver*)) 'invalid (/ *time* (driver-commands-sent *driver*))))
    (setf *final-rover-position* (rover-position *rover*))
    (setf *total-downlink* (/ (gds-total-downlink-bits *gds*) (* 8.0 1000000)))
    (setf *final-time* *time*)
    *speed-made-good-along-path*))

(defun PRINT-MODEL ()
  (run-model)
  (format t "Model Inputs:~%")
  (print-variable-value-table (sorted-variables-of-type :input))
  (format t "~%Model outputs~%")
  (print-variable-value-table (sorted-variables-of-type :output)))
