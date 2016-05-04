# rp_driving_model
Simple discrete-event model to estimate Resource Prospector driving speed in different sensor configurations

# Documentation for the discrete event simulator itself

Files:
  Currently used:
    regression.lisp           ;; regression test library
    table-printer.lisp        ;; columnar printing
    simple-queue.lisp         ;; List-based priority queue (allows queued events to be removed easily)
    tests.lisp                ;; Current regression tests (not much yet)
    sim.lisp                  ;; Foundation of the discrete event simulator
    model.lisp                ;; The model itself
    cases.lisp                ;; The cases in the spreadsheet
    monte-carlo.lisp          ;; Code for exploring the trade space
    t                         ;; Load everything, e.g., (load "t")

  Not used:
    poisson.lisp              ;; Initial code for  a poisson distribution (not used)
    warn-patch.lisp           ;; A small patch for Corman Common Lisp (not used any more)
    queue.lisp                ;; heap-based priority queue (not used)


Installation notes:

  I switched to Clozuer CL (http://ccl.clozure.com/) from Corman
  Common Lisp. (It's bad luck that they're both called ccl for
  short.)  The mac and linux versions are the most stable.  The
  windows version (which I'm using) works fine, mostly, but I've
  seen issues with threads.  (The compiler they're using under
  windows is doing some optimization that breaks the invariants
  their code is supposed to have when threadded.  I haven't looked
  at building ccl without that.  I haven't seen an issue with that
  under linux, though.)

  There's no installation for the simulator itself.  Just have the
  files in a directory.

Running it:

I run lisp, then enter (load "t") to load the various files, then
run cases.  Here's a transcript:

    Welcome to Clozure Common Lisp Version 1.10-r16196  (WindowsX8664)!

    CCL is developed and maintained by Clozure Associates. For more information
    about CCL visit http://ccl.clozure.com.  To enquire about Clozure's Common Lisp
    consulting services e-mail info@clozure.com or visit http://www.clozure.com.

    ? (load"t")
    Running TEST-BASIC-1 ... succeeded.
    Running TEST-QUEUE-1 ... succeeded.
    Running TEST-QUEUE-2 ... succeeded.
    Running TEST-QUEUE-3 ... succeeded.
    Running TEST-QUEUE-4 ... succeeded.

    SUCCESS.
    5 tests succeeded.
    0 tests failed.
    "c:/RP/Projects/DriveModel/lisp/t"
    ? (compare-all-models)
    Variable                        LIDARS-GALORE SINK-HIGH-LAT BAL-LOW-BAND  BALANCED       KITCHEN-SINK   BASELINE       SINK-LOW-BAND  BAL-HIGH-LAT   BUMPER-CAR     
    *AUTONOMOUS-WAYPOINT-DISTANCE*            50            50            50            50             50             50             50             50             50   
    *AVERAGE-CYCLE-TIME*                 1.29179    1540.46520       9.16962      34.15530     1813.50520       68.84398     3521.54520       79.13920      137.42088   
    *BELLY-BITS*                        12582912      12582912             0      12582912       12582912              0       12582912       12582912              0   
    *BELLY-PAYLOAD*                      3145728       3145728             0       3145728        3145728              0       12582912        3145728              0   
    *BUMPER-BITS*                           1280          2560          1280          1280           1280           1280           1280           1280           1280   
    *BUMPER-EVAL-PAYLOAD*               14155776     216662016     163577856     213909504      213909504      276824064       18984960      239075328      276824064   
    *BUMPER-EVAL-TIME*                       400            60           150           150            150            300            150            400            500   
    *BUMPER-PAYLOAD*                        1280          2560          1280          1280           1280           1280           1280           1280           1280   
    *BUMPER-TRIGGER-RATE*                0.00200       0.00200       0.00250       0.00400        0.00800        0.00800        0.00200        0.00800        0.00800   
    *DEFAULT-SENSOR-PAYLOAD*        LIDAR-PAYLOAD LIDAR-PAYLOAD LIDAR-PAYLOAD NAVCAM-PAYLOAD NAVCAM-PAYLOAD NAVCAM-PAYLOAD NAVCAM-PAYLOAD NAVCAM-PAYLOAD NAVCAM-PAYLOAD 
    *DESIGN-CASE*                              4             6             9             2              3              1              8              5              7   
    *DESIGN-CASE-NAME*              LIDARS-GALORE SINK-HIGH-LAT BAL-LOW-BAND      BALANCED   KITCHEN-SINK       BASELINE   SINK-LOW-BAND  BAL-HIGH-LAT     BUMPER-CAR   
    *DOWNLINK-LATENCY*                        10           120            10            10             10             10             10            120            120   
    *DOWNLINK-RATE*                       400000        400000         60000        400000         400000         400000          60000         400000         400000   
    *DRIVE-DISTANCE-ALONG-PATH*                0             0             0             0              0              0              0              0              0   
    *DRIVER-DECISION-TIME-DAY*                 1             1             1            15             15             30             30             45             45   
    *DRIVER-DECISION-TIME-NIGHT*               1             1             1            30             30             45             45             60             60   
    *DRIVING-METHOD*                  CONTINUOUS    AUTONOMOUS    CONTINUOUS    CONTINUOUS     AUTONOMOUS    STOP-AND-GO     AUTONOMOUS    STOP-AND-GO     CONTINUOUS   
    *DUTY-CYCLE*                         0.84554       0.33331       0.37558       0.35368        0.26718        0.27757        0.14422        0.09178        0.07449   
    *ENTER-PSR-DECISION-TIME*                 60            60            60            60             60             60             60             60             60   
    *FINAL-ROVER-POSITION*            5000.03660    3000.07620    3000.08280    4000.00680     4000.06000     5000.19780     3000.22240     4000.08760     6000.15770   
    *FINAL-TIME*                     59465.01600   93968.37500   99627.92000  121285.46000   174096.50000   208597.27000   211292.70000   456870.60000   841290.70000   
    *GROUND-PROCESSING-TIME*                   2             2             2             2              2              2              2              2              2   
    *HAZARD-EVAL-PAYLOAD*               50331648     216662016     100663296      75497472       75497472      100663296      101105664      125829120      276824064   
    *HAZARD-EVAL-TIME*                        90           225           105           120            120            120            120            240            240   
    *HAZARD-TRIGGER-RATE*                0.00500       0.02000       0.01000       0.04000        0.04000        0.04000        0.02000        0.05000        0.08000   
    *HAZCAM-BITS*                         393216       3145728       3145728       3145728        3145728       12582912        3201024        3145728       12582912   
    *HAZCAM-PAYLOAD*                      196608       1572864       1572864       1572864        1572864        6291456        1600512         786432        6291456   
    *LENGTH-OF-AUTONOMOUS-TRAVERSE*            0             8             0             0        4.50000              0        4.50000              0              0   
    *LIDAR-BITS*                          393216        393216        262144             0              0              0              0         393216              0   
    *LIDAR-PAYLOAD*                       196608        196608        131072             0              0              0              0              0              0   
    *LOOKAHEAD-DISTANCE-DAY*                  11             8             9       4.50000        4.50000        4.50000        4.50000        4.50000        4.50000   
    *LOOKAHEAD-DISTANCE-NIGHT*                11             8             9       4.50000        4.50000        4.50000        4.50000        4.50000        4.50000   
    *NAVCAM-BITS*                       12582912      12582912      12582912      12582912       12582912       12582912       12582912       12582912       12582912   
    *NAVCAM-PAYLOAD*                     3145728       4194304       3145728       3145728        3145728        3145728        2097152        3145728        6291456   
    *ONBOARD-PROCESSING-TIME*                  0            20             0             0             55              0             55              0              0   
    *PATH-MULTIPLIER*                          5             3             3             4              4              5              3              4              6   
    *POST-AUTONOMY-PAYLOAD*             25165824      25165824             0      25165824       25165824       25165824       25165824       25165824       25165824   
    *ROVER-DRIVING-TIME*             49974.08000   29995.81400   30000.14500   40002.57400    40001.68400    50002.76600    30002.14800    40000.98400    60000.11700   
    *ROVER-WAITING-TIME*              9129.03600   59996.89500   49875.88700   73100.01600   109717.99000   130140.63000   178030.05000   395813.94000   745494.10000   
    *RT-SCIENCE-CONSULTATION-RATE*       0.01000       0.01000       0.01000       0.01000        0.01000        0.01000        0.01000        0.01000        0.01000   
    *RT-SCIENCE-CONSULTATION-TIME*            60           180            60            60             60             60             60            180            180   
    *SPEED-MADE-GOOD*                    0.01682       0.01064       0.01004       0.00825        0.00574        0.00479        0.00473        0.00219        0.00119   
    *SPEED-MADE-GOOD-ALONG-PATH*         0.08408       0.03193       0.03011       0.03298        0.02298        0.02397        0.01420        0.00876        0.00713   
    *TIME-TO-DRIVE-SCENARIO*            16.51806      26.10233      27.67442      33.69041       48.36014       57.94368       58.69242      126.90850      233.69186   
    *UPLINK-LATENCY*                          10            10            10            10             10             10             10            120             10   

    Speed made good bar chart
	    Speed made good
    LIDARS-GALORE	     0.01682
    SINK-HIGH-LAT	     0.01064
    BAL-LOW-BAND	     0.01004
    BALANCED	     0.00825
    KITCHEN-SINK	     0.00574
    BASELINE	     0.00479
    SINK-LOW-BAND	     0.00473
    BAL-HIGH-LAT	     0.00219
    BUMPER-CAR	     0.00119

    Speed made good along path bar chart
	    Speed made good along path
    LIDARS-GALORE	     0.08408
    SINK-HIGH-LAT	     0.03193
    BAL-LOW-BAND	     0.03011
    BALANCED	     0.03298
    KITCHEN-SINK	     0.02298
    BASELINE	     0.02397
    SINK-LOW-BAND	     0.01420
    BAL-HIGH-LAT	     0.00876
    BUMPER-CAR	     0.00713
    NIL
    ? 

This runs the cases serially.  There's a (PCOMPARE-ALL-MODELS) that
runs them in parallel, and this has always worked.  But I've seen
ccl crashes when doing larger parallel runs.  Again, under linux,
this shouldn't be a problem.

In monte-carlo.lisp, there is a series of functions whose name start
with EXPLORE-.  These are the for looking at sensitivity.  They
generate files that look like this:


    (*AUTONOMOUS-WAYPOINT-DISTANCE* *AVERAGE-CYCLE-TIME* *BELLY-BITS* *BELLY-PAYLOAD* *BUMPER-BITS* *BUMPER-EVAL-PAYLOAD* *BUMPER-EVAL-TIME* *BUMPER-PAYLOAD* *BUMPER-TRIGGER-RATE* *DEFAULT-SENSOR-PAYLOAD* *DESIGN-CASE* *DESIGN-CASE-NAME* *DOWNLINK-LATENCY* *DOWNLINK-RATE* *DRIVE-DISTANCE-ALONG-PATH* *DRIVER-DECISION-TIME-DAY* *DRIVER-DECISION-TIME-NIGHT* *DRIVING-METHOD* *DUTY-CYCLE* *ENTER-PSR-DECISION-TIME* *FINAL-ROVER-POSITION* *FINAL-TIME* *GROUND-PROCESSING-TIME* *HAZARD-EVAL-PAYLOAD* *HAZARD-EVAL-TIME* *HAZARD-TRIGGER-RATE* *HAZCAM-BITS* *HAZCAM-PAYLOAD* *LENGTH-OF-AUTONOMOUS-TRAVERSE* *LIDAR-BITS* *LIDAR-PAYLOAD* *LOOKAHEAD-DISTANCE-DAY* *LOOKAHEAD-DISTANCE-NIGHT* *NAVCAM-BITS* *NAVCAM-PAYLOAD* *ONBOARD-PROCESSING-TIME* *PATH-MULTIPLIER* *POST-AUTONOMY-PAYLOAD* *ROVER-DRIVING-TIME* *ROVER-WAITING-TIME* *RT-SCIENCE-CONSULTATION-RATE* *RT-SCIENCE-CONSULTATION-TIME* *SPEED-MADE-GOOD* *SPEED-MADE-GOOD-ALONG-PATH* *TIME-TO-DRIVE-SCENARIO* *UPLINK-LATENCY*) 
    NIL 
    100 
    100 
    (50 82.57664 0 0 1280 276824064 300 1280 0.008 NAVCAM-PAYLOAD 1 BASELINE 10 400000 0 30 45 :STOP-AND-GO 0.22953111 60 5000.298 245995.81 2 100663296 300 0.04 12582912 6291456 0 0 0 4.5 4.5 12582912 3145728 0 5 25165824 50003.516 167847.2 0.01 60 0.00406511 0.02032676 68.33217 10) 
    (50 79.13224 0 0 1280 276824064 300 1280 0.008 NAVCAM-PAYLOAD 1 BASELINE 10 400000 0 30 45 :STOP-AND-GO 0.23640124 60 5000.529 239691.56 2 100663296 300 0.04 12582912 6291456 0 0 0 4.5 4.5 12582912 3145728 0 5 25165824 50005.984 161524.14 0.01 60 0.0041720285 0.020862348 66.580986 10) 
    (50 80.15508 0 0 1280 276824064 300 1280 0.008 NAVCAM-PAYLOAD 1 BASELINE 10 400000 0 30 45 :STOP-AND-GO 0.23564501 60 5000.4033 240866.03 2 100663296 300 0.04 12582912 6291456 0 0 0 4.5 4.5 12582912 3145728 0 5 25165824 50004.715 162198.86 0.01 60 0.0041516856 0.020760102 66.907234 10)
    ...
    for 97 more lines.

This file contains all model variables (inputs plus outputs) in
alphabetic order, and a final state 'vector' for each of a set of
runs that vary in some way.  My program for reading and plotting
these files is in c#, and can run under linux (mono), but I haven't
tried it.  I can send that too if it would be useful.


# Documentation for the results plotter

<todo>
