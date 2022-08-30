;(load "warn-patch.lisp")
(load "regression.lisp")
(load "table-printer.lisp")
(load "simple-queue.lisp")
(load "tests.lisp")
(load "sim.lisp")
(load "model.lisp")
(load "cases.lisp")
(load "mc-low-band.lisp")
(run-all-tests)

;(explore-lookahead)
(explore-hazard-trigger)
(explore-bumper-trigger)
;(explore-hazard-eval)
;(explore-bumper-eval)

;(explore-decsion-time-day)
;(explore-decsion-time-night)
