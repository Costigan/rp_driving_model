;;;
;;; Simple regression testing package 
;;; 

(defvar *REGRESSION-TESTS* '())
(defvar *SUCCEEDED* 0)
(defvar *FAILED* 0)

(defclass REGRESSION-TEST ()
  ((name :initarg :name :accessor regression-test-name)
   (form :initarg :form :accessor regression-test-form :initform nil)
   (value :initarg :value :accessor regression-test-value :initform nil)
   (output :initarg :output :accessor regression-test-output :initform nil)))

(defmethod PRINT-OBJECT ((self regression-test) stream)
  (format stream "#<regression-test ~s>" (regression-test-name self)))

;; Don't handle regression-test-output yet
(defmethod RUN-REGRESSION-TEST ((self REGRESSION-TEST) &key verbose)
  (catch 'RUN-REGRESSION-TEST
    (let (result error-flag output)
      (format t "Running ~s ..." (regression-test-name self))
      (setq output (with-output-to-string (*standard-output*)
		     (setq result
			   (handler-case
			       (eval (regression-test-form self))
			     (error (err)
			       (setq error-flag t)
			       (setq result err)
			       (throw 'RUN-REGRESSION-TEST nil))))))
      (cond (error-flag
	     (format t " errored.~%")
	     (when verbose 
	       (format t "  The test form was ~s" (regression-test-form self))
	       (format t "  Got error ~s while running test" result)
	       (describe result)))
	    ((not (equal result (regression-test-value self)))
	     (format t " failed.~%")
	     (when verbose
	       (format t "  The form was ~s~%" (regression-test-form self))
	       (format t "  Got wrong value:~%")
	       (pprint result)
	       (format t "~&  Should be:~%")
	       (pprint (regression-test-value self))
	       (terpri))
	     nil)
	    ((and (regression-test-output self) 
		  (not (string-equal (regression-test-output self) output)))
	     (format t " failed.~%")
	     (when verbose
	       (format t "  The form was ~s~%" (regression-test-form self))
	       (format t "  Got wrong output:~%")
	       (pprint output)
	       (format t "~&  Should be:~%")
	       (pprint (regression-test-output self))
	       (terpri)))
	    (t
	     (format t " succeeded.~%")	     
	     t)))))

(defmacro DEFINE-TEST (name form value &optional output)
  (let ((test-name (intern (format nil "TEST-~a" name))))
    `(eval-when (compile load eval)
       (let ((test (make-instance 'regression-test
				  :name ',test-name
				  :form ,form
				  :value ,value
				  :output ,output)))
	 (declare (special ,test-name))
	 (setq *regression-tests*
	       (remove-if #'(lambda (a) 
			      (and (typep a 'regression-test)
				   (eq ',test-name (regression-test-name a))))
			  *regression-tests*))	  
	 (setq *regression-tests*
	       (nconc *regression-tests* (list test)))
	 (setq ,test-name test)))))

;;; The same as before except that the arguments are quoted
(defmacro DEFINE-TESTQ (name form value &optional output)
  `(define-test ,name ',form ',value ',output))

(defmacro DEFINE-ASSERT-TEST (name &body body)
  `(define-test ,name '(progn ,@body nil) nil nil))

(defun RUN-ALL-TESTS (&key verbose)
  (let ((succeeded 0)
	(failed 0)
	(total (length *regression-tests*)))
    (loop for test in *regression-tests*
	  do (progn
	       (cond ((ignore-errors (run-regression-test test :verbose verbose))
		      (incf succeeded))
		     (t
		      (incf failed)))))
    (unless (eql total (+ succeeded failed))
      (error "internal error in run-all-tests"))
    (if (eql succeeded total)
	(format t "~%SUCCESS.~%")
	(format t "~%FAILURE.~%"))
    (format t "~d tests succeeded.~%~d tests failed.~%" succeeded failed)
    (values)))

(setq *REGRESSION-TESTS* '())

(define-test BASIC-1 '(+ 1 2) 3)


(provide :regression)
