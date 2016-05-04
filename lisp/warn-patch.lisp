;;;
;;; Trying to work around a crash that is common in the debugger
;;;

(defvar WARN-IS-RECURSIVE)

(defun CORMANLISP:WARN (string &rest args)
  (if (boundp 'warn-is-recursive)
      (format *error-output* ";;; Warning: recursive warning")
      (let ((warn-is-recursive t))
	(format *error-output* ";;; Warning: ~A~%" (apply 'format nil string args)))))
