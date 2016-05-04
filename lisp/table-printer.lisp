;;;
;;; Table Printer
;;;
;;; Look at these macros:
;;; PRINTING-TABLE
;;; PRINTING-TABLE-HEADER
;;; PRINTING-TABLE-ROW
;;; PRINTING-TABLE-CELL

(defclass TABLE-PRINTER ()
  (
   ;; List of strings (in backwards order)
   (headers :accessor table-printer-headers :initarg :headers :initform '())

   ;; List of list of strings
   (rows :accessor table-printer-rows :initarg :rows :initform '())

   ;; List of integers
   (widths :accessor table-printer-widths :initarg :widths :initform '())
   ))

(defvar *TABLE-PRINTER-ROW*)
(defvar *TABLE-PRINTER*)
(defvar *TABLE-CELL-JUSTIFICATION* :LEFT) ; :left, :right

(defmacro PRINTING-TABLE (&rest body)
  `(let ((*table-printer* (make-instance 'table-printer)))
    (prog1 
	(progn ,@body)
      (prepare-and-print-table *table-printer*))))

(defmacro PRINTING-TABLE-HEADER ((&key (justification '*table-cell-justification*)) 
				 &body body)
  `(let ((*table-printer-row* '())
	 (*table-cell-justification* ,justification))
    (prog1 
	(progn ,@body)
      (setf (table-printer-headers *table-printer*) *table-printer-row*))))

(defmacro PRINTING-TABLE-CELL (&body body)
  `(let ((table-printing-value (with-output-to-string (*standard-output*) ,@body)))
    (push (coerce-to-string table-printing-value) *table-printer-row*)))

(defmacro PRINTING-TABLE-ROW ((&key (justification '*table-cell-justification*))
			      &body body)
  `(let ((*table-printer-row* '())
	 (*table-cell-justification* ,justification))
    (prog1
	(progn ,@body)
      (push *table-printer-row* (table-printer-rows *table-printer*)))))
  
(defmethod PREPARE-AND-PRINT-TABLE ((tp TABLE-PRINTER) &optional (stream t))
  (compute-widths tp)
  (normalize-row-widths tp)
  (reverse-rows-and-columns tp)
  (print-table tp stream))

(defun COERCE-TO-STRING (value)
  (cond ((typep value 'string)
	 value)
	(t
	 (with-output-to-string (stream)
	   (princ value stream)))))

(defmethod COMPUTE-WIDTHS ((tp TABLE-PRINTER))
  ;;(format t "COMPUTE-WIDTHS:~%  headers=~s~%" (table-printer-headers tp))
  ;;(dolist (row (table-printer-rows tp))
  ;;  (format t "  row=~S~%" row))
  (let ((widths (mapcar #'length (table-printer-headers tp))))
    (loop for row in (table-printer-rows tp)
	  for rwidths = (mapcar #'length row)
	  do (setq widths (mapcar #'max widths rwidths)))
    ;;(format t "  widths=~S~%" widths)
    (setf (table-printer-widths tp) widths))
  )

(defmethod NORMALIZE-ROW-WIDTHS ((tp TABLE-PRINTER))
  ;;(format t "NORMALIZE-ROW-WIDTHS~%")
  (let ((len (length (table-printer-headers tp)))
	(newrows))
    (setq newrows
	  (loop for row in (table-printer-rows tp)
		for rlen = (length row)
		collect (cond ((eql len rlen)
			       row)
			      ((< len rlen)
			       (nthcdr (- rlen len) row))
			      (t
			       (append (make-list (- len rlen)) row)))))
    ;;(format t " normalize~%  old=~s~%  new=~s~%" 
    ;; (table-printer-rows tp)
    ;; newrows)
    (setf (table-printer-rows tp) newrows)
    ))

(defmethod REVERSE-ROWS-AND-COLUMNS ((tp TABLE-PRINTER))
  ;;(format t "REVERSE-ROWS-AND-COLUMNS~%")
  (setf (table-printer-headers tp)
	(reverse (table-printer-headers tp)))
  (setf (table-printer-rows tp)
	(loop for row in (reverse (table-printer-rows tp))
	      collect (reverse row)))
  (setf (table-printer-widths tp)
	(reverse (table-printer-widths tp)))
  )

(defmethod PRINT-TABLE ((tp TABLE-PRINTER) &optional (stream t))
  (print-row tp (table-printer-headers tp) (table-printer-widths tp) stream)
  (loop for row in (table-printer-rows tp)
	do (print-row tp row (table-printer-widths tp) stream)))

(defmethod PRINT-ROW ((tp TABLE-PRINTER) row widths stream)
  (loop for cell in row
	for width in widths
	do (format stream "~vA " width cell))
  (terpri stream))

(defun TEST-TABLE-PRINTER-1 ()
  (printing-table
   (printing-table-header ()
      (dolist (header '("header1" "header2"))
      (printing-table-cell (princ header))))
   (dotimes (row 15)
     (printing-table-row ()
      (loop for column from 1 to 2 do
	    (printing-table-cell
	     (format t "(~d x ~s)" row column)))))))
