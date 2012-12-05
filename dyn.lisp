(in-package #:sandalphon)

;;; dyn returns two values, the dynamic env for after that code
;;; and the annotated code object


(defun progn (dyn &rest forms)
  (if (null (rest forms))
      (dyn dyn (first forms))
      (multiple-value-bind (dyn code)
	  (dyn dyn (first forms))
	`(progn ,code ,(dyn dyn `(progn ,@(rest forms)))))))

(defun 
