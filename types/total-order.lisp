;;;; total-order.lisp

;;;; utility for defining totally ordered enumerated sets

(in-package #:sandalphon.types)

(defmacro define-total-order (order (&rest clauses) &key get (test '#'eql))
  `(let ((memo (list ,@(mapcar (curry #'list 'quote) clauses))))
     ,@(when get
	     (list `(defun ,get () memo)))
     (defun ,order (low high)
       (if (find (cons low high) memo :test #'equal)
	   t
	   (dolist (poss (remove low memo :key #'car :test-not ,test) nil)
	     (when (,order (cdr poss) high)
	       (setf memo (acons low high memo))
	       (return t)))))))

(define-total-order number<
    ((integer . rational)
     (ratio . rational)
     (short-float . float)
     (single-float . float)
     (double-float . float)
     (long-float . float)
     (float . real)
     (rational . real)
     (real . number))
  :get get-number-order)
