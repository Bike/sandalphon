;;;; trivalogic.lisp

;;;; trivalent logic, subtypep style
;;;; subtypep returns two values.
;;;; T and T means true, NIL and T means false, NIL and NIL means unknown, and T and NIL is disallowed.
;;;; a bit awkward, but it works.
;;;; pretty standard true/false/unknown logic.  this implements some operations with them, analogous to CL and/or/etc.

(in-package #:sandalphon.types)

(defmacro tri/if (test then else &optional (other '(values nil nil)))
  (with-gensyms (truth surety)
    `(multiple-value-bind (,truth ,surety) ,test
       (cond ((not ,surety) ,other)
	     (,truth ,then)
	     (t ,else)))))

(defmacro tri/not (values)
  ;; has to be a macro because of how values work
  `(multiple-value-bind (sub ok)
       ,values
     (if ok
	 (values (not sub) ok)
	 (values nil nil))))

(defmacro tri/and (&rest forms)
  (cond ((endp forms) '(values t t))
	;; preserve non-toplevelness.  (why not?)
	((endp (rest forms)) `(the (values t t &optional) ,(first forms)))
	(t `(tri/if ,(first forms)
		    (tri/and ,@(rest forms))
		    (values nil t)
		    (values nil nil)))))

;;; this (short-circuiting trivalent or) is unreasonably difficult for me to straighten out in my head.
;;; (tri/or t ...) => t
;;; (tri/or f ...) => (tri/or ...)
;;; (tri/or u ...) => (if (tri/or ...) t u)
;;; except that the t value has to be preserved because lolgeneralizedbooleans,
;;; and I'd rather not have two of the same tri/or form, so

(defmacro tri/or (&rest forms)
  (cond ((endp forms) '(values nil t))
	((endp (rest forms)) `(the (values t t &optional) ,(first forms)))
	(t (with-gensyms (truth surety surety2)
	     ;; FIXME: This is pretty gross.
	     `(multiple-value-bind (,truth ,surety) ,(first forms)
		(if ,truth
		    (values ,truth ,surety)
		    (multiple-value-bind (,truth ,surety2) (tri/or ,@(rest forms))
		      (if ,surety
			  ;;; f or ...
			  (values ,truth ,surety2)
			  ;;; u or ...
			  (if ,truth
			      ;;; u or t
			      (values ,truth ,surety2)
			      ;;; u or f, or u or u
			      (values nil nil))))))))))
