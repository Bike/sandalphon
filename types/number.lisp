;;;; number.lisp

;;; FIXME: Ugly.

;;; Intervals are dealt with separately: see interval.lisp.

(in-package #:sandalphon.types)

(defclass scalar-type (primitive-type)
  ((name :accessor scalar-type-name :initarg :name
	 :type symbol)))

(defmethod typep (object (type scalar-type))
  ;; kinda pointless
  (ecase (scalar-type-name type)
    ((integer) (integerp object))
    ((ratio) (cl:typep object 'ratio)) ; no ratiop
    ((rational) (rationalp object))
    ((short-float single-float double-float long-float) (cl:typep object (scalar-type-name type)))
    ((float) (floatp object))
    ((real) (realp object))
    ;; complexes are separate
    ((number) (numberp object))))

(defmethod subtypep ((t1 scalar-type) (t2 scalar-type))
  (macrolet ((tree (&rest specs)
	       `(cond ,@(mapcar (lambda (spec)
				  `((eql (scalar-type-name t1) ',(first spec))
				    ;; generalized booleans, yo.
				    (values (member (scalar-type-name t2) ',spec) t)))
				specs))))
    (tree (integer rational real number)
	  (ratio rational real number)
	  (short-float float real number)
	  (single-float float real number)
	  (double-float float real number)
	  (long-float float real number)
	  (float real number)
	  (rational real number)
	  (real number))))
