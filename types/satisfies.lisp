;;;; satisfies.lisp

(in-package #:sandalphon.types)

(defclass satisfies-type (type-object)
  ((predicate :accessor satisfies-type-predicate :initarg :predicate
	      :type symbol)))

(defmethod typep (object (type satisfies-type))
  (funcall (fdefinition (satisfies-type-predicate type)) object))

(defcomm subtypep (t1 (t2 satisfies-type))
  (values nil nil))

;;; gotta reassert these

(defmethod subtypep ((t1 satisfies-type) (t2 top-type))
  (values t t))

;; well, this one probably doesn't need reassertion because of arg precedence,
;; but why not do it anyway

(defmethod subtypep ((t1 bottom-type) (t2 satisfies-type))
  (values t t))
