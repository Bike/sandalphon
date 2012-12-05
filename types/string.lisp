;;;; string.lisp

(in-package #:sandalphon.types)

(defclass array-union-type (array-type)
  ((union-type :accessor array-union-type-type :initarg :union-type)))

;; element-type is, somewhat magically, just the default *

(defmethod typep (object (type array-union-type))
  (and (call-next-method) ; array
       (subtypep (parse-type (array-element-type object)) (array-union-type-type type))))

(defmethod subtypep ((t1 array-type) (t2 array-union-type))
  (and (call-next-method)
       (subtypep (array-type-element-type t1) (array-union-type-type t2))))

;;; subtype a-u-t a-t is covered by subtype a-t a-t, namely, no a-t is a supertype of an a-u-t, except a-t *.

(defcomm union/2 ((t1 array-type) (t2 array-union-type))
  (make-instance 'array-union-type
		 :union-type (union (array-type-element-type t1) (array-union-type-type t2))
		 :simple (and (array-type-simplicity t1) (array-type-simplicity t2))
		 :dimensions (ads-mix (array-type-dimensions t1) (array-type-dimensions t2) :union)))

(defmethod union/2 ((t1 array-union-type) (t2 array-union-type))
  ;; so much duplication
  (make-instance 'array-union-type
		 :union-type (union (array-union-type-type t1) (array-union-type-type t2))
		 :simple (and (array-type-simplicity t1) (array-type-simplicity t2))
		 :dimensions (ads-mix (array-type-dimensions t1) (array-type-dimensions t2) :union)))
