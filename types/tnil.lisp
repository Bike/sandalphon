;;;; tnil.lisp

;;;; Top and bottom types - T and NIL - top-type and bottom-type.
;;;; Also has *the-bottom-type* and *the-top-type* for all your singleton needs.
;;;; And now full-type-p and empty-type-p for convenience.

(in-package #:sandalphon.types)

;;; Starting simple: The empty type, a.k.a. nil.
(defclass bottom-type (type-object) ())
(defvar *the-bottom-type* (make-instance 'bottom-type))

;;; And its much vaunted sister, the top type, T.
(defclass top-type (type-object) ())
(defvar *the-top-type* (make-instance 'top-type))

(defcomm union/2 ((t1 bottom-type) (t2 type-object)) t2)
(defcomm intersection/2 ((t1 bottom-type) (t2 type-object)) *the-bottom-type*)
(defmethod subtypep ((t1 bottom-type) (t2 type-object)) (values t t))
;; arg precedence order means the first method here should be chosen first in (subtypep nil nil)
(defmethod subtypep ((t1 type-object) (t2 bottom-type)) (values nil t))
(defmethod typep (object (type bottom-type)) nil)
(defmethod negate ((type bottom-type)) *the-top-type*)

(defcomm union/2 ((t1 top-type) (t2 type-object)) *the-top-type*)
(defcomm intersection/2 ((t1 top-type) (t2 type-object)) t2)
(defmethod subtypep ((t1 top-type) (t2 top-type)) (values t t)) ; bla bla argument precedence
(defmethod subtypep ((t1 top-type) (t2 type-object)) (values nil t))
(defmethod subtypep ((t1 type-object) (t2 top-type)) (values t t))
(defmethod typep (object (type top-type)) t)
(defmethod negate ((type top-type)) *the-bottom-type*)

(defun full-type-p (type) (subtypep *the-top-type* type))
(defun empty-type-p (type) (subtypep type *the-bottom-type*))
