;;;; member.lisp

;;;; (member ...) and (not (member ...)) types, MEMBER-TYPE and EXCLUSION-TYPE respectively.
;;;; See subinfinite.lisp for some of their unions and intersections.

(in-package #:sandalphon.types)

;;; Member type.
;;; TODO: Should be waaaay faster with a proper finite set data structure.
(defclass member-type (type-object)
  ((objects :accessor member-objects :initarg :objects)))

(defmethod union/2 ((t1 member-type) (t2 member-type))
  (make-instance 'member-type :objects (cl:union (member-objects t1) (member-objects t2))))
(defcomm union/2 ((t1 member-type) (t2 type-object))
  (let ((outcasts (remove-if-not (rcurry #'typep t2) (member-objects t1))))
    (if outcasts
	;; (or (member 2) symbol)
	(make-instance 'outcast-type :main t2 :objects outcasts)
	;; (or (member hello) symbol)
	t2)))
(defmethod intersection/2 ((t1 member-type) (t2 member-type))
  (make-instance 'member-type :objects (cl:intersection (member-objects t1) (member-objects t2))))
(defcomm intersection/2 ((t1 member-type) (t2 type-object))
  (let ((outcasts (remove-if-not (rcurry #'typep t2) (member-objects t1))))
    (if outcasts
	;; (and (member 2) symbol)
	*the-bottom-type*
	;; (and (member 2) integer)
	t1)))
(defmethod subtypep ((t1 member-type) (t2 member-type))
  (values (cl:subsetp (member-objects t1) (member-objects t2)) t))
(defmethod subtypep ((t1 member-type) t2)
  (values (every (rcurry #'typep t2) (member-objects t1)) t))
(defmethod typep (object (type member-type))
  (member object (member-objects type)))
(defmethod negate ((type member-type))
  (if (null (member-objects type))
      *the-top-type*
      (make-instance 'exclusion-type :objects (member-objects type))))

;;; (member) and nil are the same type.
(defcomm subtypep ((t1 member-type) (t2 bottom-type))
  (values (null (member-objects t1)) t))
;;; no need for an EQL type, that can just expand to a MEMBER without real trouble

(defclass exclusion-type (type-object)
  ((objects :accessor exclusion-objects :initarg :objects))
  (:documentation "The type of all objects not in a given finite set, i.e., the opposite of MEMBER."))
;;; The set of all objects not part of some finite set.  Dual to member in enough ways that I should probably automate it
(defmethod union/2 ((t1 exclusion-type) (t2 exclusion-type))
  (make-instance 'exclusion-type :objects (cl:intersection (exclusion-objects t1) (exclusion-objects t2))))
(defcomm union/2 ((t1 exclusion-type) (t2 type-object))
  (let ((incasts (remove-if-not (rcurry #'typep t2) (exclusion-objects t1))))
    (if incasts
	;; (or (not (member 2)) symbol)
	t1
	;; (or (not (member 2)) integer)
	*the-top-type*)))
(defmethod intersection/2 ((t1 exclusion-type) (t2 exclusion-type))
  (make-instance 'exclusion-type :objects (cl:union (exclusion-objects t1) (exclusion-objects t2))))
(defcomm intersection/2 ((t1 exclusion-type) (t2 type-object))
  (let ((incasts (remove-if-not (rcurry #'typep t2) (exclusion-objects t1))))
    (if incasts
	;; (and (not (member 2)) symbol)
	t2
	;; (and (not (member 2)) integer)
	(make-instance 'incast-type :main t2 :objects incasts))))
(defmethod subtypep ((t1 exclusion-type) (t2 exclusion-type))
  (values (cl:subsetp (exclusion-objects t2) (exclusion-objects t1)) t))
(defmethod subtypep (t1 (t2 exclusion-type)) ; careful about arg precedence
  (values (notany (rcurry #'typep t1) (exclusion-objects t2)) t))
(defmethod typep (object (type exclusion-type))
  (not (member object (exclusion-objects type))))
(defmethod negate ((type exclusion-type))
  (if (null (exclusion-objects type))
      *the-bottom-type*
      (make-instance 'member-type :objects (exclusion-objects type))))

;; (not (member)) = t
(defcomm subtypep ((t1 exclusion-type) (t2 top-type))
  (values (null (exclusion-objects t1)) t))

