;;;; subinfinite.lisp

;;;; OUTCAST-TYPE and INCAST-TYPE: unions and intersections of infinite sets with finite sets (MEMBER specifically).

(in-package #:sandalphon.types)

(defclass outcast-type (member-type)
  ((main :initarg :main :accessor outcast-main))
  ;; e.g. (or (member 2 foo) random-state) -- some infinite set, plus a finite set.
  (:documentation "The type of unsimplifiable unions of MEMBER types and other types."))

(defcomm union/2 ((t1 outcast-type) (t2 type-object))
  ;; treat t1 as a member type, then union the result with the rest of the outcast type.
  ;; example: (or (or (member 1 2) symbol) (member 3)) => (or symbol (or (member 1 2) (member 3)))
  ;; => (or symbol (or member 1 2 3)), as desired.
  (union (outcast-main t1) (call-next-method)))
(defmethod union/2 ((t1 outcast-type) (t2 outcast-type))
  (union (outcast-main t1) (outcast-main t2)
	 (make-instance 'member-type :objects (cl:union (member-objects t1) (member-objects t2)))))
(defcomm intersection/2 ((t1 outcast-type) (t2 type-object))
  ;; ditto(ish)
  (intersection (outcast-main t1) (call-next-method)))
(defmethod intersection/2 ((t1 outcast-type) (t2 outcast-type))
  (intersection (outcast-main t1) (outcast-main t2)
		(make-instance 'member-type :objects (cl:intersection (member-objects t1) (member-objects t2)))))
(defmethod subtypep ((t1 outcast-type) (t2 type-object))
  (if (subtypep (outcast-main t1) t2)
      (values t t) ;; if first value is T, second must be as well
      (call-next-method))) ; t1 as member-type
(defmethod typep (object (type outcast-type))
  (or (typep object (outcast-main type)) (call-next-method)))
(defmethod negate ((type outcast-type))
  ;; (not (or (member foo) integer)) => (and (not (member foo)) (not integer)).  deMorgan, yo.
  (intersection (call-next-method) (negate (outcast-main type))))

(defclass incast-type (exclusion-type)
  ((main :initarg :main :accessor incast-main))
  ;; e.g. (or (not (member 2) integer)) -- some infinite set minus a finite set.
  (:documentation "The type of unsimplifiable intersections of negations of MEMBER types and other types."))

(defcomm union/2 ((t1 incast-type) (t2 type-object))
  (union (incast-main t1) (call-next-method)))
(defmethod union/2 ((t1 incast-type) (t2 incast-type))
  (union (incast-main t1) (incast-main t2)
	 (make-instance 'exclusion-type :objects (cl:intersection (exclusion-objects t1) (exclusion-objects t2)))))
(defcomm intersection/2 ((t1 incast-type) (t2 type-object))
  (intersection (incast-main t1) (call-next-method)))
(defmethod intersection/2 ((t1 incast-type) (t2 incast-type))
  (intersection (incast-main t1) (incast-main t2)
		(make-instance 'exclusion-type :objects (cl:union (exclusion-objects t1) (exclusion-objects t2)))))
(defmethod subtypep ((t1 incast-type) (t2 type-object))
  (multiple-value-bind (sub? ver) (subtypep (incast-main t1) t2)
    (if (and sub? ver)
	(call-next-method)
	(values sub? ver))))
(defmethod typep (object (type incast-type))
  (and (typep object (incast-main type)) (call-next-method)))
(defmethod negate ((type incast-type))
  ;; more deMorgan: (not (and (not (member foo)) symbol)) => (or (not symbol) (member foo))
  (union (negate (incast-main type)) (call-next-method)))

