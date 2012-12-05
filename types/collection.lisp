(in-package #:sandalphon.types)

(defclass collection-type (type-object)
  ((element-type :initarg :element-type :accessor collection-type-element-type
		 :type type-object)
   (size :initarg :size :accessor collection-type-size)
   (impl :initarg :impl :accessor collection-type-implementation)))

;;; collections are extensible (i.e. new subtypes of COLLECTION can be defined that are disjoint from all others)
;;; so the predicate's kind of tricky... so... I'm just going to fudge it

;;; thinking more on that, that basically means the fundamental collection methods are "unsealed".
;;; that's easy for methods that take the type, slightly trickier for collectionp here.

;;; wait, no.  what am I doing.

(defgeneric collectionp (object)
  (:documentation "True if OBJECT is some sort of collection, false otherwise.")
  (:method (object) nil)
  (:method ((object sequence)) t)
  (:method ((object array)) t))

(defgeneric size (object)
  (:documentation "The size of a collection, e.g. LENGTH for sequences.")
  (:method ((object sequence))
    (length object))
  (:method ((object list))
    ;; for circular lists
    (list-length object))
  (:method ((object array))
    ;; a-t-s = length for a vector, of course
    (array-total-size object)))

(defgeneric size< (s1 s2)
  (:method ((s1 integer) (s2 integer))
    (< s1 s2))
  (:method ((s1 null) (s2 integer))
    nil)
  (:method ((s1 integer) (s2 null))
    t))

(defgeneric size= (s1 s2)
  (:method ((s1 integer) (s2 integer)) (= s1 s2))
  (:method ((s1 null) (s2 integer)) nil)
  (:method ((s1 integer) (s2 null)) nil))

(defun dimension-spec-<= (ds1 ds2)
  (or (eql ds2 '*)
      (and (not (eql ds1 '*))
	   (size= ds1 ds2))))

(defmethod typep (object (type collection-type))
  (and (collectionp object)
       (dimension-spec-<= (size object) (collection-type-size type))
       (typep object (collection-type-implementation type))))

(defmethod subtypep ((t1 collection-type) (t2 collection-type))
  (if (dimension-spec-<= (collection-type-size t1) (collection-type-size t2))
      (subtypep (collection-type-implementation t1) (collection-type-implementation t2))
      (values nil t)))
