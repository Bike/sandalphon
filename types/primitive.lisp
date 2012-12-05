;;;; primitive.lisp

;;;; The type system is unconcerned with the representation of objects; this is left up to the implementation.
;;;; As such, it just delegates to more primitive functions for typep, and defines the simple relations that it can.

;;;; A type being primitive only really implies that it's disjoint from other primitive types.
;;;; For example, take cons.  Cons is primitive, but it's still a subtype of list, which is in turn a subtype of sequence.

;;;; or let me just quote the spec.
;;;; 4.2.2 Type Relationships
;;;;  The types cons, symbol, array, number, character, hash-table, function, readtable, package, pathname, stream,
;;;; random-state, condition, restart, and any single other type created by defstruct, define-condition, or defclass are
;;;; pairwise disjoint, except for type relations explicitly established by specifying superclasses in defclass or
;;;; define-condition or the :include option of destruct.

;;;; "disjoint" means they have no elements in common (meaning their intersection is nil), not that they have no relation
;;;; in the type hierarchy.  for example, cons and array could both be subtypes of structure-object.

(in-package #:sandalphon.types)

(defclass primitive-type (type-object) ())

(defmethod union/2 ((t1 primitive-type) (t2 primitive-type))
  (make-instance 'primitive-union :components (list t1 t2)))
(defmethod intersection/2 ((t1 primitive-type) (t2 primitive-type))
  *the-bottom-type*)
(defmethod subtypep ((t1 primitive-type) (t2 primitive-type))
  (values nil t))
(defmethod negate ((type primitive-type))
  (make-instance 'negation-type :type type))

(defmacro define-primitive-type (name discriminator)
  (with-gensyms (object type)
    `(progn
       (defclass ,name (primitive-type) ())
       ;; todo: add singleton?
       (defmethod subtypep ((t1 ,name) (t2 ,name)) (values t t))
       (defmethod typep (,object (,type ,name))
	 (,discriminator ,object)))))
