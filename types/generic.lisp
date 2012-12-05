;;;; generic.lisp

;;;; default behavior for types, such as generic unions

(in-package #:sandalphon.types)

;; FIXME: move this?
(defmethod subtypep ((t1 type-object) (t2 type-object))
  (declare (ignore t1 t2))
  (values nil nil))

;;; these are imprecise, because uuuuugh.

(defclass intersection-type (type-object)
  ((components :accessor intersection-components :initarg :components)))

(defmethod typep (object (type intersection-type))
  (every (curry #'typep object) (intersection-components type)))

(defmethod subtypep ((t1 type-object) (t2 intersection-type))
  (if (every (curry #'subtypep t1) (intersection-components t2))
      (values nil nil)
      (values nil t)))

(defmethod subtypep ((t1 intersection-type) (t2 type-object))
  (if (some (rcurry #'subtypep t2) (intersection-components t1))
      (values t t)
      (values nil nil)))

(defmethod intersection/2 ((t1 intersection-type) (t2 intersection-type))
  (apply #'intersection (append (intersection-components t1) (intersection-components t2))))
(defcomm intersection/2 ((t1 type-object) (t2 intersection-type))
  (labels ((basic (type types)
	     (apply #'intersection type types))
	   #+closer-mop
	   (aux (simple types)
	     (dolist (type types)
	       (let ((specializers (c2mop:method-specializers
				    (first (compute-applicable-methods #'intersection/2 (list simple type))))))
		 (when (or (not (cl:subtypep 'type-object (first specializers)))
			   (not (cl:subtypep 'type-object (second specializers))))
		   (return-from aux (aux (intersection/2 simple type) (remove type types))))))
	     (basic simple types)))
    #-closer-mop
    (basic t1 (intersection-components t2))
    #+closer-mop
    (aux t1 (intersection-components t2))))

(defclass union-type (type-object)
  ((components :accessor union-components :initarg :components)))

(defmethod typep (object (type union-type))
  (some (curry #'typep object) (union-components type)))

(defmethod subtypep ((t1 type-object) (t2 union-type))
  (if (some (curry #'subtypep t1) (union-components t2))
      (values t t)
      (values nil nil)))

(defmethod subtypep ((t1 union-type) (t2 type-object))
  (if (every (rcurry #'subtypep t2) (union-components t1))
      (values nil nil)
      (values nil t)))

(defmethod union/2 ((t1 union-type) (t2 union-type))
  (apply #'union (append (union-components t1) (union-components t2))))
(defcomm union/2 ((t1 union-type) (t2 type-object))
  (labels ((basic (type types)
	     (apply #'union type types))
	   #+closer-mop
	   (aux (simple types)
	     (dolist (type types)
	       (let ((specializers (c2mop:method-specializers
				    (first (compute-applicable-methods #'union/2 (list simple type))))))
		 (when (or (not (cl:subtypep 'type-object (first specializers)))
			   (not (cl:subtypep 'type-object (second specializers))))
		   (return-from aux (aux (union/2 simple type) (remove type types))))))
	     (basic simple types)))
    #-closer-mop
    (basic t1 (union-components t2))
    #+closer-mop
    (aux t1 (union-components t2))))

(defclass negation-type (type-object)
  ((type :accessor negation-type-type :initarg :type)))

(defmethod typep (object (type negation-type))
  (not (typep object (negation-type-type type))))
(defmethod subtypep ((t1 type-object) (t2 negation-type))
  (empty-type-p (intersection t1 (negation-type-type t2))))
(defmethod subtypep ((t1 negation-type) (t2 type-object))
  ;; i have suddenly and tragically lost my ability to figure out what the hell
  (values nil nil))

(defcomm union/2 ((t1 negation-type) (t2 type-object))
  (if (type= (negation-type-type t1) t2)
      *the-top-type*
      (call-next-method)))
(defcomm intersection/2 ((t1 negation-type) (t2 type-object))
  (if (type= (negation-type-type t1) t2)
      *the-bottom-type*
      (call-next-method)))

(defmethod negate ((type negation-type))
  (negation-type-type type))

(defmethod typep (object (type union-type))
  (some (curry #'typep object) (union-components type)))

;;; now the actual generic operations

(defmethod intersection/2 ((t1 type-object) (t2 type-object))
  (make-instance 'intersection-type :components (list t1 t2)))

(defmethod union/2 ((t1 type-object) (t2 type-object))
  (make-instance 'union-type :components (list t1 t2)))

(defmethod negate ((type type-object))
  (make-instance 'negation-type :type type))
