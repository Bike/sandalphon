(in-package #:sandalphon.types)

;;;; Notes: Types are (often infinite) sets.
;;;; Necessary operations on them are union, intersection, subtypep (subset), typep (membership).
;;;; Negation (wrt T) too, though that's a bit tricky for a variety of reasons.

(defclass type-object () ()) ; bla bla symbol conflicts bla

(defgeneric union/2 (t1 t2)
  (:documentation "Given two types, return a new type for their union."))
(defgeneric intersection/2 (t1 t2)
  (:documentation "Given two types, return a new type for their intersection."))
(defgeneric subtypep (t1 t2)
  (:documentation "As cl:subtypep (excepting the lack of environment)"))
(defgeneric typep (object type)
  (:argument-precedence-order type object)
  (:documentation "As cl:typep (excepting the lack of environment)"))
(defgeneric negate (type)
  ;; if at all possible, avoid NOT types, and have this return some usual type
  (:documentation "Return a new type containing all objects not contained by TYPE."))

(defun union (&rest types) (reduce #'union/2 types))
(defun intersection (&rest types) (reduce #'intersection/2 types))

(defun type= (t1 t2)
  (tri/if (subtypep t1 t2)
	  (subtypep t2 t1)
	  (values nil t)
	  (values nil nil)))

;; not sure how to properly express commutativity well, bleh.
(defmacro defcomm (name lambda-list &body body)
  `(progn (defmethod ,name ,lambda-list ,@body)
	  (defmethod ,name ,(reverse lambda-list) ,@body)))

;;; TODO: Maybe move this?
;;; holy shit this turned out long.
(defmacro define-compound-type (class negaclass-name &rest accessors-etc)
  "Define a \"compound\" type's methods.
This does not correspond to CL's compound type specifiers, nor is there a class of compound types.
Rather, a compound type is one made up of a number of components which can be checked for various relations elementwise
  in a fashion predictable enough to let this convenience macro help."
  (let* ((accessors (mapcar #'first accessors-etc))
	 (initargs (mapcar #'second accessors-etc))
	 (objkeys (mapcar #'third accessors-etc))
	 (object (gensym "OBJECT"))
	 (type (gensym "TYPE"))
	 (slots (mapcar #'make-gensym accessors)))
    (labels ((method (name)
	       (flet ((initarg (initarg slot) (list initarg slot))
		      (empty-check (slot)
			`(empty-type-p ,slot))
		      (bind (slot accessor)
			`(,slot (,name (,accessor t1) (,accessor t2)))))
		 `(defmethod ,name ((t1 ,class) (t2 ,class))
		    (let (,@(mapcar #'bind slots accessors))
		      (if (or ,@(mapcar #'empty-check slots)) ; i.e. impossibility
			  *the-bottom-type*
			  (make-instance ',class ,@(mapcan #'initarg initargs slots))))))))
	`(progn
	   ,(method 'union/2)
	   ,(method 'intersection/2)
	   (defmethod subtypep ((t1 ,class) (t2 ,class))
	     (tri/and ,@(mapcar (lambda (accessor) `(subtypep (,accessor t1) (,accessor t2))) accessors)))
	   (defmethod typep (,object (,type ,class))
	     (and (call-next-method)
		  ,@(mapcar (lambda (accessor objkey) `(typep (,objkey ,object) (,accessor ,type)))
			    accessors objkeys)))
	   (defmethod negate ((type ,class))
	     (make-instance ',negaclass-name :type type))
	   (defclass ,negaclass-name (negation-type) ())))))
