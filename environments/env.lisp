;;;; env.lisp

;;;; generic environment structures.

;;;; an environment is a mapping from names (symbols) to objects.
;;;; but actually, lisp environments have multiple mappings, because CL is a "lisp-n".
;;;; e.g., "(function foo)" looks up foo in the function namespace, "foo" looks up foo in the variable namespace.
;;;; or perhaps it would be better to say FUNCTION evaluates its argument in a function namespace, where LAMBDA
;;;;  is a "special form"?

;;;; whatever.  the point is that an environment can have some number of namespaces.
;;;; they also have parents.  it may be useful, for say dynamic analysis, to have environments form a graph, not just a tree
;;;; but different sorts of environments have different concerns for parent lookup.

(in-package #:sandalphon.environments)

(defclass environment ()
  ((namespaces :accessor environment-namespaces :initarg :namespaces
	       :initform nil)
   (parents :accessor environment-parents :initarg :parents
	    :initform nil)))

(defgeneric augment-environment (environment &rest keys &key &allow-other-keys)
  (:documentation "Given an environment, return a new environment with the given bindings in the given namespaces, 
with the provided environment as a parent.

For example, (augment-environment env :variable '((a . 4))) makes a new environment where a is bound to 4, 
and has env as a parent."))

(defmethod augment-environment ((environment environment) &rest keys)
  (make-instance (class-of environment)
		 :namespaces keys
		 :parents (list environment)))

(defun direct-lookup (environment namespace name)
  (cdr (assoc name (getf (environment-namespaces environment) namespace))))
