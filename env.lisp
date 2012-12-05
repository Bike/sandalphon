(in-package #:sandalphon)

(defclass environment ()
  ((parents :initarg :parents :accessor environment-parents
	    :initform nil)))

(defun augment-environment (parent &rest initargs &key (class (class-of parent)) &allow-other-keys)
  (apply #'make-instance class :parents (list parent) (remove-from-plist initargs :class)))

(defgeneric lookup (name env kind))

(defmacro define-environment-class (class namespaces)
  (labels ((namespace->accessor-name (namespace)
	     (symbolicate class '#:- namespace))
	   (namespace->slot (namespace)
	   `(,namespace :initarg ,(make-keyword namespace)
			:accessor ,(namespace->accessor-name namespace)
			:initform nil))
	   (namespace->lookup (namespace)
	     `(defmethod lookup (name (env ,class) (kind (eql ',namespace)))
		;; FIXME: annoying plural in third arg
		(assoc-in-graph.preorder name env #'environment-parents
					 :key (function ,(namespace->accessor-name namespace))))))
    `(progn
       (defclass ,class (environment)
	 (,@(mapcar #'namespace->slot namespaces)))
       ,@(mapcar #'namespace->lookup namespaces))))
