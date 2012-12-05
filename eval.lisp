(defpackage #:sandalphon.eval
  (:use #:cl #:alexandria #:sandalphon.objectify)
  (:shadow cl:eval))

(in-package #:sandalphon.eval)

(defun eval (form)
  (%eval (objectify form nil) nil))

(defgeneric lookup (env name accessor))

(defclass environment ()
  ((parent :accessor parent :initarg :parent :initform nil)))

(defmethod lookup ((env environment) name accessor)
  (or (ignore-errors (cdr (assoc name (funcall accessor env))))
      (lookup (parent env) name accessor)))

(defmethod lookup ((env null) name accessor)
  (declare (ignore name accessor))
  nil)

(defclass lexical-environment (environment)
  ((lvars :accessor lvars :initarg :lvars :initform nil)
   (lfuns :accessor lfuns :initarg :lfuns :initform nil)
   (blocks :accessor blocks :initarg :blocks :initform nil)
   (tagbody-tags :accessor tagbody-tags :initarg :tags :initform nil)))

(defmethod lookup :around ((env lexical-environment) name (accessor (eql #'lfuns)))
  (or (call-next-method)
      (cl:fdefinition name)))

(defgeneric %eval (object env))

(defun %evlis (objects env) (mapcar (rcurry #'%eval env) objects))

(defun %evbody (body env)
  (loop with result = nil
     for code in body do (setf result (%eval code env))
     finally (return result)))

(defmethod %eval ((code sandalphon.so:let) env)
  (%evbody (forms code)
	   (make-instance 'lexical-environment
			  :parent env
			  :lvars (mapcar (lambda (var val) (cons var (%eval val env))) (vars code) (vals code)))))

(defmethod %eval ((code sandalphon.so:function) env)
  (if (typep (thing code) '(cons (member lambda)))
      (error "not implemented yet")
      (lookup env (thing code) #'lfuns)))
(defmethod %eval ((code sandalphon.so:quote) env)
  (declare (ignore env))
  (thing code))

(defmethod %eval ((code %funcall) env)
  (apply (%eval (func code) env) (%evlis (arguments code) env)))
(defmethod %eval ((code %literal) env)
  (thing code))
(defmethod %eval ((code %variable) env)
  (lookup env (thing code) #'lvars))
