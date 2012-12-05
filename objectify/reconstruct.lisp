;;;; reconstruct.lisp
;;;; given a code object, output some lisp-looking source that could give rise to it

(in-package #:sandalphon.objectify)

(defgeneric reconstruct-source (code-object)
  (:method ((code t)) code))

(declaim (inline reconstruct-list))
(defun reconstruct-list (list) (mapcar #'reconstruct-source list))

(defun reconstruct-let (code)
  (with-slots (vars vals declarations forms) code
    `(,(class-name (class-of code)) (,@(mapcar (lambda (var val) (list var (reconstruct-source val))) vars vals))
       ,@declarations
       ,@(reconstruct-list forms))))
(defun reconstruct-flet (code)
  (with-slots (vars vals declarations forms) code
    `(,(class-name (class-of code)) (,@(mapcar (lambda (var val)
						 (list* var (params val) (reconstruct-list forms)))
					       vars vals))
       ,@declarations
       ,@(reconstruct-list forms))))

(defmethod reconstruct-source ((code sandalphon.so:catch))
  `(catch ,(reconstruct-source (tag code)) ,@(reconstruct-list (forms code))))
(defmethod reconstruct-source ((code sandalphon.so:block))
  `(block ,(tag code) ,@(reconstruct-list (forms code))))
(defmethod reconstruct-source ((code sandalphon.so:tagbody))
  `(tagbody ,@(mapcar (lambda (stmt) (if (symbolp stmt) stmt (reconstruct-source stmt))) (statements code))))

(defmethod reconstruct-source ((code sandalphon.so:let))
  (reconstruct-let code))
(defmethod reconstruct-source ((code sandalphon.so:let*))
  (reconstruct-let code))

(defmethod reconstruct-source ((code sandalphon.so:flet))
  (reconstruct-flet code))
(defmethod reconstruct-source ((code sandalphon.so:labels))
  (reconstruct-flet code))

(defmethod reconstruct-source ((code sandalphon.so:progv))
  `(progv ,(vars code) ,(reconstruct-source (vals code)) ,@(forms code)))

(defmethod reconstruct-source ((code sandalphon.so:progn))
  `(progn ,@(reconstruct-list (forms code))))
(defmethod reconstruct-source ((code sandalphon.so:eval-when))
  `(eval-when ,(situations code) ,@(reconstruct-list (forms code))))
(defmethod reconstruct-source ((code sandalphon.so:locally))
  `(locally ,@(declarations code) ,@(reconstruct-list (forms code))))

(defmethod reconstruct-source ((code sandalphon.so:function))
  (reconstruct-source (thing code)))
(defmethod reconstruct-source ((code sandalphon.so:quote))
  (list 'quote (thing code)))

(defmethod reconstruct-source ((code sandalphon.so:if))
  `(if ,(reconstruct-source (test code))
       ,(reconstruct-source (consequent code))
       ,(reconstruct-source (else code))))
(defmethod reconstruct-source ((code sandalphon.so:multiple-value-call))
  `(multiple-value-call ,(reconstruct-source (func code)) ,@(reconstruct-list (arguments code))))

(defmethod reconstruct-source ((code %funcall))
  `(,(reconstruct-source (func code)) ,@(reconstruct-list (arguments code))))
(defmethod reconstruct-source ((code %variable))
  (thing code))
(defmethod reconstruct-source ((code %literal))
  (thing code))

(defmethod reconstruct-source ((code lambda-function))
  `(lambda ,(params code) ,(reconstruct-list (forms code))))

