(in-package #:sandalphon)

;;;; Helper macros for not much but saving typing.

(defmacro define-namespace (accessor-name variable-name)
  `(progn
     (defvar ,variable-name (make-hash-table)) ; FIXME: include hash table test option?
     (defun ,accessor-name (name)
       (gethash name ,variable-name))
     (defun (setf ,accessor-name) (new-value name)
       (setf (gethash name ,variable-name) new-value))))

(defmacro defclass-struct (name direct-superclasses direct-slots &rest options)
  "Shorthand for DEFCLASS with DEFSTRUCT-like slots automatically used."
  ;; may or may not be a good idea.
  (flet ((frob-slot-spec (slot-spec)
	   (if (consp slot-spec)
	       slot-spec
	       `(,slot-spec :initarg ,(make-keyword slot-spec) :accessor ,(symbolicate name '#:- slot-spec)))))
    `(defclass ,name ,direct-superclasses
       (,@(mapcar #'frob-slot-spec direct-slots))
       ,@options)))
