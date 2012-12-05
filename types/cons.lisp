;;;; cons.lisp

;;;; cons type.

(in-package #:sandalphon.types)

(define-primitive-type cons-type consp)

(defclass compound-cons-type (cons-type)
  ((car-type :accessor cons-type-car-type :initarg :car)
   (cdr-type :accessor cons-type-cdr-type :initarg :cdr)))

(defmethod subtypep ((t1 compound-cons-type) (t2 cons-type)) (values t t))

(define-compound-type compound-cons-type cons-negation-type
  (cons-type-car-type :car car)
  (cons-type-cdr-type :cdr cdr))
