;;;; misc-primitive.lisp

;;;; atomic types that are opaque to the system.

(in-package #:sandalphon.types)

(define-primitive-type symbol-type symbolp)
(define-primitive-type hash-table-type hash-table-p)
(define-primitive-type readtable-type readtablep)
(define-primitive-type package-type packagep)
(define-primitive-type pathname-type pathnamep)
(define-primitive-type random-state-type random-state-p)
(define-primitive-type restart-type (lambda (x) (cl:typep x 'restart)))
