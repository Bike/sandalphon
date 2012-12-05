;;;; package.lisp

(defpackage #:sandalphon
  (:use #:cl #:alexandria)
  (:shadow cl:variable
	   cl:macroexpand cl:macroexpand-1 cl:macro-function))
