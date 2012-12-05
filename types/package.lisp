(defpackage #:sandalphon.types
  (:use #:cl #:alexandria)
  ;; from CL
  (:shadow #:union #:intersection
	   #:subtypep #:typep)
  ;; from CL, unexported
  (:shadow #:array-rank #:pathname-type)
  ;; from alexandria
  (:shadow #:type=)
  ;; exports
  (:export #:union #:intersection
	   #:subtypep #:typep))
