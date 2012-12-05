(in-package #:sandalphon)

(defclass compile-object ()
  ((original-form :initarg :original-form :accessor compile-object-original-form)))

(defclass symbol-access (compile-object)
  ((var :initarg :var :accessor symbol-access-var)))

(defclass function-call (compile-object)
  ((function :initarg :function :accessor function-call-function)
   (args :initarg :args :accessor function-call-args)))
