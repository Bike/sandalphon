;;;; special-operators.lisp

(defpackage #:sandalphon.special-operators
  (:nicknames #:sandalphon.so)
  (:export #:catch #:load-time-value #:setq
	   #:eval-when #:locally #:symbol-macrolet
	   #:flet #:macrolet #:tagbody
	   #:function #:multiple-value-call #:the
	   #:go #:multiple-value-prog1 #:throw
	   #:if #:progn #:unwind-protect
	   #:block #:return-from
	   #:labels #:progv
	   #:let #:let* #:quote))

(defun cl->so (symbol)
  (find-symbol (symbol-name symbol) (find-package '#:sandalphon.special-operators)))
