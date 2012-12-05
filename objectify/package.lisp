(defpackage #:sandalphon.objectify
  (:use #:cl #:alexandria)
  (:export #:objectify)
  (:export #:tag #:forms #:statements #:values-type #:form
	   #:vars #:vals #:declarations
	   #:test #:consequent #:else
	   #:setq-params #:situations
	   #:func #:arguments
	   #:thing #:read-only-p)
  (:export #:%variable #:%literal #:%funcall)
  (:export #:reconstruct-source)
  (:shadow #:special-operator-p
	   #:macro-function #:macroexpand #:macroexpand-1))
