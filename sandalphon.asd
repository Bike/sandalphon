(asdf:defsystem #:sandalphon
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :components ((:module "types"
			:components ((:file "package")
				     (:file "trivalogic" :depends-on ("package"))
				     (:file "types" :depends-on ("package" "trivalogic"))
				     (:file "primitive" :depends-on ("package" "types"))
				     (:file "tnil" :depends-on ("package" "types"))
				     (:file "member" :depends-on ("package" "types" "tnil"))
				     (:file "subinfinite" :depends-on ("package" "types" "member"))
				     (:file "interval" :depends-on ("package" "types"))
				     (:file "cons" :depends-on ("package" "types" "primitive"))
				     (:file "array" :depends-on ("package" "types"))))))
#|
	       (:file "package")
	       (:file "env" :depends-on ("package"))
	       (:file "compile" :depends-on ("package"))
	       (:file "lex" :depends-on ("package" "env" "compile"))))
|#
