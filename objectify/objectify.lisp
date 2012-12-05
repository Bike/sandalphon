
;;;; NOTE TO SELF: This is strictly SYNTAX ONLY.  No rearranging things for convenience, do that later!
;;;; Essentially, this is minimal compilation (3.2.2.2) AT MOST.

;;;; The macroexpansion doesn't really work.  For example, sbcl's PUSH (quite reasonably) calls get-setf-expansion
;;;; (at macroexpansion time), and that'll fail if the macroexpansion function is passed a macro-env.

(in-package #:sandalphon.objectify)

;;; FIXME
;;; (defmacro foo (stuff &environment env) (macroexpand `(bar ,@stuff) env))
;;; (defmacro bar (&rest things) `(list ,@things))
;;; (macrolet ((bar (&rest things) `(vector ,@things))) (foo 1 2))
;;;  this will error, because CL macros won't know how to deal with the sandalphon env passed to (macro-function 'foo)

;;; We basically want to redefine macro-function, since we can't do jack (like augment) with native envs

(defclass macro-env ()
  ((macros :accessor macro-env-macros :initarg :macros)
   (symbol-macros :accessor macro-env-symbol-macros :initarg :symbol-macros)))

(defmethod macro-env-macros ((env null)) nil)
(defmethod macro-env-symbol-macros ((env null)) nil)

(defun empty-macro-env ()
  (make-instance 'macro-env :symbol-macros nil :macros nil))

(defun augment-symbol-macros (binds env)
  (make-instance 'macro-env
		 :macros (macro-env-macros env)
		 :symbol-macros (list* binds (macro-env-symbol-macros env))))

(defun augment-macros (binds env)
  (make-instance 'macro-env
		 :symbol-macros (macro-env-symbol-macros env)
		 :macros (list* binds (macro-env-macros env))))

(defun macro-function (symbol &optional env)
  (cond ((typep env 'macro-env)
	 (or (cdr (assoc symbol (macro-env-macros env)))
	     (cl:macro-function symbol nil)))
	(t (cl:macro-function symbol env))))

(defun symbol-macro-function (symbol &optional env)
  (flet ((native (env)
	   (multiple-value-bind (expansion expanded?)
	       (cl:macroexpand symbol env)
	     (if expanded?
		 ;; not sure how macro functions are supposed to deal with different environments...
		 ;; but then CL doesn't have symbol macro function access anyway!
		 (lambda (symbol env) (declare (ignore symbol env)) expansion)
		 nil))))
    (cond ((typep env 'macro-env)
	   (or (cdr (assoc symbol (macro-env-symbol-macros env)))
	       (native nil)))
	  (t (native env)))))

(defun macroexpand-1 (form &optional env)
  (typecase form
    (symbol (let ((expander (symbol-macro-function form env)))
	      (if expander
		  (values (funcall *macroexpand-hook* expander form env) t)
		  (values form nil))))
    (cons (let ((expander (macro-function (first form) env)))
	    (if expander
		(values (funcall *macroexpand-hook* expander form env) t)
		(values form nil))))
    (t (values form nil))))

(defun macroexpand (form &optional env)
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded-p)
                 (macroexpand-1 form env)
               (if newly-expanded-p
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form nil)))

;; FIXME
(defun special-operator-p (symbol) (cl:special-operator-p symbol))

(defun objectify (form env)
  (let ((form (macroexpand form env)))
    (typecase form
      (symbol (objectify-variable form env))
      (cons (cond ((special-operator-p (first form))
		   (objectify-special-operator form env))
		  (t (objectify-call form env))))
      (t (objectify-constant form env)))))

(defun objectify-constant (form env)
  (declare (ignore env))
  (make-instance '%literal :thing form :read-only-p t))

(declaim (inline objectify-forms))
(defun objectify-forms (forms env) (mapcar (rcurry #'objectify env) forms))

(defun objectify-variable (variable env)
  (declare (ignore env))
  (make-instance '%variable :thing variable))

(deftype lambda-expression ()
  '(cons (member lambda) (cons list *)))

(defclass lambda-function () ; bleh, symbol conflicts.
  ((params :accessor params :initarg :params)
   (forms :accessor forms :initarg :forms)))

(defun objectify-lambda (lexpr env)
  (check-type lexpr lambda-expression)
  (make-instance 'lambda-function
		 :params (second lexpr)
		 :forms (objectify-forms (cddr lexpr) env)))

(defun objectify-call (form env)
  ;; note - 3.1.2.1.2.3 specifies that while argument evaluation is left to right, looking up the function itself
  ;;  can happen before or after that.
  ;; (I think, but am not sure, this justifies the macroexpansion order here)
  (destructuring-bind (func &rest args) form
    ;; this is here because (a) why not do it early (b) (function (setf foo)) is legal but not ((setf foo) ...)
    (check-type func (or symbol lambda-expression) "a symbol naming a function or a lambda expression")
    (make-instance '%funcall
		   :func (objectify-special-operator `(function ,func) env) ; why not?
		   :arguments (objectify-forms args env))))

(defvar *special-operator-objectifiers* (make-hash-table))

(defmacro define-so-objectifier (name env-var lambda-list &body body)
  (let ((env-used env-var)
	(env-var (or env-var (gensym "ENV")))
	(form-var (gensym "FORM")))
    `(setf (gethash ',name *special-operator-objectifiers*)
	   (lambda (,form-var ,env-var)
	     ,@(unless env-used (list `(declare (ignore ,env-var))))
	     (destructuring-bind ,lambda-list (rest ,form-var)
	       ,@body)))))

(defmacro define-simple-objectifier (name special-lambda-list)
  (with-gensyms (env)
    (labels ((m-i-args (lambda-list)
	       (etypecase lambda-list
		 (null nil)
		 (symbol (list (make-keyword lambda-list) `(objectify-forms ,lambda-list ,env)))
		 ((cons (cons (member quote) (cons symbol null)))
		  (list* (make-keyword (second (first lambda-list)))
			 (second (first lambda-list))
			 (m-i-args (rest lambda-list))))
		 ((cons symbol *)
		  (list* (make-keyword (first lambda-list))
			 `(objectify ,(first lambda-list) ,env)
			 (m-i-args (rest lambda-list))))))
	     (real-lambda-list (lambda-list)
	       ;; could be a MAPLIST probably
	       (etypecase lambda-list
		 (null nil)
		 (symbol (list '&body lambda-list))
		 ((cons (cons (member quote) (cons symbol null)) *)
		  (cons (second (first lambda-list))
			(real-lambda-list (rest lambda-list))))
		 ((cons symbol *)
		  (cons (first lambda-list)
			(real-lambda-list (rest lambda-list)))))))
      `(define-so-objectifier ,name ,env ,(real-lambda-list special-lambda-list)
	 (make-instance ',(find-symbol (string name) '#:sandalphon.so)
			,@(m-i-args special-lambda-list))))))

(defun objectify-special-operator (form env)
  (funcall (gethash (first form) *special-operator-objectifiers*) form env))

(define-so-objectifier macrolet env (bindings &body body)
  (labels ((ll-keyword (lambda-list keyword)
	     "Given a macro lambda list and a lambda list keyword,
return the keyword's parameter (or NIL), and a lambda list cleansed of it as multiple values."
	     (loop for (param . rest) on lambda-list
		with so-far = nil
		when (eql param keyword) return (values (first rest) (append (nreverse so-far) (rest rest)))
		do (push param so-far)
		finally (return (values nil (nreverse so-far)))))
	   (macro-lexpr (lambda-list body)
	     ;; all this garbage and (macrolet ((fuck (&environment env) (declare env) ...)) ...) will still break
	     ;; FUCKING SYNTAX
	     (multiple-value-bind (whole lambda-list)
		 (if (eql (first lambda-list) '&whole)
		     (values (second lambda-list) (cddr lambda-list))
		     (values nil lambda-list))
	       (multiple-value-bind (env lambda-list)
		   (ll-keyword lambda-list '&environment)
		 (let ((env-used env)
		       (env (or env (gensym "ENV")))
		       (whole (or whole (gensym "WHOLE"))))
		   `(lambda (,whole ,env)
		      ,@(unless env-used (list `(declare (ignore ,env))))
		      (destructuring-bind ,lambda-list (rest ,whole)
			,@body)))))))
  (multiple-value-bind (forms decls) (parse-body body)
    (declare (ignore decls)) ;; TODO: Figure out wtf
    (objectify `(progn ,@forms) ; not objectify-forms, which just returns a list!
	       (augment-macros (mapcar (lambda (def)
					 (cons (first def)
					       (coerce (macro-lexpr (second def) (cddr def)) 'function)))
				       bindings)
			       env)))))

(define-so-objectifier symbol-macrolet env (bindings &body body)
  (multiple-value-bind (body decls) (parse-body body)
    (declare (ignore decls)) ;; again: wtf
    (objectify `(progn ,@body)
	       (augment-symbol-macros (mapcar (lambda (bind)
						(cons (first bind)
						      (lambda (form env)
							;; double closure~
							(declare (ignore form env))
							(second bind))))
					      bindings)
				      env))))

(define-so-objectifier load-time-value nil (form &optional read-only-p)
  (make-instance '%literal
		 :read-only-p (eql read-only-p t) ; precisely T, yup.  weird, huh?
		 ;; I'm not entirely sure about (symbol-macrolet ((foo t)) (load-time-value die foo))
		 ;; but who cares
		 ;; anyway eval is a bit tricky here and I'm probably doing something wrong
		 :thing (eval form)))

(define-simple-objectifier catch (tag . forms))
(define-simple-objectifier block ('tag . forms))

(define-so-objectifier tagbody env (&body statements)
  (make-instance 'sandalphon.so:tagbody
		 :statements (mapcar (lambda (statement)
				       (if (symbolp statement)
					   statement
					   (objectify statement env)))
				     statements)))
(define-simple-objectifier the ('values-type form))

(define-simple-objectifier throw (tag form))
(define-simple-objectifier return-from ('tag form))
(define-simple-objectifier go ('tag))

(macrolet ((define-binder (name var val augment objectify)
	     `(define-so-objectifier ,name env (bindings &body body)
		(flet ((var (bind) ,var)
		       (val (bind) ,val)
		       (shadow-macros (shadowing) (,augment (mapcar (rcurry #'cons nil) shadowing) env)))
		  (let* ((vars (mapcar #'var bindings))
			 (vals (mapcar (compose (rcurry #',objectify env) #'val) bindings))
			 (env (shadow-macros vars)))
		    (multiple-value-bind (forms decls) (parse-body body)
		      (make-instance ',(find-symbol (string name) '#:sandalphon.so)
				     :vars vars
				     :vals vals
				     :declarations decls
				     :forms (objectify-forms forms env)))))))
	   (define-variable-binder (name)
	     `(define-binder ,name
		  (if (consp bind) (first bind) bind) (if (consp bind) (second bind) nil)
		  augment-symbol-macros objectify))
	   (define-function-binder (name)
	     `(define-binder ,name
		  (first bind) (cons 'lambda (rest bind))
		  augment-macros objectify-lambda)))
  (define-variable-binder let)
  (define-variable-binder let*)
  (define-function-binder flet)
  (define-function-binder labels))

(define-simple-objectifier progv (vars vals . forms))

(define-so-objectifier locally env (&body body)
  (multiple-value-bind (forms decls) (parse-body body)
    (make-instance 'sandalphon.so:locally
		   :declarations decls
		   :forms (objectify-forms forms env))))

(define-simple-objectifier progn forms)
(define-simple-objectifier multiple-value-prog1 (form . forms))
(define-simple-objectifier unwind-protect (form . forms))

(define-simple-objectifier quote ('thing)) ; subtly different from %literal, just trust me

(defclass named-lambda-function (lambda-function) ;bleh again with the pkg locks
  ((name :accessor name :initarg :name)))

(defun objectify-named-lambda (name params forms env)
  (make-instance 'named-lambda-function
		 :name name
		 :params params
		 :forms (objectify-forms forms env)))

(define-so-objectifier function env (thing)
  (make-instance 'sandalphon.so:function
		 :thing (typecase thing
			  (lambda-expression (objectify-lambda thing env))
			  #+sbcl
			  ((cons (member sb-int:named-lambda) *)
			   ;;(objectify `(labels ((,(second thing) ,@(cddr thing))) (function ,(second thing))) env))
			   ;; that's wrong, in that (sb-int:named-lambda ...) expands to #'(sb-int:named-lambda)
			   ;; so you end up with (function (labels ...)) which is wrong.
			   ;; instead, let's just throw in named lambdas, why the hell not?
			   (objectify-named-lambda (second thing) (third thing) (cdddr thing) env))
			  (t thing))))

(define-so-objectifier if env (test consequent &optional else)
  (make-instance 'sandalphon.so:if :test
                 (objectify test env) :consequent
                 (objectify consequent env) :else
                 (objectify else env)))
(define-simple-objectifier setq setq-params)
(define-simple-objectifier eval-when ('situations . forms))
(define-simple-objectifier multiple-value-call (func . arguments))
