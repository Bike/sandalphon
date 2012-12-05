;;;; parse.lisp

;;;; parse type specifiers into type objects.

(in-package #:sandalphon.types)

;;; TODO: Package this shit into environment arguments

(defvar *atomic-types* (make-hash-table))
(defvar *compound-type-makers* (make-hash-table))
(defvar *type-macros* (make-hash-table))

(defmacro define-atomic-parse (atomic-name class-name)
  `(setf (gethash ',atomic-name *atomic-types*)
	 (make-instance ',class-name)))

;;; TODO: Move parsers to appropriate files (e.g. cons in cons.lisp)

(define-atomic-parse cons cons-type)
(setf (gethash 't *atomic-types*) *the-top-type*)

(setf (gethash 'array *compound-type-makers*) 'parse-array)

(defun parse-array (form env)
  (let ((simplicity nil))
    (destructuring-bind (array &optional (element-type '*) (dimensions '*)) form
      (ecase array
	((array))
	((simple-array) (setf simplicity t)))
      (make-instance 'array-type
		     :element-type (if (eql element-type '*)
				       element-type ; special-case
				       ;; ideally the env argument would be passed to u-a-e-t,
				       ;; buuuuut the lack of standard environments strikes again
				       (parse-type (upgraded-array-element-type element-type) env))
		     :simple simplicity
		     :dimensions (canonicalize-ads dimensions)))))

(defun parse-vector (form env)
  (let ((simplicity nil))
    (destructuring-bind (vector &optional (element-type '*) (dimensions '*))
	form
      (ecase vector
	((vector))
	((simple-vector) (setf simplicity t)))
      (make-instance 'array-type
		     :element-type (if (eql element-type '*)
				       element-type ; special-case
				       ;; ideally the env argument would be passed to u-a-e-t,
				       ;; buuuuut the lack of standard environments strikes again
				       (parse-type (upgraded-array-element-type element-type) env))
		     :simple simplicity
		     :dimensions (canonicalize-ads (list dimensions))))))

(defun parse-special-vector (form env)
  (declare (ignore env))
  (let ((simplicity nil)
	(element-type nil))
    (destructuring-bind (name &optional (size '*))
	form
      (ecase name
	((string simple-string)
	 ;; the character type referred to by strings is env-independent, I think.
	 (setf element-type (make-instance 'array-element-union :type (parse-type 'character))))
	((bit-vector simple-bit-vector)
	 ;; ditto/mod above
	 (setf element-type (parse-type 'bit)))
	((base-string simple-base-string)
	 (setf element-type (parse-type 'base-char))))
      (case name
	((simple-string simple-bit-vector simple-base-string) (setf simplicity t)))
      (make-instance 'array-type
		     :simple simplicity
		     :element-type element-type
		     :dimensions (canonicalize-ads (list size))))))

(defun typexpand-1 (type-specifier &optional env)
  (let* ((possible-macro (if (listp type-specifier) (first type-specifier) type-specifier))
	 (macro-function (gethash possible-macro *type-macros*)))
    (if macro-function
	(values (funcall macro-function (ensure-list type-specifier) env) t)
	(values type-specifier nil))))

(defun typexpand (type-specifier &optional env) ; see above env-related todo
  (multiple-value-bind (expansion expanded)
      (typexpand-1 type-specifier env)
    (if expanded
	(values (typexpand expansion env) t)
	(values expansion nil))))

(defun parse-type (type-specifier &optional env)
  (let ((type-specifier (typexpand type-specifier env)))
    (etypecase type-specifier
      (class type-specifier)
      (symbol (or (gethash type-specifier *atomic-types*)
		  (error "unknown atomic type")))
      (cons (funcall (or (gethash (first type-specifier) *compound-type-makers*)
			 (error "unknown compound type"))
		     type-specifier env)))))
