;;;; classes.lisp

;;;; Defines the basic syntactic objects.

(in-package #:sandalphon.objectify)

(defmacro define-so-class (name slots)
  (let ((name (or (find-symbol (symbol-name name) (find-package '#:sandalphon.so)) name)))
    `(defclass ,name (special-operator)
       (,@(mapcar (lambda (sym) `(,sym :initarg ,(make-keyword sym) :accessor ,sym)) slots)))))

;;; symbol-macrolet and macrolet just parse into other forms, they don't need analysis objects.
;;; TODO: Figure out what the fuck (macrolet|symbol-macrolet (...) (declare ...) ...) means

;;; these are (very) roughly organized:

;;; control flow blocks

(define-so-class catch (tag forms))
(define-so-class block (tag forms))
(define-so-class tagbody (statements))

;;; things that take a form... or are control flow... uh... I dunno

(define-so-class the (values-type form))
(define-so-class throw (tag form))
(define-so-class return-from (tag form))
(define-so-class go (tag))

;;; bindings

(define-so-class let (vars vals declarations forms))
(define-so-class let* (vars vals declarations forms))
(define-so-class flet (vars vals declarations forms))
(define-so-class labels (vars vals declarations forms))
(define-so-class progv (vars vals forms))

;;; things with bodies

(define-so-class locally (declarations forms))
(define-so-class progn (forms))
(define-so-class multiple-value-prog1 (form forms))
(define-so-class unwind-protect (form forms)) 

;;; things with things

(define-so-class quote (thing))
(define-so-class function (thing))

;;; everything else

(define-so-class if (test consequent else))
;(define-so-class load-time-value (form read-only-p)) ; part of minimal compilation, which this implements
(define-so-class setq (setq-params))
(define-so-class eval-when (situations forms))
(define-so-class multiple-value-call (func arguments))

;;; ours

(define-so-class %variable (thing)) ; like FUNCTION, but for the variable namespace
(define-so-class %literal (thing read-only-p))
(define-so-class %funcall (func arguments)) ; single-value-call, as it were
