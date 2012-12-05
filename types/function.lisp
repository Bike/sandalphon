;;;; function.lisp

(in-package #:sandalphon.types)

(defstruct function-type-ll
  (required nil) ; sequence of types
  (optional nil) ; sequence of types
  (rest nil) ; type, or NIL
  (keys nil) ; sequence of (cons symbol type)
  (aok-p nil) ; whether other keys are allowed
)

(defun ll-subtypep (ll arg-types)
  (let ((required (function-type-ll-required ll))
	(optional (function-type-ll-optional ll))
	(rest (function-type-ll-rest ll))
	(keys (copy-list (function-type-ll-keys ll)))
	(aok-p (function-type-ll-aok-p ll))
	(surety t)
	(magic-seen (gensym "MAGIC-SEEN")))
    (flet ((stfail (t1 t2)
	     (tri/if (subtypep t1 t2)
		     t
		     (return-from ll-subtypep (values nil t))
		     (progn (setf surety nil) t)))
	   (finish ()
	     (cond (required (values nil t))
		   (surety (values t t))
		   (t (values nil nil))))
	   (get-value-type (keyword-type)
	     (let ((result *the-bottom-type*))
	       (dolist (key-spec keys)
		 (tri/if (subtypep keyword-type (parse-type `(member ',(car key-spec))))
			 (if (eq (cdr key-spec) magic-seen)
			     (setf surety nil)
			     (setf result (union (cdr key-spec) result)
				   (cdr key-spec) magic-seen))
			 nil
			 (if (eq (cdr key-spec) magic-seen)
			     (setf surety nil)
			     (setf result (union (cdr key-spec) result)
				   (cdr key-spec) magic-seen))))
	       (if (and (eq result *the-bottom-type*) aok-p)
		   *the-top-type*
		   result))))
      (loop for (type . rest-types) on arg-types
	 do (cond (required (stfail type (pop required)))
		  (optional (stfail type (pop optional)))
		  (keys (when rest
			  (stfail type rest)
			  (stfail (first rest-types) rest))
			(unless rest-types
			  (return-from ll-subtypep (values nil t)))
			(stfail (first rest-types) (get-value-type type))
			(setf rest-types (cdr rest-types)))
		  (rest (stfail type rest)))
	 finally (return (finish))))))

(defclass function-type (primitive-type)
  ((function :accessor function-type-function :initarg :function
	     :type function)))

;;; hm, this is wrong.
;;; e.g., an environment of only (X . T) may be inferrable, but that doesn't mean that (1+ x) is wrong.
;;; it does imply that after the 1+ x must be a NUMBER, though...

(defun make-simple-function-type-function (ll result-type)
  (lambda (&rest arg-types)
    (cond ((ll-subtypep ll arg-types) result-type)
	  (t (signal "argument mismatch")
	     *the-bottom-type*))))

;;; example: type of MAP

;;; maybe it can just be derived?

(declaim (inline map-for-effect))
(defun map-for-effect (function sequences)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0))
	   (type function function)
	   (type list sequences))
  (let ((args (make-list (length sequences)))
	(iters (mapcar (lambda (seq)
			 (etypecase seq
			   (list seq)
			   (vector 0)
			   #-(and)
			   (sequence (values-list (make-sequence-iterator seq)))
			   ))
		       sequences)))
    (declare (dynamic-extent iters)) ; args-list can't be in general, because of for example (map foo #'identity ...)
    (loop (do ((sequences sequences (cdr sequences))
	       (args-list args (cdr args-list))
	       (iters iters (cdr iters)))
	      ((null sequences))
	    (typecase (car sequences)
	      (list (if (null (car iters))
			(return-from map-for-effect nil)
			(setf (car args-list) (car (car iters))
			      (car iters) (cdr (car iters)))))
	      (vector
	       (let ((i (car iters)))
		 (declare (type (integer 0 #.array-dimension-limit) i))
		 (if (array-in-bounds-p (car sequences) i)
		     (setf (car args-list) (aref (car sequences) i)
			   (car iters) (1+ i))
		     (return-from map-for-effect nil))))
	      #-(and)
	      (sequence
	       (destructuring-bind (iterator limit from-end step endp elt setelt index copy)
		   (car iters)
		 (declare (ignore setelt index copy))
		 (if (funcall endp (car sequences) iterator limit from-end)
		     (return-from map-for-effect nil)
		     (setf (car args-list) (funcall elt (car sequences) iterator)
			   (car (car iters)) (funcall step (car sequences) iterator from-end)))))))
       (apply function args))))

(declaim (inline get-map-length))
(declaim (ftype (function (list) (values (integer 0 (#.array-dimension-limit)) &optional)) get-map-length))
(defun get-map-length (seqs)
  (let ((i 0))
    (declare (type (integer 0 (#.array-dimension-limit)) i))
    (map-for-effect (lambda (&rest args) (declare (ignore args)) (incf i)) seqs)
    i))

(deftype type-specifier ()
  '(or symbol cons class))
(deftype function-designator () '(or symbol function))

(declaim (ftype (function (type-specifier function-designator sequence &rest sequence)
			  (values (or sequence null) &optional))
		my-map))
(defun my-map (result-type function sequence &rest sequences)
  (declare ;(inline map-for-effect) ;; hugination
	   (optimize (speed 3) (space 0) (compilation-speed 0)))
  (let ((function (ensure-function function))
	(sequences (cons sequence sequences)))
    (cond ((cl:subtypep result-type 'nil)
	   (map-for-effect function sequences))
	  ((cl:subtypep result-type 'list)
	   (let* ((result (list nil))
		  (altering result))
	     (map-for-effect (lambda (&rest args)
			       (setf (cdr altering) (list (apply function args))
				     altering (cdr altering)))
			     sequences)
	     (cdr result)))
	  ((cl:subtypep result-type 'vector)
	   (let* ((length (get-map-length sequences))
		  (result (make-sequence result-type length)))
	     (declare (type vector result))
	     (map-for-effect (let ((i 0))
			       (declare (type (integer 0 #.array-dimension-limit) i))
			       (lambda (&rest args)
				 (setf (elt result i) (apply function args)
				       i (1+ i))))
			     sequences)
	     result))
	  #-(and)
	  ((cl:subtypep result-type 'sequence)
	   (let* ((length (get-map-length sequences))
		  (result (make-sequence result-type length)))
	     (declare (type (and sequence (not list) (not vector)) result)) ; whoopidoo
	     (with-sequence-iterator (iterator limit from-end step endp elt setelt index copy)
		 (result)
	       (declare (ignore limit endp elt index copy))
	       (map-for-effect (lambda (&rest args)
				 (funcall setelt (apply function args) result iterator)
				 (setf iterator (funcall step result iterator from-end)))
			       sequences))
	     result)))))

;;; okay no, that's a bit much to derive, goddamn.
;;; or perhaps not.  in the vector branch RESULT is obviously a subtype of VECTOR (not altered by the lambda or anything).
;;; in the list branch RESULT is obviously going to be a list, though the length is a bit trickier...
;;; map-for-effect should be pretty easy to determine as returning (member nil) - SBCL did, certainly!

#-(and)
(defmethod my-map ((result-type (subtype lazy-cons)) function sequence &rest sequences)
  (let* ((function (ensure-function function))
	 (sequences (cons sequence sequences))
	 (iters (mapcar (lambda (s)
			  (etypecase s
			    (list s)
			    (vector 0)
			    (sequence (multiple-value-list (make-sequence-iterator s)))))
			sequences))
	 (args (make-list (length sequences))))
    (labels ((map-aux ()
	       (do ((sequences sequences (cdr sequences))
		    (iters iters (cdr iters))
		    (args-list args (cdr args-list)))
		   ((null sequences) (lazy-cons (apply function args) (map-aux)))
		 (typecase (car sequences)
		   (list (if (null (car iters))
			     (return-from map-aux nil)
			     (setf (car args-list) (car (car iters))
				   (car iters) (cdr (car iters)))))
		   (vector
		    (let ((i (car iters)))
		      (declare (type (integer 0 #.array-dimension-limit) i))
		      (if (array-in-bounds-p (car sequences) i)
			  (setf (car args-list) (aref (car sequences) i)
				(car iters) (1+ i))
			  (return-from map-for-effect nil))))
		   (sequence
		    (destructuring-bind (iterator limit from-end step endp elt setelt index copy)
			(car iters)
		      (declare (ignore setelt index copy))
		      (if (funcall endp (car sequences) iterator limit from-end)
			  (return-from map-for-effect nil)
			  (setf (car args-list) (funcall elt (car sequences) iterator)
				(car (car iters)) (funcall step (car sequences) iterator from-end)))))))))
      (map-aux))))

(lambda (result-type function &rest sequences)
  (macrolet ((cnot (values-form)
	       "Certainly not."
	       `(multiple-value-bind (truth surety) values-form (and (not truth) surety))))
    ;; handles '(member array) => #<ARRAY-TYPE ...>, as well as foo => #<RUNTIME-TYPE ...> or something
    (let ((type (lvar-value-type result-type)))
      (cond ((subtypep (parse-type 'sequence) type)
	     (signal "~a is not a _proper_ subtype of SEQUENCE" result-type)
	     *the-bottom-type*)
	    ((or (cnot (subtypep result-type 'sequence))
		 (some (lambda (type) (cnot (subtypep type 'sequence))) sequences))
	     (signal "type mismatch")
	     *the-bottom-type*)
	    (t (let* ((result-length (reduce #'array-dim-min sequences :key #'sequence-type-length))
		      (result-element-type (values-type-primary
					    (apply (function-type-function function)
						   (mapcar #'sequence-type-element-type sequences))))
		      (result (intersection (parse-type `(rectilinear ,result-element-type (,result-length)))
					    type)))
		 (when (subtypep result *the-bottom-type*)
		   (signal "type mismatch"))
		 result))))))
