(in-package #:sandalphon.types)

(defclass bounded-type (type-object)
  ((order :accessor bt-order :initarg :order)
   (equal :accessor bt-equality :initarg :equal)
   (lower :accessor bt-lower :initarg :lower
	  :initform '*)
   (lower-inclusive-p :accessor bt-lower-inclusive-p :initarg :lower-inclusive
		      :initform t)
   (greater :accessor bt-greater :initarg :greater
	    :initform '*)
   (greater-inclusive-p :accessor bt-greater-inclusive-p :initarg :greater-inclusive
			:initform t)))

(defun order= (t1 t2)
  (and (eql (bt-order t1) (bt-order t2))
       (eql (bt-equality t1) (bt-equality t2))))

(defun pred-min (order object &rest more-objects)
  (let ((min object))
    (dolist (object more-objects min)
      (when (funcall order object min)
	(setq min object)))))

(defun pred-max (order object &rest more-objects)
  (let ((max object))
    (dolist (object more-objects max)
      (when (funcall order max object)
	(setq max object)))))

(defmethod typep (object (type bounded-type))
  (with-accessors ((order bt-order) (equal bt-equality)
		   (lower bt-lower) (greater bt-greater)
		   (lower-inclusive bt-lower-inclusive-p) (greater-inclusive bt-greater-inclusive-p))
      type
    (and (if (not (eql lower '*)) ; infinity
	     (or (and lower-inclusive (funcall equal lower object))
		 (funcall order lower object))
	     t)
	 (if (not (eql greater '*))
	     (or (and greater-inclusive (funcall equal object greater))
		 (funcall order object greater))
	     t))))

(defmethod subtypep ((t1 bounded-type) (t2 bounded-type))
  (and (order= t1 t2)
       (or (eql (bt-lower t2) '*)
	   (funcall (bt-order t1) (bt-lower t2) (bt-lower t1))
	   (and (funcall (bt-equality t1) (bt-lower t1) (bt-lower t2))
		(or (bt-lower-inclusive-p t2)
		    (not (bt-lower-inclusive-p t1)))))
       (or (eql (bt-greater t2) '*)
	   (funcall (bt-order t1) (bt-greater t1) (bt-greater t2))
	   (and (funcall (bt-equality t1) (bt-greater t1) (bt-greater t2))
		(or (bt-greater-inclusive-p t2)
		    (not (bt-greater-inclusive-p t1)))))))

(defmethod union/2 ((t1 bounded-type) (t2 bounded-type))
  (if (not (order= t1 t2))
      (call-next-method) ; nothing we can do if they're not the same order
      (make-instance 'bounded-type
		     :equal (bt-equality t1)
		     :order (bt-order t1)
		     :lower (if (or (eql (bt-lower t1) '*) (eql (bt-lower t2) '*))
				'*
				(pred-min (bt-order t1) (bt-lower t1) (bt-lower t2)))
		     :lower-inclusive (cond ((or (eql (bt-lower t1) '*) (eql (bt-lower t2) '*))
					     ;; doesn't matter
					     nil)
					    ((funcall (bt-order t1) (bt-lower t1) (bt-lower t2))
					     (bt-lower-inclusive-p t1))
					    ((funcall (bt-equality t1) (bt-lower t1) (bt-lower t2))
					     (or (bt-lower-inclusive-p t1) (bt-lower-inclusive-p t2)))
					    (t (bt-lower-inclusive-p t2)))
		     :greater (if (or (eql (bt-greater t1) '*) (eql (bt-greater t2) '*))
				  '*
				  (pred-max (bt-order t1) (bt-greater t1) (bt-greater t2)))
		     :greater-inclusive (cond ((or (eql (bt-greater t1) '*) (eql (bt-greater t2) '*))
					       nil)
					      ((funcall (bt-order t1) (bt-greater t1) (bt-greater t2))
					       (bt-greater-inclusive-p t1))
					      ((funcall (bt-equality t1) (bt-greater t1) (bt-greater t2))
					       (or (bt-greater-inclusive-p t1) (bt-greater-inclusive-p t2)))
					      (t (bt-greater-inclusive-p t2))))))


(defmethod intersection/2 ((t1 bounded-type) (t2 bounded-type))
  (if (not (order= t1 t2))
      (call-next-method) ; nothing we can do if they're not the same order
      (make-instance 'bounded-type
		     :equal (bt-equality t1)
		     :order (bt-order t1)
		     :lower (cond ((eql (bt-lower t1) '*) (bt-lower t2))
				  ((eql (bt-lower t2) '*) (bt-lower t1))
				  (t (pred-max (bt-order t1) (bt-lower t1) (bt-lower t2))))
		     :lower-inclusive (cond ((eql (bt-lower t1) '*)
					     ;; doesn't really matter
					     (bt-lower-inclusive-p t2))
					    ((eql (bt-lower t2) '*)
					     (bt-lower-inclusive-p t1))
					    ((funcall (bt-order t1) (bt-lower t1) (bt-lower t2))
					     (bt-lower-inclusive-p t2))
					    ((funcall (bt-equality t1) (bt-lower t1) (bt-lower t2))
					     (and (bt-lower-inclusive-p t1) (bt-lower-inclusive-p t2)))
					    (t (bt-lower-inclusive-p t1)))
		     :greater (cond ((eql (bt-greater t1) '*) (bt-greater t2))
				    ((eql (bt-greater t2) '*) (bt-greater t1))
				    (t (pred-min (bt-order t1) (bt-greater t1) (bt-greater t2))))
		     :greater-inclusive (cond ((eql (bt-greater t1) '*)
					       (bt-greater-inclusive-p t2))
					      ((eql (bt-greater t2) '*)
					       (bt-greater-inclusive-p t1))
					      ((funcall (bt-order t1) (bt-greater t1) (bt-greater t2))
					       (bt-greater-inclusive-p t2))
					      ((funcall (bt-equality t1) (bt-greater t1) (bt-greater t2))
					       (and (bt-greater-inclusive-p t1) (bt-greater-inclusive-p t2)))
					      (t (bt-greater-inclusive-p t1))))))
