;;;; pattern.lisp

;;;; Non-CL speculative pattern matching mechanism through the type system, to help me iron out how things should work.

(in-package #:sandalphon.types) ; maybe make another package to emphasize non-canonicity

;;; Default methods should be available for union and intersection, that just make types that dumbly check everything.
;;; e.g. default for union/2 should see if either's a subtype of the other,
;;;  and if not (or can't tell) return a generalized union.
;;; Default for subtypep should just be (values nil nil).

(defmethod subtypep ((t1 pattern-type) (t2 pattern-type))
  (labels ((starwalk (subpattern star)
	     (or (subpatternp star subpattern)
		 (and (consp subpattern)
		      (subpatternp (first subpattern) (second star))
		      (starwalk (rest subpattern) star))))
	   (subpatternp (p1 p2)
	     (etypecase p2
	       (null nil)
	       (symbol t)
	       (cons (and (consp p1)
			  (case (first p2)
			    ((quote) (and (eql (first p1) 'quote)
					  (eql (second p1) (second p2))))
			    ((star) (if (eql (first p1) 'star)
					(subpatternp (second p1) (second p2))
					(starwalk p1 p2)))
			    (otherwise (and (subpatternp (first p1) (first p2))
					    (subpatternp (rest p1) (rest p2))))))))))
    (subpatternp (pattern-type-pattern t1) (pattern-type-pattern t2))))
(defmethod 
