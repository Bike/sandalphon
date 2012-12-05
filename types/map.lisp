(defmacro doiterations ((&whole specs (macro-name seq) &rest more-specs) result &body body)
  (declare (ignore macro-name seq more-specs))
  (let ((macro-names (mapcar #'first specs))
	(seqs (mapcar #'second specs))
	(seq-vars (make-gensym-list (length specs) "SEQ"))
	(iter-vars (make-gensym-list (length specs) "ITER"))
	(done-label (gensym "DONE"))
	(top-label (gensym "TOP")))
    (flet ((make-seq-binding (seq-var seq) `(,seq-var ,seq))
	   (make-iterator-binding (iter-var seq-var)
	     ;; could extend to have from-end and subseq designators and shit
	     `(,iter-var (multiple-value-list (sb-sequence:make-simple-sequence-iterator ,seq-var))))
	   (make-element-macro (macro-name seq-var iter-var)
	     `(,macro-name (sb-sequence:iterator-element ,seq-var (first ,iter-var))))
	   (make-update (seq-var iter-var)
	     `(setf (first ,iter-var)
		    (destructuring-bind (state limit from-end) ,iter-var
		      (declare (ignore limit))
		      (sb-sequence:iterator-step ,seq-var state from-end))))
	   (make-endp-check (seq-var iter-var)
	     `(destructuring-bind (state limit from-end) ,iter-var
	       (when (sb-sequence:iterator-endp ,seq-var state limit from-end)
		 (go ,done-label)))))
    `(block nil
       (let (,@(mapcar #'make-seq-binding seq-vars seqs))
	 (let (,@(mapcar #'make-iterator-binding iter-vars seq-vars))
	   (tagbody
	      ,top-label
	      ;; actually.
	      ;; symbol macros are a hack here
	      ;; we need let-setf-expansion to deal with read = funcall elt and write = funcall setelt.
	      ;; of course even better would be the iterator protocol not returning all the damn functions.
	      ;; or no, you could still use let-setf-expansion if you wanted to cache the element access.
	      ;; no.  wait.  well you probably don't want to do that anyway, you want sets to hold.
	      ;; but you still need it so that accesses work right with the endp check.
	      ;; except you don't, I can just move up that check.
	      ;; @_@
	      ,@(mapcar #'make-endp-check seq-vars iter-vars)
	      (symbol-macrolet (,@(mapcar #'make-element-macro macro-names seq-vars iter-vars))
		,@body)
	      ,@(mapcar #'make-update seq-vars iter-vars)
	      (go ,top-label)
	      ,done-label
	      (return ,result))))))))

(defun map-to-circular (function &rest sequences)
  (let ((finitude nil)
	(acyclic nil)
	(cyclic 1))
    (map nil
	 (lambda (s)
	   (etypecase s
	     (list (multiple-value-bind (total nils acycle cycle)
		       (get-list-metrics s)
		     (declare (ignore total nils))
		     (cond ((zerop cycle)
			    (if finitude
				(setf finitude (min acycle finitude))
				(setf finitude acycle)))
			   (finitude) ;cyclic list but the result will be acyclic, so don't bother
			   (t (setf cyclic (lcm cyclic cycle))
			      (if acyclic
				  (setf acyclic (max acyclic acycle))
				  (setf acyclic acycle))))))
	     (sequence (setf finitude (length s)))))
	 sequences)
    (let* ((result (if finitude
		       (make-list finitude)
		       (append (make-list acyclic) (make-circular-list cyclic))))
	   (cons result))
      (dotimes (i (or finitude (+ acyclic cyclic)) result)
	(flet ((celt (seq) (elt seq i)))
	  (setf (car cons) (apply function (mapcar #'celt sequences))
		cons (cdr cons)))))))
