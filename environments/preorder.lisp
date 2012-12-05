;;;; preorder.lisp

;;;; preorder traversal for graphs

(in-package #:sandalphon.environments)

(defun traverse (start-node function traverse-function &key loop)
  (let ((seen nil)
	(current (list start-node)))
    (catch 'cease-traversal
      (loop (when loop
	      (setf current (set-difference current seen)
		    seen (append current seen)))
	 (unless current (return))
	 (mapc function current)
	 (setf current (mappend traverse-function current))))))

(defun make-finder (function &key (key #'identity))
  (lambda (node)
    (let ((result (funcall function (funcall key node))))
      (if result
	  (throw 'cease-traversal result)
	  (values)))))
