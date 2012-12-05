(in-package #:sandalphon)

(defun traverse-graph.preorder (function graph traverse &key (key #'identity) (test #'eql))
  "Traverse an object representing a graph, calling FUNCTION on each node
and using TRAVERSE to get the list of nodes a given node connects to.
Each node is only traversed once, and in preorder.
Returns NIL.

FUNCTION is a function of one argument.
GRAPH is an object.
TRAVERSE is a function that, given a node object, returns a list of nodes that node connects to.
KEY is called on each node to get the value that FUNCTION is actually called on.
TEST is used to compare nodes for equality."
  (let ((stack (list graph))
	(done nil))
    (loop (mapc (compose function key) stack)
       (setf done (append stack done)
	     stack (remove-if (lambda (node)
				(member node done :test test))
			      (mappend traverse stack)))
       (when (null stack)
	 (return-from traverse-graph.preorder)))))

(defun find-in-graph.preorder (item graph traverse &key (key #'identity) (test #'eql))
  ;; man.  figure out a better way to do test and all (with the general traversal function), this is bad
  (traverse-graph.preorder (lambda (node)
			     (let ((thing (funcall key node)))
			       (when (funcall test item thing)
				 (return-from find-in-graph.preorder thing))))
			   graph
			   traverse))

(defun assoc-in-graph.preorder (item graph traverse &key (key #'identity) (test #'eql))
  ;; jesus christ this sucks
  ;; like holy shit
  (traverse-graph.preorder (lambda (node)
			     (let ((assoc (assoc item (funcall key node) :test test)))
			       (when assoc
				 (return-from assoc-in-graph.preorder assoc))))
			   graph
			   traverse))