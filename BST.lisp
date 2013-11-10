(defun make-tree (init-value)
"Creates a tree with the specified value for the root node."
  (cons init-value (cons nil nil)))
(defun insert-value (tree value)
"Inserts a value into the tree."
  (let ((node-value (car tree)))
    (cond
     ;;If the tree is nil then the previous level of recursion already found
     ;;the place to insert the value, so return a node containing it.
     ((null tree)
      (make-tree value))
     ;;If the value being inserted is less than the value to insert then 
     ;;insert it to the node's left child.
     ((< value node-value)
      ;;Return a cons cell to replace the current node consisting of the value
      ;;of the current node and another cons cell containing its children,
      ;;but with value inserted in its left child
      (cons node-value (cons (insert-value (cadr tree) value) (cddr tree))))
     ;;If the value being inserted is greater than the value of the current
     ;;node, then insert it to its right child.
     ((> value node-value)
      (cons node-value (cons (cadr tree) (insert-value (cddr tree) value))))
     ;;If neither of the previous conditions were true, the node-value is equal
     ;;to that of the current node, and it ignore it.
     (t tree))))
(defun contains-value (node value)
"Returns t if the tree contains the value, otherwise nil."
  (let ((root-value (car node)))
    (cond
     ;;If the node is nil then the value is not there.
     ((null root-value) nil)
     ((= value root-value) t)
     ((< value root-value) (contains-value (cadr node) value))
     ((> value root-value) (contains-value (cddr node) value)))))
(defun height (tree)
  (if (null tree)
      0
    ;;Return 1 plus the maximum of the left and right sub-trees.
    (1+ (max (height (cadr tree)) (height (cddr tree))))))
(defun lowest (tree)
  (if (null (cadr tree))
      (car tree)
    (lowest (cadr tree))))
(defun highest (tree)
  (if (null (cddr tree))
      (car tree)
    (highest (cddr tree))))
(defun dump (tree &optional (indentation 0))
"Dumps the tree by printing the value of each node and dumping its left and
right sub-trees with greater indent (this is effectively a pre-order 
transversal)."
	(if (null tree)
	    ;;For a blank node, return an empty literal.
		""
	(format nil "~% ~A ~D ~A ~A"
		;;A string with a certain number of tabs.
		(make-string indentation :initial-element #\tab)
		(car tree)
		(dump (cadr tree) (1+ indentation))
		(dump (cddr tree) (1+ indentation)))))
(defun delete-value (tree element)
"Returns the tree with the element deleted as the first return value and a 
t or nil depending on whether the value exists as the second return value."
  (let ((node-value (car tree)) 
	(left-node (cadr tree)) (right-node (cddr tree)))
    (cond
     ;;If the tree is nil then the element does not exist, so return nil as
     ;;the first value to the previous member of the recursion chain in order
     ;;to complete the tree and nil as the second return value in order to show
     ;;that the deletion failed.
     ((null tree)
      (values nil nil))
     ((< element node-value)
      ;;Call itself on the left branch, and pass down both of its return 
      ;;values.  
      (multiple-value-bind (children succeeded)
			   (delete-value left-node element)
			   (values (cons node-value
					 (cons children right-node)) succeeded)))
     ((> element node-value)
      (multiple-value-bind (children succeeded)
			   (delete-value right-node element)
			   (values (cons node-value
					 (cons left-node children)) succeeded)))
     (t
      ;;If none of the previous conditions were tree then element must be 
      ;;equal to node-value.  Delete that node by return a new tree with
      ;;its children but not the value of the node itself.
      (values
       ;;If the right node is nil then just return the left node.  If the left
       ;;node is nil, return the right node.  If both are occupied, replcace
       ;;the current node with its inorder successor (lowest right-node) and
       ;;keeps its left sub-tree and deletes the successor from the right
       ;;sub tree.
       (cond
	((null right-node)
	 left-node)
	((null left-node)
	 right-node)
	(t (let ((succ-value (lowest right-node)))
	     (cons succ-value
		   (cons left-node (delete-value right-node succ-value))))))
       ;;Return t as the second return value to show that the deletion 
       ;;succeeded.
       t)))))
