(defun make-tree (init-value &optional left-branch right-branch)
"Creates a tree with the specified value for the root node.  It takes two 
optional arguments for the sub-nodes, and calculates the height based on their
heights."
  (cons (cons init-value (1+ (max (height left-branch) (height right-branch))))
	(cons left-branch right-branch)))
(defun height (tree)
"Returns the height of the tree."
;;;The purpose of this function is to return zero when the tree is nil.  
;;;This prevents empty nodes from causing errors.
  (if (null tree)
      0
    (cdar tree)))
(defun insert-value (tree value)
  "Inserts a value into the tree.  Returns a new tree with the value inserted
to it."
;;;Returns a node consisting of the parameter with the value inserted into 
;;;one of its children.
  (let ((node-value (caar tree)))
    ;;If the tree is nil then the previous level of recursion already found
    ;;the place to insert the value, so return a node containing it.
    (if (null tree)
	(make-tree value)
      ;;If the value being inserted is less than the value of the node then the
      ;;left child should be the node's left child with the value inserted
      ;;in it if value is less than node-value.  Otherwise, it is the left 
      ;;node without anything inserted to it.
      (let ((left-child
	     (if (< value node-value) (insert-value (cadr tree) value)
	       (cadr tree)))
	    (right-child 
	     (if (< value node-value) (cddr tree)
	       (insert-value (cddr tree) value))))
	;;Call the balance function to balance the resulting node.
	(balance (make-tree node-value left-child right-child))))))
(defun balance (tree)
"Rotates the tree if it is disbalanced."
(let ((left-subtree (cadr tree)) (right-subtree (cddr tree)))
  (cond
   ;;If the left-subtree is deeper than the right subtree.
   ((> (- (height left-subtree) (height right-subtree)) 1)
    ;;Rotate the tree to the right.  The original root becomes the right node
    ;;with its left sub-tree its left node's right-subtree (in the original
    ;;tree, it contained values less than the root and greater than its left
    ;;child).
    (let ((new-right (make-tree (caar tree) (cddr left-subtree) right-subtree)))
      ;;The new root should be what was the left node, with its left node 
      ;;unchanged and its right node the former root (new-right).
      (make-tree (caar left-subtree) (cadr left-subtree) new-right)))
   ;;If the right node is deeper than the left node.
   ((> (- (height right-subtree) (height left-subtree)) 1)
    (let ((new-left (make-tree (caar tree) left-subtree (cadr right-subtree))))
      (make-tree (caar right-subtree) new-left (cddr right-subtree))))
   ;;If neither condition was true, the tree is balanced and return it 
   ;;unchanged.
   (t tree))))
(defun contains-value (node value)
"Returns t if the tree contains the value, otherwise nil."
  (let ((root-value (caar node)))
    (cond
     ;;If the node is nil then the value is not there.
     ((null root-value) nil)
     ((= value root-value) t)
     ((< value root-value) (contains-value (cadr node) value))
     ((> value root-value) (contains-value (cddr node) value)))))
(defun lowest (tree)
"Returns the lowest value of the tree (its left-most node)."
  (if (null (cadr tree))
      (caar tree)
    (lowest (cadr tree))))
"Returns the highest value of the tree (its right-most node)."
(defun highest (tree)
  (if (null (cddr tree))
      (caar tree)
    (highest (cddr tree))))
(defun dump (tree &optional (indentation 0))
"Dumps the tree by printing the value of each node and dumping its left and
right sub-trees with greater indent (this is effectively a pre-order 
transversal)."
	(if (null tree)
	    ;;For a blank node, return an empty literal.
		""
	(format nil "~% ~A ~D (~D) ~A ~A"
		;;A string with a certain number of tabs.
		(make-string indentation :initial-element #\tab)
		(caar tree)
		(cdar tree)
		(dump (cadr tree) (1+ indentation))
		(dump (cddr tree) (1+ indentation)))))
(defun delete-value (tree element)
"Returns the tree with the element deleted as the first return value and a 
t or nil depending on whether the value exists as the second return value."
  (let ((node-value (caar tree)) (left-node (cadr tree))
	(right-node (cddr tree)))
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
			   (values (balance (make-tree node-value children right-node))
				   succeeded)))
     ((> element node-value)
      (multiple-value-bind (children succeeded)
			   (delete-value right-node element)
			   (values (balance (make-tree node-value left-node children))
				   succeeded)))
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
	(t (let* ((succ-value (lowest right-node))
		  (children (delete-value right-node succ-value)))
	     (balance (make-tree succ-value left-node children)))))
       ;;Return t as the second return value to show that the deletion 
       ;;succeeded.
       t)))))
