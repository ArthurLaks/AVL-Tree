(defun dump (tree &optional (indentation 0))
	(if (null tree)
		""
	(format nil "~A ~D ~% ~A ~% ~A"
		(make-string indentation :initial-element #\tab)
		(car tree)
		(dump (cadr tree) (+1 indentation))
		(dump (cddr tree) (+1 indentation)))))
(defun delete (tree element)
	(cond
		(< element (car tree)
			(delete (cadr tree) element))
		(> element (car tree)
			(delete (cddr tree) element))
		(t
			(add-value 