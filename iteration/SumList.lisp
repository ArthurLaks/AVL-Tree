(defun sumlist (list)
  (let ((sum 0)) 
    (dolist (cnumber list sum)
	    (setf sum (+ sum cnumber)))))
