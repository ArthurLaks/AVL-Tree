(defun apply-to-list (list function)
"As far as I can tell, this function is identical to mapcar"
  (let (retval) 
    (dolist (citem list retval)
	    (push (funcall function citem) retval))))
