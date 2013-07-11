(defun tostring (list)
	(if (= (length list) 0 ) ""
		(concatenate 'string (first list) (tostring (subseq list 1)))))