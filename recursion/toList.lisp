(defun toList (string)
(if (= (length string) 0) nil
(cons (subseq string 0 1) (toList (subseq string 1 (length string))))))
