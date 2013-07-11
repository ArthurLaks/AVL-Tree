(defun addarray (array)
  (if (= (length array) 0) 0
    (+ (aref array 0) (addarray (subseq array 1)))))
