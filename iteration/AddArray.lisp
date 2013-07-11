(defun addarray (array)
  (let ((sum 0)) 
    (dotimes (counter (length array) sum)
    (setf sum (+ sum (aref array counter))))))
