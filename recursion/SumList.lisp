(defun sumlist (list)
"Returns the sum of the elements of the list"
  (if (= list '()) 0
    (+ (first list) (sumlist (subseq list 1)))))
