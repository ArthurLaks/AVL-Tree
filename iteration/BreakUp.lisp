(definef breakup (string) 
(let ((retval (quote ()))) 
(dotimes (counter (length string) retval)
(setf retval (cons (subseq string (- (length string) counter) 1))))))