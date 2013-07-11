(defun primep (toTest &optional (startFrom 2))
"Determines if the first parameter is prime by determining if it is divisible by all the numbers above the second, optional parameter, which is two by default"
  (cond ((= totest startfrom) t)	;In the base case, return true.
	((= (mod totest startfrom) 0) nil)	;If toTest is divisible by startfrom, return nil
	(t (primep totest (+ startfrom 1)))))	;If toTest is not divisible by startfrom (this is the equivalent of an else statement) return the return vallue
	;of primep starting from the next number.