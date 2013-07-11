(defun factor (number) (let ((factors (quote ()))) (dotimes (counter number factors) (if (= (mod number (+ counter 1)) 0) (setf factors (cons (+ counter 1)factors))))))
(print (factor 210))