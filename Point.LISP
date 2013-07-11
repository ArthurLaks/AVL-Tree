;This function is an attempt to replicate a class through closures. It returns a plisp with functions that operate on the parameters
(defun createPoint (&key (x 0) (y 0))
"Returns a plisp containing functions called getX, getY, theta, distance, setX, and setY that operate on the x and y values that are passed as parameters."
  (list
   :getX #'(lambda () x)
   :getY #'(lambda () y)
   :theta #'(lambda () (atan (/ y x)))
   :distance #'(lambda ()(sqrt (+ (expt x 2) (expt y 2))))
   :setX #'(lambda (newx) (setq x newx))
   :setY #'(lambda (newy) (setq y newy))))
