(load "Programs/lisp/Iteration/Sumlist.lisp")
(defun average (&rest list)
  (/ (sumlist list) (length list)))
