;;; utils.lisp

(defmacro while (test &body body)
  `(do nil ((not ,test)) ,@body))

(defun random-permutation (vector)
  (let ((len (length vector)))
    (dotimes (i len vector)
      (dotimes (j len)
	(when (= (random 2) 0)
	  (let ((x (aref vector i))
		(y (aref vector j)))
	    (setf (aref vector i) y)
	    (setf (aref vector j) x)))))))

;;; utils.lisp ends here