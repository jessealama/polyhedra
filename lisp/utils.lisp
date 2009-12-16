;;; utils.lisp

(provide 'utils)

(defun upto (n)
  "The list of integers from 0 to N, exclusive."
  (let (l)
    (dotimes (i n (reverse l))
      (push i l))))

(defun from-to (m n)
  "The list of integers starting with (i.e., including) M, going up to but exlcuding N."
  (when (<= m n)
    (let (l)
      (do ((i m (1+ i)))
	  ((= i n) (reverse l))
	(push i l)))))

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

(defun first-n (lst n)
  (when lst
    (do ((l nil)
	 (i 0 (1+ i))
	 (x (car lst) (car tail))
	 (tail (cdr lst) (cdr tail)))
	((= i n) (reverse l))
      (push x l))))

;;; utils.lisp ends here