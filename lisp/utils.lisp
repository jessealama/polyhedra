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

;;; Array utilities

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) symbols)
     ,@body))

(defmacro do-square (x y dim &body body)
  `(dotimes (,x ,dim)
     (dotimes (,y ,dim)
       ,@body)))

(defun copy-square-matrix (matrix)
  (let* ((dim (array-dimensions matrix))
	 (m (make-array dim)))
    (do-square x y (car dim)
      (setf (aref m x y) (aref matrix x y)))
    m))

(defun invert (matrix i j) ;; this could be a macro -- see chapter 12 of On Lisp
  (let ((val (aref matrix i j)))
    (if (zerop val)
	(setf (aref matrix i j) 1)
	(setf (aref matrix i j) 0))
    matrix))

;;; utils.lisp ends here