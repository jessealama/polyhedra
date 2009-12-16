;;; ga.lisp A simple framework for genenetic algorithms

(require 'poly-str "poly-str")
(require 'utils "utils")

(defun random-population (num-individuals random-individual-generator)
  (let ((pop nil))
    (dotimes (i num-individuals pop)
      (push (funcall random-individual-generator) pop))))

(defun best-individual (population fitness-function)
  (let ((pop (sort population #'(lambda (ind-1 ind-2)
				  (> (funcall fitness-function ind-1)
				     (funcall fitness-function ind-2))))))
    (first pop)))

(defun best-fitness (population fitness-function)
  (funcall fitness-function (best-individual population fitness-function)))

(defun next-generation (population fitness-function
			           mutation-function
			           combination-function)
  (let ((pop (sort population #'(lambda (ind-1 ind-2)
				  (> (funcall fitness-function ind-1)
				     (funcall fitness-function ind-2))))))
    (let* ((new-gen nil)
	   (num-inds (length pop))
	   (half (/ num-inds 2)))
      (let ((best-half (first-n pop half)))
	(dotimes (i half)
	  (let ((parent-index-1 (random half))
		(parent-index-2 (random half)))
	    (let ((parent-1 (nth parent-index-1 best-half))
		  (parent-2 (nth parent-index-2 best-half)))
	      (let ((new-ind (funcall combination-function parent-1 parent-2)))
		(while (zerop (random 20)) ; mutate type vector
		  (setf new-ind (funcall mutation-function new-ind)))
		(push new-ind new-gen)))))
	(append new-gen best-half)))))

;;; ga.lisp ends here