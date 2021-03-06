;;; polystr-ga.lisp Apply GAs to polyhedron structures

(require 'ga "ga")
(require 'utils "utils")

(defun random-poly-str-genome (length)
  (let ((genome (make-array (list length))))
    (dotimes (i length genome)
      (setf (aref genome i) (random 3)))))

(defun random-typed-poly-str-genome (length)
  (let ((genome (make-array (list length))))
    ; ensure that there is at least one vertex, one edge, and one face
    (setf (aref genome 0) 0)
    (setf (aref genome 1) 1)
    (setf (aref genome 2) 2)
    ; fill out the rest of the genome randomly
    (dotimes (i (- length 3))
      (setf (aref genome (+ i 3)) (random 3)))
    ; scramble the whole mess, so that the presence of the initial
    ; segment (0 1 2) doesn't mean anything
    (random-permutation genome)))

(defun random-fixed-cardinality-poly-str-genome (num-vertices num-edges num-faces)
  (let ((genome (make-array (list (+ num-vertices num-edges num-faces)))))
    (dotimes (i num-vertices)
      (setf (aref genome i) 0))
    (dotimes (i num-edges)
      (setf (aref genome (+ num-vertices i)) 1))
    (dotimes (i num-faces)
      (setf (aref genome (+ num-vertices num-edges i)) 2))
    genome))

(defun random-binary-array (dim)
  (let ((m (make-array (list dim dim))))
    (dotimes (i dim m)
      (dotimes (j dim)
	(setf (aref m i j) (random 2))))))

(defun random-poly-str (total-cardinality)
  (make-poly-str (random-poly-str-genome total-cardinality)
		 (random-binary-array total-cardinality)))

(defun random-typed-poly-str (total-cardinality)
  (make-poly-str (random-typed-poly-str-genome total-cardinality)
		 (random-binary-array total-cardinality)))

(defun random-fixed-cardinality-poly-str (num-vertices num-edges num-faces)
  (make-poly-str (random-fixed-cardinality-poly-str-genome num-vertices
							   num-edges
							   num-faces)
		 (random-binary-array (+ num-vertices num-edges num-faces))))

(defun mutate-poly-str (poly-str)
  (if (zerop (random 2)) ; mutate the type vector
      (let* ((genome (type-vector poly-str))
	     (card (cardinality poly-str))
	     (new-genome (make-array (list card))))
	(dotimes (i card)
	  (setf (aref new-genome i) (aref genome i)))
	(let ((random-index (random card)))
	  (let ((type (aref genome random-index)))
	    (if (= type 0)
		(setf (aref new-genome random-index) 1)
		(if (= type 1)
		    (setf (aref new-genome random-index) 2)
		    (setf (aref new-genome random-index) 0)))
	    (make-poly-str new-genome (incidence-matrix poly-str)))))
      (let* ((matrix (incidence-matrix poly-str))
	     (card (cardinality poly-str))
	     (new-matrix (make-array (list card card))))
	(dotimes (i card)
	  (dotimes (j card)
	    (setf (aref new-matrix i j)
		  (aref matrix i j))))
	(let ((random-x (random card))
	      (random-y (random card)))
	  (if (zerop (aref matrix random-x random-y))
	      (setf (aref new-matrix random-x random-y) 1)
	      (setf (aref new-matrix random-x random-y) 0))
	  (make-poly-str (type-vector poly-str) new-matrix)))))

(defun mutate-fixed-cardinality-poly-str (poly-str)
  (let* ((matrix (incidence-matrix poly-str))
	 (card (cardinality poly-str))
	 (new-matrix (make-array (list card card))))
    (dotimes (i card)
      (dotimes (j card)
	(setf (aref new-matrix i j)
	      (aref matrix i j))))
    (let ((random-x (random card))
	  (random-y (random card)))
      (if (zerop (aref matrix random-x random-y))
	  (setf (aref new-matrix random-x random-y) 1)
	  (setf (aref new-matrix random-x random-y) 0))
      (make-poly-str (type-vector poly-str) new-matrix))))

(defun crossover-point (poly-str-1 poly-str-2)
  (declare (ignore poly-str-2))
  (let ((n (first (array-dimensions poly-str-1))))
    (cons (random n) (random n))))

(defun meld-matrices (matrix-1 matrix-2 x y)
  (let ((dim (car (array-dimensions matrix-1))))
    (let ((matrix (make-array (list dim dim))))
      (dotimes (i dim matrix)
	(dotimes (j dim)
	  (if (< i x)
	    (if (< j y)
		(setf (aref matrix i j)
		      (aref matrix-1 i j))
		(setf (aref matrix i j)
		      (aref matrix-2 i j)))
	    (if (< j y)
		(setf (aref matrix i j)
		      (aref matrix-2 i j))
		(setf (aref matrix i j)
		      (aref matrix-1 i j)))))))))

(defun meld-genomes (genome-1 genome-2 x)
  (let ((len (length genome-1)))
    (let ((new-genome (make-array (list len))))
      (dotimes (i len new-genome)
	(if (< i x)
	    (setf (aref new-genome i)
		  (aref genome-1 i))
	    (setf (aref new-genome i)
		  (aref genome-2 i)))))))

(defun combine-poly-str (poly-str-1 poly-str-2)
  (let ((genome-1 (type-vector poly-str-1))
	(genome-2 (type-vector poly-str-2))
	(crossover-index (random (cardinality poly-str-1)))
	(matrix-1 (incidence-matrix poly-str-1))
	(matrix-2 (incidence-matrix poly-str-2)))
    (let ((new-genome (meld-genomes genome-1 genome-2 crossover-index)))
      (destructuring-bind (cross-x . cross-y)
	  (crossover-point matrix-1 matrix-2)
	(let ((m (meld-matrices matrix-1 matrix-2 cross-x cross-y)))
	  (make-poly-str new-genome m))))))

(defun combine-fixed-cardinality-poly-str (poly-str-1 poly-str-2)
  (let ((matrix-1 (incidence-matrix poly-str-1))
	(matrix-2 (incidence-matrix poly-str-2))
	(genome (type-vector poly-str-1)))
    (destructuring-bind (cross-x . cross-y)
	(crossover-point matrix-1 matrix-2)
      (let ((m (meld-matrices matrix-1 matrix-2 cross-x cross-y)))
	(make-poly-str genome m)))))

(defun poly-str-fitness (poly-str)
  (let ((n 0))
    (dolist (ax steinitz-rademacher-axioms n)
      (when (true? ax poly-str nil)
	(incf n)))))

(defun typed-poly-str-fitness (poly-str)
  (let ((n 0))
    (dolist (ax steinitz-rademacher-axioms-typed n)
      (when (true? ax poly-str nil)
	(incf n)))))

(defun typed-poly-str-simplicial-fitness (poly-str)
  (let ((n 0))
    (dolist (ax steinitz-rademacher-axioms-typed-simplicial n)
      (when (true? ax poly-str nil)
	(incf n)))))

(defun poly-str-weighted-fitness (poly-str)
  (let ((weighted-axioms
	 '(
	   ((and (exists (?x V) (top))
	         (exists (?x E) (top))
	         (exists (?x F) (top))) . 5)
	   ((all (?x V) (all (?y V) (not (I ?x ?y)))) . 2)
	   ((all (?x E) (all (?y E) (not (I ?x ?y)))) . 2)
	   ((all (?x F) (all (?y F) (not (I ?x ?y)))) . 2)
	   ((all (?x V)
	     (all (?y E)
	   	  (all (?z F)
	   	       (implies 
	   		(and (I ?x ?y) (I ?y ?z))
	   		(I ?x ?z))))) . 1)
	   ((all (?e E)
	     (exists (?v1 V)
		     (exists (?v2 V)
			     (and (not (= ?v1 ?v2))
				  (I ?v1 ?e)
				  (I ?v2 ?e)
				  (all (?v V)
				       (implies (I ?v ?e)
						(or (= ?v ?v1)
						    (= ?v ?v2)))))))) . 4)
	   ((all (?e E)
	     (exists (?f1 F)
		     (exists (?f2 F)
			     (and (not (= ?f1 ?f2))
				  (I ?e ?f1)
				  (I ?e ?f2)
				  (all (?f F)
				       (implies (I ?e ?f)
						(or (= ?f1 f)
						    (= ?f2 f)))))))) . 4)
	   ((all (?v V) (exists (?e E) (I ?v ?e))) . 1)
	   ((all (?f F) (exists (?e E) (I ?e ?f))) . 1)))
	(fitness 0))
    (dolist (weighted-ax weighted-axioms fitness)
      (destructuring-bind (ax . weight) weighted-ax
	(when (true? ax poly-str nil)
	  (incf fitness weight))))))

(defun poly-str-ga (population-size num-generations cardinality)
  (let ((pop (random-population population-size
				#'(lambda ()
				    (random-poly-str cardinality)))))
    (dotimes (i num-generations pop)
      (setf pop (next-generation pop
				 #'poly-str-fitness
				 #'mutate-poly-str
				 #'combine-poly-str)))))

(defun typed-poly-str-ga (population-size num-generations cardinality)
  (let ((pop (random-population population-size
				#'(lambda ()
				    (random-poly-str cardinality)))))
    (dotimes (i num-generations pop)
      (format t "Generation ~S~%" i)
      (setf pop (next-generation pop 
				 #'typed-poly-str-fitness
				 #'mutate-poly-str
				 #'combine-poly-str)))))

(defun typed-simplicial-poly-str-ga (population-size num-generations cardinality)
  (let ((pop (random-population population-size
				#'(lambda ()
				    (random-poly-str cardinality)))))
    (dotimes (i num-generations pop)
      (format t "Generation ~S~%" i)
      (setf pop (next-generation pop 
				 #'typed-poly-str-simplicial-fitness
				 #'mutate-poly-str
				 #'combine-poly-str)))))

(defun typed-poly-str-weighted-ga (population-size num-generations cardinality)
  (let ((pop (random-population population-size
				#'(lambda ()
				    (random-typed-poly-str cardinality)))))
    (dotimes (i num-generations pop)
      (format t "Generation ~S~%" i)
      (setf pop (next-generation pop 
				 #'poly-str-weighted-fitness
				 #'mutate-poly-str
				 #'combine-poly-str)))))

(defun typed-fixed-cardinality-ga (num-vertices num-edges num-faces
				   population-size num-generations)
  (let ((pop (random-population population-size
				#'(lambda ()
				    (random-fixed-cardinality-poly-str
				     num-vertices num-edges num-faces)))))
    (dotimes (i num-generations pop)
      (format t "Generation ~S~%" i)
      (setf pop (next-generation pop 
				 #'poly-str-weighted-fitness
				 #'mutate-fixed-cardinality-poly-str
				 #'combine-fixed-cardinality-poly-str)))))

;;; polystr-ga.lisp ends here