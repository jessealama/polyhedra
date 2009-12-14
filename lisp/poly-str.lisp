;;; Polyhedra as incidence structures

(defun negation? (formula)
  (eq (car formula) 'not))

(defun unnegate (negation)
  (second negation))

(defun implication? (formula)
  (eq (car formula) 'implies))

(defun antecedent (implication)
  (cadr implication))

(defun consequent (implication)
  (caddr implication))

(defun equivalence? (formula)
  (eq (car formula) 'iff))

(defun lhs (formula)
  (cadr formula))

(defun rhs (formula)
  (caddr formula))

(defun disjunction? (formula)
  (eq (car formula) 'or))

(defun disjuncts (disjunction)
  (cdr disjunction))

(defun conjunction? (formula)
  (eq (car formula) 'and))

(defun conjuncts (conjunction)
  (cdr conjunction))

(defun universal? (formula)
  (eq (car formula) 'all))

(defun existential? (formula)
  (eq (car formula) 'exists))

(defun matrix (quantified-formula)
  (caddr quantified-formula))

(defun bound-variable (quantified-formula)
  (cadr quantified-formula))

(defun bare-variable? (variable)
  (symbolp variable))

(defun typed-variable? (variable)
  (not (bare-variable? variable)))

(defun variable-type (typed-variable)
  (second typed-variable))

(defun variable-name (typed-variable)
  (first typed-variable))

(defun v-statement? (formula)
  (eq (car formula) 'V))

(defun e-statement? (formula)
  (eq (car formula) 'E))

(defun f-statement? (formula)
  (eq (car formula) 'F))

(defun equation? (formula)
  (eq (car formula) '=))

(defun unary-statement-argument (unary-statement)
  (cadr unary-statement))

(defun binary-statement-first-arg (binary-statement)
  (cadr binary-statement))

(defun binary-statement-second-arg (binary-statement)
  (caddr binary-statement))

;;; Structures

(defun type-vector (poly-str)
  (second poly-str))

(defun cardinality (poly-str)
  (first poly-str))

(defun domain (poly-str)
  (upto (cardinality poly-str)))

(defun vertex-range (poly-str)
  (let ((tv (type-vector poly-str))
	(vertices nil))
    (dotimes (i (cardinality poly-str) vertices)
      (when (= (aref tv i) 0)
	(push i vertices)))))

(defun edge-range (poly-str)
  (let ((tv (type-vector poly-str))
	(edges nil))
    (dotimes (i (cardinality poly-str) edges)
      (when (= (aref tv i) 1)
	(push i edges)))))

(defun face-range (poly-str)
  (let ((tv (type-vector poly-str))
	(faces nil))
    (dotimes (i (cardinality poly-str) faces)
      (when (= (aref tv i) 2)
	(push i faces)))))

(defun variable-domain (variable-type poly-str)
  (if (eq variable-type 'V)
      (vertex-range poly-str)
      (if (eq variable-type 'E)
	  (edge-range poly-str)
	  (face-range poly-str))))

(defun incidence-matrix (poly-str)
  (third poly-str))

(defun polyhedron-structure? (x)
  (and (listp x)
       (not (null (nthcdr 0 x)))
       (not (null (nthcdr 1 x)))
       (not (null (nthcdr 2 x)))
       (not (null (nthcdr 3 x)))
       (integerp (nth 0 x)) ;domain size
       (not (zerop (nth 0 x))) ;; greater than 1
       (list (nth 1 x)) ; interpretation of V
       (list (nth 2 x)) ; interpretation of E
       (list (nth 3 x)) ; interpretation of F
       (arrayp (nth 4 x)) ;; incidence matrix
       (let ((n (nth 0 x))
	     (V (nth 1 x))
	     (E (nth 2 x))
	     (F (nth 3 x))
	     (I (nth 4 x)))
	 (and (every #'(lambda (y) (and (integerp y)
					(<= 0 y)
					(< y n)))
		     V)
	      (every #'(lambda (y) (and (integerp y)
					(<= 0 y)
					(< y n)))
		     E)
	      (every #'(lambda (y) (and (integerp y)
					(<= 0 y)
					(< y n)))
		     F)
	      (equal (array-dimensions I) (list n n))))))

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

(defun make-poly-str (genome incidence-matrix)
  (list (length genome)
	genome
	incidence-matrix))

(defun num-vertices (poly-str)
  (length (vertex-range poly-str)))

(defun num-edges (poly-str)
  (length (edge-range poly-str)))

(defun num-faces (poly-str)
  (length (face-range poly-str)))

(defun incidence-matrix (poly-str)
  (fifth poly-str))

;;; Assignments

(defun assignment-value (var assignment)
  (cdr (assoc var assignment)))

(defun update-assignment (var assignment val)
  (let ((existing (assoc var assignment)))
    (if existing
	(rplacd existing val)
	(cons (cons var val) assignment))))

(defun true? (formula poly-str assign)
  "Is FORMULA true in POLY-STR with respect to ASSIGN?"
  (cond ((negation? formula)
	 (not (true? (unnegate formula) poly-str assign)))
	((implication? formula)
	 (or (not (true? (antecedent formula) poly-str assign))
	     (true? (consequent formula) poly-str assign)))
	((equivalence? formula)
	 (or (and (true? (lhs formula) poly-str assign)
		  (true? (rhs formula) poly-str assign))
	     (and (not (true? (lhs formula) poly-str assign))
		  (not (true? (rhs formula) poly-str assign)))))
	((disjunction? formula)
	 (some #'(lambda (disjunct)
		   (true? disjunct poly-str assign))
	       (disjuncts formula)))
	((conjunction? formula)
	 (every #'(lambda (conjunct)
		    (true? conjunct poly-str assign))
		(conjuncts formula)))
	((universal? formula)
	 (let ((matrix (matrix formula))
	       (var (bound-variable formula)))
	   (if (typed-variable? var)
	       (let ((var-type (variable-type var))
		     (var-name (variable-name var)))
		 (every #'(lambda (a)
			    (true? matrix
				   poly-str 
				   (update-assignment var-name assign a)))
			(variable-domain var-type poly-str)))
	       (every #'(lambda (a)
			  (true? matrix 
				 poly-str
				 (update-assignment var assign a)))
		      (domain poly-str)))))
	((existential? formula)
	 (let ((matrix (matrix formula))
	       (var (bound-variable formula)))
	   (if (typed-variable? var)
	       (let ((var-type (variable-type var))
		     (var-name (variable-name var)))
		 (some #'(lambda (a)
			   (true? matrix
				  poly-str
				  (update-assignment var-name assign a)))
		       (variable-domain var-type poly-str)))
	       (some #'(lambda (a)
			 (true? matrix
				poly-str
				(update-assignment var assign a)))
		     (domain poly-str)))))
	(t ;; atomic case
	 (cond ((equation? formula)
		(let ((var-1 (lhs formula))
		      (var-2 (rhs formula)))
		  (let ((val-1 (assignment-value var-1 assign))
			(val-2 (assignment-value var-2 assign)))
		    (eq val-1 val-2))))
	       ((v-statement? formula)
		(let ((var (unary-statement-argument formula))
		      (ran (vertex-range poly-str)))
		  (let ((val (assignment-value var assign)))
		    (if val
			(member val ran)
			(error "Variable ~S does not hav a value according to assignment ~S" var assign)))))
	       ((e-statement? formula)
		(let ((var (unary-statement-argument formula))
		      (ran (edge-range poly-str)))
		  (let ((val (assignment-value var assign)))
		    (if val
			(member val ran)
			(error "Variable ~S does not hav a value according to assignment ~S" var assign)))))
	       ((f-statement? formula)
		(let ((var (unary-statement-argument formula))
		      (ran (face-range poly-str)))
		  (let ((val (assignment-value var assign)))
		    (if val
			(member val ran)
			(error "Variable ~S does not hav a value according to assignment ~S" var assign)))))
	       (t ;; incidence statement
		(let ((var-1 (binary-statement-first-arg formula))
		      (var-2 (binary-statement-second-arg formula))
		      (ran (incidence-matrix poly-str)))
		  (let ((val-1 (assignment-value var-1 assign))
			(val-2 (assignment-value var-2 assign)))
		    (not (zerop (aref ran val-1 val-2))))))))))

(defvar steinitz-rademacher-axioms
  '(
    ; non-empty polytope domains
    (exists ?x (V ?x))
    (exists ?x (E ?x))
    (exists ?x (F ?x))
    ; symmetric incidence relation -- do we really want this?
   (all ?x (all ?y (implies (I ?x ?y) (I ?y ?x))))
    ; No two objects of the same type are incident
   (all ?x (all ?y (implies (and (V ?x) (V ?y)) (not (I ?x ?y)))))
   (all ?x (all ?y (implies (and (E ?x) (E ?y)) (not (I ?x ?y)))))
   (all ?x (all ?y (implies (and (F ?x) (F ?y)) (not (I ?x ?y)))))
   ; a kind of transitivity
   (all ?x 
	(all ?y 
	     (all ?z 
		  (implies 
		   (and (V ?x) (E ?y) (F ?z) (I ?x ?y) (I ?y ?z))
		   (I ?x ?z)))))))

(defvar steinitz-rademacher-axioms-typed
  '(
    ; No two objects of the same type are incident
    (all (?x V) (all (?y V) (not (I ?x ?y))))
    (all (?x E) (all (?y E) (not (I ?x ?y))))
    (all (?x F) (all (?y F) (not (I ?x ?y))))
   ; a kind of transitivity
   (all (?x V)
	(all (?y E)
	     (all (?z F)
		  (implies 
		   (and (I ?x ?y) (I ?y ?z))
		   (I ?x ?z)))))))

(defun steinitz-rademacher? (poly-str)
  "Determine whether a polyhedron is a Steinitz-Rademacher polyhedron."
  (let ((failed nil))
    (dolist (ax steinitz-rademacher-axioms)
      (unless (true? ax poly-str nil)
	(push ax failed)))
    (or (null failed) failed)))

(defun steinitz-rademacher-typed? (poly-str)
  "Determine whether a polyhedron is a Steinitz-Rademacher polyhedron,
according to the typed presentation of the axioms."
  (let ((failed nil))
    (dolist (ax steinitz-rademacher-axioms-typed)
      (unless (true? ax poly-str nil)
	(push ax failed)))
    (or (null failed) failed)))


;;; Some specific polyhedra

(defun array-from-incidences (num-elements &rest incidences)
  (let ((a (make-array (list num-elements num-elements) 
		       :element-type 'bit
		       :initial-element 0)))
    (dolist (incidence incidences a)
      (let ((x (first incidence))
	    (y (second incidence)))
	(setf (aref a x y) 1)))))

(defvar tetrahedron
  (list 14
	#1A(0 0 0 0 1 1 1 1 1 1 2 2 2 2)
	#2A((0 0 0 0 1 0 1 1 0 0 0 0 0 0)
	    (0 0 0 0 1 1 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 1 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 0 1 1 1 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 1 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 1 0 1 0)
	    (0 0 0 0 0 0 0 0 0 0 1 0 0 1)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1)
	    (0 0 0 0 0 0 0 0 0 0 0 1 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defvar simplest-sr-poly
  (list 6
	#1A(0 0 1 1 2 2)
	#2A((0 0 1 1 1 1)
	    (0 0 1 1 1 1)
	    (1 1 0 0 1 1)
	    (1 1 0 0 1 1)
	    (1 1 1 1 0 0)
	    (1 1 1 1 0 0))))

(provide 'poly-str)
