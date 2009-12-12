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

(defun domain (poly-str)
  (first poly-str))

(defun vertex-range (poly-str)
  (second poly-str))

(defun edge-range (poly-str)
  (third poly-str))

(defun face-range (poly-str)
  (fourth poly-str))

(defun incidence-matrix (poly-str)
  (fifth poly-str))

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
  (let (l)
    (do ((i m (1+ i)))
	((= i n) (reverse l))
      (push i l))))

(defun make-poly-str (num-vertices num-edges num-faces incidence-matrix)
  (list (upto (+ num-vertices num-edges num-faces))
	(upto num-vertices)
	(from-to num-vertices (+ num-vertices num-edges))
	(from-to (+ num-vertices num-edges) (+ num-vertices num-edges num-faces))
	incidence-matrix))

(defun cardinality (poly-str)
  (length (first poly-str)))

(defun num-vertices (poly-str)
  (length (second poly-str)))

(defun num-edges (poly-str)
  (length (third poly-str)))

(defun num-faces (poly-str)
  (length (fourth poly-str)))

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
	       (var (bound-variable formula))
	       (dom (domain poly-str)))
	   (every #'(lambda (a)
		      (true? matrix poly-str (update-assignment var assign a)))
		  dom)))
	((existential? formula)
	 (let ((matrix (matrix formula))
	       (var (bound-variable formula))
	       (dom (domain poly-str)))
	   (some #'(lambda (a)
		       (true? matrix poly-str (update-assignment var assign a)))
		   dom)))
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


;;; Some specific polyhedra

(defun array-from-incidences (num-elements &rest incidences)
  (let ((a (make-array (list num-elements num-elements) :element-type 'bit)))
    a))

(defvar tetrahedron
  (list 14
	'(0 1 2 3)
	'(4 5 6 7 8 9)
	'(10 11 12 13)
	(transitive-symmetric-closure
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
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0)))))
