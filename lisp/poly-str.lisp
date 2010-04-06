;;; Polyhedra as incidence structures

(require 'utils "utils")
(require 'formulas "formulas")

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
  (cond ((top? formula) t)
	((bottom? formula) nil)
	((negation? formula)
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
    ; I'm reluctant to include these, but they might help
    (exists (?x V) (top))
    (exists (?x E) (top))
    (exists (?x F) (top))
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
	   (I ?x ?z)))))
    ; edges have exactly two vertices
    (all (?e E)
     (exists (?v1 V)
      (exists (?v2 V)
        (and (not (= ?v1 ?v2))
	     (I ?v1 ?e)
	     (I ?v2 ?e)
	     (all (?v V)
		  (implies (I ?v ?e)
			   (or (= ?v ?v1)
			       (= ?v ?v2))))))))
    ; edges are incident with two faces
    (all (?e E)
     (exists (?f1 F)
      (exists (?f2 F)
        (and (not (= ?f1 ?f2))
	     (I ?e ?f1)
	     (I ?e ?f2)
	     (all (?f F)
		  (implies (I ?e ?f)
			   (or (= ?f1 f)
			       (= ?f2 f))))))))
    ; no disconnected vertices
    (all (?v V) (exists (?e E) (I ?v ?e)))
    ; no disconnected faces
    (all (?f F) (exists (?e E) (I ?e ?f)))))

(defvar steinitz-rademacher-axioms-typed-simplicial
  '(
    ; I'm reluctant to include these, but they might help
    (exists (?x V) (top))
    (exists (?x E) (top))
    (exists (?x F) (top))
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
	   (I ?x ?z)))))
    ; edges have exactly two vertices
    (all (?e E)
     (exists (?v1 V)
      (exists (?v2 V)
        (and (not (= ?v1 ?v2))
	     (I ?v1 ?e)
	     (I ?v2 ?e)
	     (all (?v V)
		  (implies (I ?v ?e)
			   (or (= ?v ?v1)
			       (= ?v ?v2))))))))
    ; edges are incident with two faces
    (all (?e E)
     (exists (?f1 F)
      (exists (?f2 F)
        (and (not (= ?f1 ?f2))
	     (I ?e ?f1)
	     (I ?e ?f2)
	     (all (?f F)
		  (implies (I ?e ?f)
			   (or (= ?f1 f)
			       (= ?f2 f))))))))
    ; no disconnected vertices
    (all (?v V) (exists (?e E) (I ?v ?e)))
    ; no disconnected faces
    (all (?f F) (exists (?e E) (I ?e ?f)))
    ; all faces are triangles
    (all (?f F) (exists (?e1 E)
		 (exists (?e2 E)
		   (exists (?e3 E)
		     (and (not (= ?e1 ?e2))
			  (not (= ?e2 ?e3))
			  (not (= ?e3 ?e1))
			  (I ?e1 ?f)
			  (I ?e2 ?f)
			  (I ?e3 ?f)
			  (all (?e E)
			       (implies (I ?e ?f)
					(or (= ?e ?e1)
					    (= ?e ?e2)
					    (= ?e ?e3)))))))))))
			 


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

;;; Successor function

(defun neighbors (poly-str)
  (let ((card (cardinality poly-str))
	(matrix (incidence-matrix poly-str))
	(nbrs nil))
    (do-square x y card
      (let ((m (copy-square-matrix matrix)))
	(push (make-poly-str (type-vector poly-str) (invert m x y)) nbrs)))
    nbrs))

(provide 'poly-str)
