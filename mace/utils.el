(require 'cl)

(defun account-for-extension (constants predicate)
  "Make a formula saying that the extension of PREDICATE is exhausted
by the list CONSTANTS of constant symbols.  E.g,

\(ACCOUNT-FOR-EXTENSION '(A B C) 'VERTEX\)

should return the formula

\(ALL ?X (IMPLIES (VERTEX ?X) (OR (= ?X A) (= ?X B) (= ?X C)))\)"
  (when constants
    (when (member 'x constants)
      (error "Unble to proceed: \"x\" is one of the constants!"))
    (insert "all x (")
    (insert (concat (format "%s" predicate) "(x) -> "))
    (insert "(")
    (do* ((len (length constants))
	  (constant (car constants) (car constants-tail))
	  (constants-tail (cdr constants) (cdr constants-tail)))
	((null constants-tail) (insert (concat "x = " (format "%s" constant))))
      (insert (concat "x = " (format "%s" constant)))
      (insert (concat " | ")))
    (insert ")")
    (insert ").")
    (newline)))

(defun length-two-partitions (n)
  (let (result)
    (dotimes (i (1+ n) result)
      (push (list i (- n i)) result))))

(defun length-three-partitions (n)
  (let (result)
    (dotimes (i (1+ n) result)
      (let ((parts (length-two-partitions (- n i))))
	(dolist (part parts)
	  (push (cons i part) result))))))

(defun all-different (constants)
  (when constants
    (do* ((c (car constants) (car rest))
	  (rest (cdr constants) (cdr rest)))
	((null rest))
      (dolist (d rest)
	(insert (format "%s != %s." c d))
	(newline)))))

(defun type-constants (constants predicate)
  (dolist (constant constants)
    (insert (concat (format "%s" predicate)
		   "("
		   (format "%s" constant)
		   ")."))
    (newline)))

(defun fully-account-for-extension (constants predicate)
  (cond ((null constants)
	 (insert (concat "-(exists x " (format "%s" predicate) "(x))."))
	 (newline))
	 (t (type-constants constants predicate)
	    (all-different constants)
	    (account-for-extension constants predicate))))

(defun new-constants (n)
  (let (result)
    (dotimes (i n (reverse result))
      (push (format "c%s" i) result))))

(defun first-n (lst n)
  (if (zerop n)
      nil
    (when lst
      (cons (car lst) (first-n (cdr lst) (1- n))))))

(defun partition-theory (theory cardinality)
  (let ((partitions (length-three-partitions cardinality))
	(all-constants (new-constants cardinality))
	(result-theories nil))
    (dolist (part partitions result-theories)
      (destructuring-bind (num-vertices num-edges num-faces)
	  part
	(let ((vertex-constants (first-n all-constants num-vertices))
	      (edge-constants (first-n (nthcdr num-vertices all-constants) 
				       num-edges))
	      (face-constants (first-n (nthcdr (+ num-vertices num-edges)
					       all-constants)
				       num-faces)))
	  (let ((theory (with-output-to-string
			  (insert theory)
			  (fully-account-for-extension vertex-constants
						       "vertex")
			  (fully-account-for-extension edge-constants
						       "edge")
			  (fully-account-for-extension face-constants
						       "face"))))
	    (push theory result-theories)))))))

(defvar *base-theory*
  "% Setting up the language: three types of objects, one incidence
% relation split into three, with signatures as thus:
%
% - i1: vertices, edges;
% - i2: edges, faces;
% - i3: vertices, faces
all x all y (i1(x,y) -> (-face(x) & -face(y))).
all x all y (i2(x,y) -> (-vertex(x) & -vertex(y))).
all x all y (i3(x,y) -> (-edge(x) & -edge(y))).

% Everything is either a vertex, edge, or face
all x (vertex(x) | edge(x) | face(x)).

% Non-triviality
exists x (vertex(x)).
exists x (edge(x)).
exists x (face(x)).

% The sets of vertices, edges, and faces are disjoint
all x (vertex(x) -> -edge(x)).
all x (vertex(x) -> -face(x)).
all x (edge(x) -> -face(x)).

% Symmetric incidence relation -- doesn't make sense when we've split
% the incidence relation
% all x all y (i(x,y) -> i(y,x)).
all x all y (i1(x,y) -> i1(y,x)).
all x all y (i2(x,y) -> i2(y,x)).
all x all y (i3(x,y) -> i3(y,x)).

% No two objects of the same type are incident This notion requires
% more formulas to express in the split incidence relation approach.
%
% Or, in light of the \"typing\" above, we don't need these at all
% all x all y ((vertex(x) & vertex(y)) -> -i1(x,y)).
% all x all y ((vertex(x) & vertex(y)) -> -i2(x,y)).
% all x all y ((vertex(x) & vertex(y)) -> -i3(x,y)).
% all x all y ((edge(x) & edge(y)) -> -i2(x,y)).
% all x all y ((edge(x) & edge(y)) -> -i2(x,y)).
% all x all y ((edge(x) & edge(y)) -> -i2(x,y)).
% all x all y ((face(x) & face(y)) -> -i3(x,y)).
% all x all y ((face(x) & face(y)) -> -i3(x,y)).
% all x all y ((face(x) & face(y)) -> -i3(x,y)).

% \"Transitivity\"
all x all y all z ((vertex(x) & edge(y) & face(z) & i1(x,y) & i2(y,z)) -> i3(x,z)).

% every edge is incident with at exactly two vertices
all x (edge(x) -> exists y exists z (y != z & vertex(y) & vertex(z) & i1(y,x) & i1(z,x) & all w (vertex(w) & i1(w,x) -> (w = y | w = z)))).

% every edge is incident with at least two vertices
% all x (edge(x) -> exists y exists z (y != z & vertex(y) & vertex(z) & i1(y,x) & i1(z,x))).

% every edge is incident with at exactly two faces
all x (edge(x) -> exists y exists z (y != z & face(y) & face(z) & i2(x,y) & i2(x,z) & all w (face(w) & i2(x,w) -> (w = y | w = z)))).

% every edge is incident with at least two faces
% all x (edge(x) -> exists y exists z (y != z & face(y) & face(z) & i2(x,y) & i2(x,z))).

% for every vertex v and every face f such that v is incident to f, there exactly two edges that are indicent with both v and f
all v all f (vertex(v) & face(f) & i3(v,f) -> exists e1 exists e2 (edge(e1) & edge(e2) & e1 != e2 & i1(v,e1) & i1(v,e2) & all e ((edge(e) & i1(v,e) & i2(e,f)) -> (e = e1 | e = e2)))).

% two faces can share at most one edge
all f1 all f2 all e1 all e2 all e3 ((face(f1) & face(f2) & edge(e1) & edge(e2) &edge(e3) & i2(e1,f1) & i2(e2,f1) & i2(e3,f1) & i2(e1,f2) & i2(e2,f2) & i2(e3,f2)) -> (e1 = e2 | e1 = e3 | e2 = e3)).

% every vertex is incident with something
% all v (vertex(v) -> exists x i(v,x)).
% Stronger form: every vertex is incident with an edge and a face
all v (vertex(v) -> exists e exists f (edge(e) & face(f) & i1(v,e) & i3(v,f))).

% every face is incident with something
% all f (face(f) -> exists x (i(x,f))).
% Stronger form: every face is incident with one edge and one vertex
all f (face(f) -> exists v exists e (vertex(v) & edge(e) & i3(v,f) & i2(e,f))).

% simpliciality: every face is a triangle
% all f (face(f) -> exists e1 exists e2 exists e3 (edge(e1) & edge(e2) & edge(e3) & e1 != e2 & e2 != e3 & e3 != e1 & i2(e1,f) & i2(e2,f) & i2(e3,f) & all e ((edge(e) & i2(e,f)) -> (e = e1 | e = e2 | e = e3)))).
% weakening: every face has at least three edges (rules out cases where faces have exactly two edges)
all f (face(f) -> exists e1 exists e2 exists e3 (edge(e1) & edge(e2) & edge(e3) & e1 != e2 & e2 != e3 & e1 != e3 & i2(e1,f) & i2(e2,f) & i2(e3,f))).

% every face is incident with at least three vertices
all f (face(f) -> exists v1 exists v2 exists v3 (vertex(v1) & vertex(v2) & vertex(v3) & v1 != v2 & v2 != v3 & v1 != v3 & i3(v1,f) & i3(v2,f) & i3(v3,f))).

% \"simpliciality\" for vertices: every vertex is incident with at least three faces
all v (vertex(v) -> exists f1 exists f2 exists f3 (face(f1) & face(f2) & face(f3) & f1 != f2 & f2 != f3 & f1 != f3 & i3(v,f1) & i3(v,f2) & i3(v,f3))).

% extensionality
% for vertices
all v1 all v2 ((vertex(v1) & vertex(v2) & all e (edge(e) -> (i1(v1,e) <-> i1(v2,e)))) -> (v1 = v2)).
all v1 all v2 ((vertex(v1) & vertex(v2) & all f (face(f) -> (i3(v1,f) <-> i3(v2,f)))) -> (v1 = v2)).
% for edges
all e1 all e2 ((edge(e1) & edge(e2) & all v (vertex(v) -> (i1(v,e1) <-> i1(v,e2)))) -> (e1 = e2)).
all e1 all e2 ((edge(e1) & edge(e2) & all f (face(f) -> (i2(e1,f) <-> i2(e2,f)))) -> (e1 = e2)).
% for faces
all f1 all f2 ((face(f1) & face(f2) & all v (vertex(v) -> (i3(v,f1) <-> i3(v,f2)))) -> (f1 = f2)).
all f1 all f2 ((face(f1) & face(f2) & all e (edge(e) -> (i2(e,f1) <-> i2(e,f2)))) -> (f1 = f2)).

% width 1: everything is connected by an edge to every other thing
% all x all y (x = y | (i1(x,y) | i2(x,y) | i3(x,y))).

% width 2: everything is connected by a path of length 2 to every other thing
% all x all y (x = y 
%              | (i1(x,y) | i2(x,y) | i3(x,y))
% 	     | exists z ((i1(x,z) | i2(x,z) | i3(x,z)) & (i1(z,y) | i2(z,y) | i3% (z,y)))).")

(defun export-theories (theory cardinality)
  (let ((partitions (length-three-partitions cardinality))
	(all-constants (new-constants cardinality))
	(result-theories nil))
    (dolist (part partitions result-theories)
      (destructuring-bind (num-vertices num-edges num-faces)
	  part
	(let ((vertex-constants (first-n all-constants num-vertices))
	      (edge-constants (first-n (nthcdr num-vertices all-constants) 
				       num-edges))
	      (face-constants (first-n (nthcdr (+ num-vertices num-edges)
					       all-constants)
				       num-faces)))
	  (let ((theory-filename (concat "steinitz-rademacher-"
					 (format "%s" num-vertices)
					 "-"
					 (format "%s" num-edges)
					 "-"
					 (format "%s" num-faces)
					 ".in")))
	  (with-temp-file theory-filename
	    (insert "formula_list(sos).")
	    (newline)
	    (insert theory)
	    (fully-account-for-extension vertex-constants
					 "vertex")
	    (fully-account-for-extension edge-constants
					 "edge")
	    (fully-account-for-extension face-constants
					 "face")
	    (insert "end_of_list.")
	    (newline))))))))

