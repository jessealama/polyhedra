;;; formulas.lisp A representation for first-order formulas

(defun top? (formula)
  (eq (car formula) 'top))

(defun bottom? (formula)
  (eq (car formula) 'bottom))

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

(provide 'formulas)

;;; formulas.lisp ends here