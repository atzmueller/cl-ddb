(in-package :cl-dbd-user)

(defun setup-example-db ()
  (<- (person john))
  (<- (person mary))
  (<- (person alice))
  (<- (parent john mary))
  (<- (parent mary alice))
  (<- (ancestor ?x ?y) (parent ?x ?y))
  (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
  (<- (not-grandparent ?x ?y) (person ?x) (person ?y) (person ?z) (parent ?z ?y) (not (parent ?x ?z))))

(defun run-example ()
  (setup-example-db)
  (forward-chain)
  (let ((result (?- (not-grandparent john ?who))))
    (LOOP :FOR bindings :IN result :DO
      (let ((who (cdr (assoc '?who bindings))))
	(format t "Solution: ~A~%" who)))))

;; (run-example)

;; (clear-dl-db)
;; (show-dl-db)

(<- (a ?x ?y) (b ?y ?x))
(<- (b ?x ?y) (c ?x ?y) (d ?x))

(<- (a 5 6))
(<- (c 1 2))
(<- (c 3 4))
(<- (d 3))

(?- (a ?x ?y))
(?- (b ?x ?y))



(<- (edge a b))
(<- (edge b c))
(<- (edge c d))
(<- (edge d a))

(<- (path ?x ?y) (edge ?x ?y))
(<- (path ?x ?y) (edge ?x ?z) (path ?z ?y))


         path(X, Y)?




;;;; cl-dbd-tests.lisp
;;;; Illustrative examples and regression tests for the cl-dbd Datalog engine.
;;;; Load this file after loading cl-dbd.

(in-package :cl-dbd)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Test Harness
;;; ═══════════════════════════════════════════════════════════════════════════

(defvar *pass-count* 0 "Cumulative pass tally.")
(defvar *fail-count* 0 "Cumulative fail tally.")

(defmacro suite (name &body body)
  "Print a section heading, then evaluate BODY sequentially."
  `(progn
     (format t "~%--- ~A ---~%" ,name)
     ,@body))

(defmacro is (label form expected &key (test '#'equal))
  "Assert FORM equals EXPECTED under TEST; update the pass/fail tallies."
  `(let ((actual ,form))
     (if (funcall ,test actual ,expected)
         (progn (incf *pass-count*)
                (format t "  PASS  ~A~%" ,label))
         (progn (incf *fail-count*)
                (format t "  FAIL  ~A~%        expected: ~S~%        got:      ~S~%"
                        ,label ,expected actual)))))

(defmacro signals-error (label form &optional (type 'error))
  "Assert that evaluating FORM signals a condition of TYPE."
  `(handler-case
       (progn ,form
              (incf *fail-count*)
              (format t "  FAIL  ~A  (no condition signalled)~%" ,label))
     (,type ()
       (incf *pass-count*)
       (format t "  PASS  ~A~%" ,label))))

(defun report ()
  "Print the final pass/fail summary."
  (format t "~%Results: ~A passed, ~A failed.~%" *pass-count* *fail-count*))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Test Utilities
;;; ═══════════════════════════════════════════════════════════════════════════

(defmacro with-fresh-db (&body body)
  "Execute BODY against an isolated, empty database using dynamic binding.
   All five database globals are shadowed; the caller's database is unaffected."
  `(let ((*facts*       '())
         (*derived*      '())
         (*derived-tail* nil)
         (*rules*        '())
         (*db-dirty*      t))
     ,@body))

(defun values-for (var results)
  "Collect the binding of VAR from each alist in RESULTS, then sort them
   lexicographically by their printed representation for stable comparison."
  (sort (mapcar (lambda (b) (cdr (assoc var b :test #'eq))) results)
        #'string<
        :key (lambda (v) (format nil "~S" v))))

(defun count-derived ()
  "Count only the IDB-portion of *derived* by walking the list until
   the first cons cell that is physically identical to *facts*."
  (loop :for cell :on *derived*
        :until (eq cell *facts*)
        :count 1))

(defun succeeds-p (results) (consp results))
(defun fails-p    (results) (null  results))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 1 — Basic EDB Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "1. Basic EDB Operations"

  (with-fresh-db
    (add-fact '(color red))
    (add-fact '(color green))
    (add-fact '(color blue))

    (is "Exact positive query returns one empty-binding solution"
        (query '(color red))
        '(()))

    (is "Query for absent fact returns no solutions"
        (query '(color purple))
        '())

    (is "Variable query returns one binding alist per matching fact"
        (length (query '(color ?c)))
        3)

    (is "Variable query captures all three values"
        (values-for '?c (query '(color ?c)))
        '(blue green red))

    (is "Duplicate add-fact is silently ignored"
        (progn (add-fact '(color red))
               (length (query '(color ?c))))
        3)

    (is "Negated query succeeds when fact is absent"
        (succeeds-p (query '(not (color purple))))
        t)

    (is "Negated query fails when fact is present"
        (fails-p (query '(not (color red))))
        t)))


;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 3 — Family Tree: Basic Rules
;;;
;;;   tom ──▶ bob ──▶ ann
;;;       │       └──▶ pat
;;;       └──▶ liz
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "3. Family Tree — Basic Rules"

  (with-fresh-db
    (<- (parent tom bob))
    (<- (parent tom liz))
    (<- (parent bob ann))
    (<- (parent bob pat))

    (<- (grandparent ?x ?z)
        (parent ?x ?y)
        (parent ?y ?z))

    ;; The relation is reflexive: a person is their own sibling here.
    (<- (sibling ?x ?y)
        (parent ?p ?x)
        (parent ?p ?y))

    (is "Direct parent fact is found"
        (succeeds-p (query '(parent tom bob)))
        t)

    (is "Reversed parent direction is absent"
        (fails-p (query '(parent bob tom)))
        t)

    (is "Tom has exactly two children"
        (length (query '(parent tom ?c)))
        2)

    (is "Grandchildren of tom are ann and pat"
        (values-for '?c (query '(grandparent tom ?c)))
        '(ann pat))

    (is "Liz has no children, so no grandchildren via her"
        (fails-p (query '(grandparent liz ?c)))
        t)

    (is "Tom has no grandparent"
        (fails-p (query '(grandparent ?x tom)))
        t)

    ;; (sibling ann ?s) matches (parent bob ann)(parent bob ann) → ann
    ;;                      and  (parent bob ann)(parent bob pat) → pat
    (is "Ann has 2 sibling solutions (self-pair included)"
        (length (query '(sibling ann ?s)))
        2)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 4 — Graph Reachability: Recursive Rules
;;;
;;;   a ──▶ b ──▶ c ──▶ d
;;;   └──────────────────▶  (direct shortcut)
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "4. Graph Reachability — Recursive Rules"

  (with-fresh-db
    (<- (edge a b))
    (<- (edge b c))
    (<- (edge c d))
    (<- (edge a d))   ; shortcut: two derivation paths to d

    ;; Base case: a direct edge is reachable.
    (<- (reachable ?x ?y) (edge ?x ?y))
    ;; Inductive case: step from x to z, then reach y transitively.
    (<- (reachable ?x ?y) (edge ?x ?z) (reachable ?z ?y))

    (is "Single-hop edge a→b is reachable"
        (succeeds-p (query '(reachable a b)))
        t)

    (is "Two-hop path a→b→c is reachable"
        (succeeds-p (query '(reachable a c)))
        t)

    (is "Node d is reachable from a via multiple paths"
        (succeeds-p (query '(reachable a d)))
        t)

    (is "Reverse direction is not reachable (directed graph)"
        (fails-p (query '(reachable d a)))
        t)

    ;; add-derived-fact deduplicates, so d appears exactly once despite
    ;; two derivation paths (a→d directly and a→b→c→d).
    (is "IDB deduplication: a can reach exactly {b, c, d}"
        (values-for '?y (query '(reachable a ?y)))
        '(b c d))

    (is "c can only reach d"
        (values-for '?y (query '(reachable c ?y)))
        '(d))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 5 — Negation as Failure (NAF)
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "5. Negation as Failure"

  (with-fresh-db
    (<- (color red))
    (<- (color blue))
    (<- (color green))
    (<- (banned red))

    ;; available(?c) :- color(?c), NOT banned(?c).
    ;; Safety is satisfied: ?c appears in positive literal (color ?c).
    (<- (available ?c)
        (color ?c)
        (not (banned ?c)))

    (is "Blue is available because it is not banned"
        (succeeds-p (query '(available blue)))
        t)

    (is "Red is not available because it is banned"
        (fails-p (query '(available red)))
        t)

    (is "Exactly two colors are available"
        (length (query '(available ?c)))
        2)

    (is "Available colors are blue and green"
        (values-for '?c (query '(available ?c)))
        '(blue green))

    ;; Simulate fact retraction: remove the ban and force re-derivation.
    (is "Retracting the ban on red makes it available again"
        (progn
          (setf *facts* (remove '(banned red) *facts* :test #'equal))
          (setf *db-dirty* t)
          (values-for '?c (query '(available ?c))))
        '(blue green red))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 6 — Stratified Negation
;;;
;;; The strata for this example:
;;;   Stratum 0: employee/2, department/1 (EDB), has-employee/1 (positive rule)
;;;   Stratum 1: empty-dept/1             (negated dependency on has-employee)
;;;
;;; forward-chain must fully saturate stratum 0 before evaluating any
;;; negated literal in stratum 1 — otherwise empty-dept would be evaluated
;;; before has-employee facts exist.
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "6. Stratified Negation"

  (with-fresh-db
    (<- (employee alice engineering))
    (<- (employee bob   sales))
    (<- (employee carol engineering))
    (<- (department engineering))
    (<- (department sales))
    (<- (department hr))

    ;; Stratum 0: summarise which departments have at least one employee.
    (<- (has-employee ?d) (employee ?e ?d))

    ;; Stratum 1: a department is empty iff it has no employee record.
    (<- (empty-dept ?d)
        (department ?d)
        (not (has-employee ?d)))

    (is "HR has no employees and is detected as empty"
        (succeeds-p (query '(empty-dept hr)))
        t)

    (is "Engineering is not empty"
        (fails-p (query '(empty-dept engineering)))
        t)

    (is "Exactly one department is empty"
        (values-for '?d (query '(empty-dept ?d)))
        '(hr))

    ;; Verify the stratifier assigns correct levels.
    (let ((strata (compute-strata (all-rules))))
      (is "has-employee is at stratum 0 (positive rule on EDB)"
          (gethash 'has-employee strata)
          0)
      (is "empty-dept is at stratum 1 (one above its negated dependency)"
          (gethash 'empty-dept strata)
          1))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 7 — Split-Pointer Mechanics
;;;
;;; This suite inspects the physical representation of the combined list
;;; to verify:
;;;   (a) all-facts returns *deriveqd* directly — no allocation.
;;;   (b) the IDB-tail cons is patched when new EDB facts arrive.
;;;   (c) clear-derived severs the link while leaving the EDB intact.
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "7. Split-Pointer Mechanics"

  (with-fresh-db
    (add-fact '(base a))
    (add-fact '(base b))
    (<- (derived-from ?x) (base ?x))
    (forward-chain)

    ;; ── Structure after initial derivation ──────────────────────────────────
    ;;
    ;; *derived* ──▶ [derived-from a] ──▶ [derived-from b] ──▶ [base b] ──▶ [base a] ──▶ nil
    ;;                                          ▲
    ;;                                    *derived-tail*
    ;;                                    (cdr = *facts*)

    (is "all-facts returns *derived* without consing a new list"
        (eq (all-facts) *derived*)
        t)

    (is "Exactly 2 IDB facts precede the EDB in the combined list"
        (count-derived)
        2)

    (is "Combined list has 4 elements (2 IDB + 2 EDB)"
        (length (all-facts))
        4)

    (is "*derived-tail* cdr is physically identical to *facts*"
        (cdr *derived-tail*)
        *facts*
        :test #'eq)

    ;; ── Add a new EDB fact after the IDB is already built ───────────────────
    (add-fact '(base c))

    ;; add-fact must patch *derived-tail* so the IDB chain still flows
    ;; into the updated *facts* head.
    (is "Tail link is patched: *derived-tail* cdr equals the new *facts* head"
        (cdr *derived-tail*)
        *facts*
        :test #'eq)

    (is "Combined list grows to 5 without triggering re-derivation"
        (length (all-facts))
        5)

    ;; ── Sever the IDB portion ───────────────────────────────────────────────
    (clear-derived)

    (is "*derived* is nil after clear-derived"
        (null *derived*)
        t)

    (is "*derived-tail* is nil after clear-derived"
        (null *derived-tail*)
        t)

    (is "EDB is intact: all 3 base facts remain"
        (length *facts*)
        3)

    (is "all-facts falls back to *facts* when IDB is empty"
        (all-facts)
        *facts*
        :test #'eq)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 8 — Dirty-Flag Optimisation
;;;
;;; *db-dirty* is the gate that prevents redundant re-derivation.
;;; Invariants:
;;;   • starts as T so the first query always runs forward-chain.
;;;   • cleared to NIL after each forward-chain run.
;;;   • set back to T by add-fact, add-rule, and clear-dl-db.
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "8. Dirty-Flag Optimisation"

  (with-fresh-db

    (is "Database starts dirty"
        *db-dirty*
        t)

    ;; x is a constant (no leading ?), so (<- (item x)) adds a base fact.
    (<- (item x))
    (<- (copy ?v) (item ?v))
    (forward-chain)

    (is "After forward-chain, dirty flag is cleared"
        *db-dirty*
        nil)

    ;; A second call with no changes must not rebuild *derived*.
    (let ((snapshot *derived*))
      (forward-chain)
      (is "Redundant forward-chain is a no-op (same cons cell)"
          *derived*
          snapshot
          :test #'eq))

    (add-fact '(item y))

    (is "add-fact marks the database dirty"
        *db-dirty*
        t)

    ;; query triggers forward-chain internally.
    (is "New derivation is visible after the next query"
        (succeeds-p (query '(copy y)))
        t)

    (is "Database is clean again after query-triggered forward-chain"
        *db-dirty*
        nil)

    (clear-dl-db)

    (is "clear-dl-db resets the dirty flag to T"
        *db-dirty*
        t)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Suite 9 — Safety Violations and Error Conditions
;;; ═══════════════════════════════════════════════════════════════════════════

(suite "9. Safety Violations and Error Conditions"

  ;; Case A: A variable appears in the rule head but in no positive body
  ;;         literal.  The head fact would be non-ground.
  (with-fresh-db
    (signals-error "Head variable unbound by any positive body literal"
        (add-rule '(foo ?x) '((not (bar ?x))))))

  ;; Case B: A variable appears inside a negated literal but in no positive
  ;;         body literal.  The negated goal would be non-ground at eval time.
  (with-fresh-db
    (signals-error "Negated-literal variable not bound by a positive literal"
        (add-rule '(foo ?x) '((pos ?x) (not (bar ?y))))))

  ;; Case C: Mutual negative cycle — p :- not q; q :- not p.
  ;;         The safety check passes (no variables), but the stratifier detects
  ;;         that no consistent stratum assignment exists.
  (with-fresh-db
    (signals-error "Mutual negative cycle triggers a stratification error"
        (progn
          (add-rule '(p) '((not (q))))
          (add-rule '(q) '((not (p))))
          (forward-chain)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Run
;;; ═══════════════════════════════════════════════════════════════════════════

(report)
