(defpackage #:cl-dbd-tests
  (:use #:cl #:fiveam))

(in-package #:cl-dbd-tests)

(def-suite cl-dbd-suite
  :description "Tests targeting each bug identified in the cl-dbd Datalog engine.")

(in-suite cl-dbd-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Utility
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro with-cleaned-db (&body body)
  "Reset the Datalog database to an empty state before running BODY."
  `(progn (cl-dbd::clear-dl-db) ,@body))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #1 – Empty bindings '() are falsy and treated as failure
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-1/ground-query-succeeds-against-ground-fact
  "A ground query matching an identical ground fact must return non-nil.
   Unifying equal ground terms yields '(); (and '() ...) short-circuits
   because '() is falsy in CL, so no result is ever collected."
  (with-cleaned-db
    (cl-dbd::<- (parent alice bob))
    (let ((result (cl-dbd::?- (parent alice bob))))
      (is (not (null result))
          "Ground query (parent alice bob) should succeed; got NIL"))))

(test bug-1/resolve-body-returns-singleton-list-for-ground-match
  "resolve-body must return '(()) -- a list holding one empty binding set --
   when a ground body literal unifies with a ground fact.
   The falsy-bindings bug causes it to return NIL instead."
  (with-cleaned-db
    (cl-dbd::<- (foo bar))
    (let ((result (cl-dbd::resolve-body '((foo bar)) '())))
      (is (equal result '(()))
          "resolve-body returned ~S; expected '(()) for a ground match"
          result))))

(test bug-1/propositional-rule-derives-its-head
  "The rule (b) :- (a) must add (b) to the database when (a) is a fact.
   The literal (a) unifies with fact (a) yielding '(); the falsy check
   skips the result so the head is never asserted."
  (with-cleaned-db
    (cl-dbd::<- (a))
    (cl-dbd::<- (b) (a))
    (cl-dbd::forward-chain)
    (is (cl-dbd::fact-exists-p '(b))
        "(b) should be derived by (b :- a) but is absent")))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #2 – Query results are not grouped per solution
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-2/each-solution-is-a-complete-binding-alist
  "Two matching facts against a two-variable query should yield a list of
   two alists, each containing bindings for both variables.
   (dolist (binding bindings) (push binding results)) instead flattens all
   cons pairs from every solution into a single list."
  (with-cleaned-db
    (cl-dbd::<- (parent alice bob))
    (cl-dbd::<- (parent carol dave))
    (let ((results (cl-dbd::?- (parent ?x ?y))))
      (is (= 2 (length results))
          "Expected 2 grouped solutions, got ~A: ~S"
          (length results) results)
      (is (every #'listp results)
          "Each solution must be an alist, not a raw cons pair. Got: ~S"
          results)
      (is (every (lambda (sol)
                   (and (assoc '?x sol) (assoc '?y sol)))
                 results)
          "Every solution must bind both ?x and ?y. Got: ~S" results))))

(test bug-2/solutions-from-distinct-facts-are-separable
  "With two facts matching a two-variable query, it must be possible to
   pair each ?k with its corresponding ?v.  The flattened result makes
   cross-solution pairing impossible."
  (with-cleaned-db
    (cl-dbd::<- (kv a 1))
    (cl-dbd::<- (kv b 2))
    (let ((results (cl-dbd::?- (kv ?k ?v))))
      (is (= 2 (length results))
          "Expected 2 grouped solutions, got ~A: ~S"
          (length results) results)
      (is (every (lambda (sol)
                   (and (assoc '?k sol) (assoc '?v sol)))
                 results)
          "Every solution must carry both ?k and ?v. Got: ~S" results))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #3 – Positive goal extracted from negated query using cdr, not cadr
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-3/cdr-vs-cadr-on-negated-goal
  "For (not (p a b)), (cdr goal) = ((p a b)) -- an extra wrapping list --
   while (cadr goal) = (p a b), the literal itself.  This test documents
   the structural difference that the bug exploits."
  (let ((goal '(not (parent alice carol))))
    (is (equal (cadr goal) '(parent alice carol))
        "cadr gives the bare literal -- correct")
    (is (equal (cdr goal)  '((parent alice carol)))
        "cdr wraps the literal in an extra list -- what the buggy code uses")))

(test bug-3/negated-query-succeeds-when-fact-is-absent
  "(?- (not (parent alice carol))) must return non-nil because that fact
   is absent.  The cdr/cadr bug causes unification to attempt matching
   ((parent alice carol)) against each fact, which always fails, so the
   query returns NIL regardless of database content."
  (with-cleaned-db
    (cl-dbd::<- (parent alice bob))   ; carol is absent
    (let ((result (cl-dbd::?- (not (parent alice carol)))))
      (is (not (null result))
          "Negated query for an absent fact should succeed; got NIL"))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #4 – apply-substitutions does not follow transitive variable chains
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-4/transitive-chain-resolves-to-ground-value
  "With bindings ((?x . ?y) (?y . alice)), applying to ?x must yield alice.
   The single-step lookup returns ?y."
  (let* ((bindings '((?x . ?y) (?y . alice)))
         (result   (cl-dbd::apply-substitutions '?x bindings)))
    (is (equal result 'alice)
        "Expected alice via ?x->?y->alice; got ~S" result)))

(test bug-4/variable-bound-to-nil-returns-nil
  "If ?x is bound to NIL the result must be NIL.
   (or (cdr (assoc ?x bindings)) ?x) evaluates to (or nil ?x) = ?x -- wrong."
  (let* ((bindings '((?x . nil)))
         (result   (cl-dbd::apply-substitutions '?x bindings)))
    (is (null result)
        "?x bound to NIL should yield NIL, not the symbol ?x; got ~S"
        result)))

(test bug-4/chained-variable-inside-compound-term
  "Transitive resolution must propagate inside compound terms.
   (parent ?x bob) with ((?x . ?y) (?y . alice)) must become
   (parent alice bob), not (parent ?y bob)."
  (let* ((bindings '((?x . ?y) (?y . alice)))
         (result   (cl-dbd::apply-substitutions '(parent ?x bob) bindings)))
    (is (equal result '(parent alice bob))
        "Expected (parent alice bob); got ~S" result)))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #5 – NAF with unbound variables uses literal equality, not unification
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-5/naf-incorrectly-succeeds-when-matching-ground-fact-exists
  "resolve-body on (not (parent ?x alice)) with empty bindings must return
   NIL when (parent bob alice) is in the database -- some X satisfies
   (parent X alice).  fact-exists-p uses #'equal; the symbol ?x never equals
   the ground atom bob, so NAF incorrectly succeeds."
  (with-cleaned-db
    (cl-dbd::<- (parent bob alice))
    (let ((result (cl-dbd::resolve-body '((not (parent ?x alice))) '())))
      (is (null result)
          "NAF with free ?x should fail when (parent bob alice) exists; got ~S"
          result))))

(test bug-5/naf-correctly-fails-when-variable-is-pre-bound
  "Contrast: when ?x is already bound to bob in the incoming bindings,
   apply-substitutions grounds the literal to (parent bob alice) before
   the NAF check.  fact-exists-p then correctly detects the fact.
   The bug is isolated to the case where the variable is still free."
  (with-cleaned-db
    (cl-dbd::<- (parent bob alice))
    (let ((result (cl-dbd::resolve-body '((not (parent ?x alice)))
                                         '((?x . bob)))))
      (is (null result)
          "NAF (not (parent bob alice)) with ?x=bob pre-bound should fail"))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #6 – Unhygienic variable capture in the two iteration macros
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-6/iterate-with-rule-shadows-outer-rule-binding
  "iterate-with-rule hardcodes 'rule' as its dolist loop variable.  A
   binding of 'rule' in the enclosing lexical scope is silently shadowed
   inside the body.  Contrast with a plain dolist that uses a different
   loop variable where the outer binding remains accessible."
  (with-cleaned-db
    (cl-dbd::<- (ancestor ?x ?y) (parent ?x ?y))
    (let ((rule :sentinel))
      ;; Plain dolist with loop var 'r': outer 'rule' = :sentinel is visible.
      (let ((outer-visible nil))
        (dolist (r (cl-dbd::get-rules))
          (declare (ignore r))
          (when (eq rule :sentinel)
            (setf outer-visible t)))
        (is outer-visible
            "With dolist and loop var 'r', outer 'rule' = :sentinel is visible"))
      ;; iterate-with-rule injects its own 'rule': outer value is unreachable.
      (let ((outer-visible nil))
        (cl-dbd::iterate-with-rule (cl-dbd::get-rules)
          (when (eq rule :sentinel)     ; 'rule' is now the injected loop var
            (setf outer-visible t)))
        (is (not outer-visible)
            "With iterate-with-rule, outer 'rule' = :sentinel is shadowed ~
             and unreachable")))))

(test bug-6/iterate-facts-with-fact-shadows-outer-fact-binding
  "iterate-facts-with-fact hardcodes 'fact' as its dolist loop variable.
   A binding of 'fact' in the enclosing scope is silently shadowed.
   Contrast with a plain dolist where the outer binding stays accessible."
  (with-cleaned-db
    (cl-dbd::<- (color red))
    (let ((fact :sentinel))
      ;; Plain dolist with loop var 'f': outer 'fact' = :sentinel is visible.
      (let ((outer-visible nil))
        (dolist (f (cl-dbd::get-facts))
          (declare (ignore f))
          (when (eq fact :sentinel)
            (setf outer-visible t)))
        (is outer-visible
            "With dolist and loop var 'f', outer 'fact' = :sentinel is visible"))
      ;; iterate-facts-with-fact injects its own 'fact': outer is unreachable.
      (let ((outer-visible nil))
        (cl-dbd::iterate-facts-with-fact
          (when (eq fact :sentinel)     ; 'fact' is now the injected loop var
            (setf outer-visible t)))
        (is (not outer-visible)
            "With iterate-facts-with-fact, outer 'fact' = :sentinel is shadowed ~
             and unreachable")))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #7 – +unify-fail+ uses defparameter instead of defconstant
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-7/rebinding-unify-fail-breaks-failure-detection
  "+unify-fail+ is declared defparameter and can be mutated at runtime.
   After mutation, unify still returns the original :fail keyword while
   +unify-fail+ holds a new value; all eq-based failure guards throughout
   the engine silently stop detecting failure."
  (let ((original cl-dbd::+unify-fail+))
    (unwind-protect
        (progn
          (setf cl-dbd::+unify-fail+ :new-sentinel)
          (let ((fail-result (cl-dbd::unify 'a 'b '())))
            ;; unify still returns the original :fail keyword,
            ;; but +unify-fail+ is now :new-sentinel -- they differ.
            (is (not (eq fail-result cl-dbd::+unify-fail+))
                "After rebinding: unify returned ~S, +unify-fail+ is ~S -- ~
                 eq-based failure detection is broken"
                fail-result cl-dbd::+unify-fail+)))
      (setf cl-dbd::+unify-fail+ original))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Bug #8 – Dead (null x)(null y) clause in unify
;;; ─────────────────────────────────────────────────────────────────────────

(test bug-8/unify-nil-nil-handled-by-equal-clause
  "(equal nil nil) is T, so the first cond clause fires for two NILs; the
   subsequent (and (null x)(null y)) clause is unreachable dead code.
   Unification remains correct; this test documents the redundancy."
)
