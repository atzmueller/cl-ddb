;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024 Martin Atzmueller

;;; datalog.lisp

(in-package :cl-ddb)

;;; (declaim (optimize (debug 3)))

(declaim (optimize (speed 1) (safety 1)))

;; data structure setup
;; core idea: use split-pointer for all-facts, so that instead of two
;; independent lists that are append-ed on every call, maintain a single
;; physically linked list where the IDB facts live at the head and the
;; EDB facts are the shared tail. A second pointer tracks the last IDB
;; cons cell so the link can be severed when the IDB is rebuilt.
;; *derived*                     *derived-tail*
;;     │                                │
;;     ▼                                ▼
;;  ┌────┬──┐   ┌────┬──┐   ┌────┬──┐  ╔════╦══╗   ┌────┬──┐   ┌────┬──┐
;;  │ d3 │ ─┼──>│ d2 │ ─┼──>│ d1 │ ─┼─>╠ d0 ║ ─╬──>│ f1 │ ─┼──>│ f0 │/│
;;  └────┴──┘   └────┴──┘   └────┴──┘  ╚════╩══╝   └────┴──┘   └────┴──┘
;;  ◄──────── IDB (derived) ──────────►│◄───────── EDB (facts) ──────────►
;;                                     │                  ▲
;;                                (cdr *derived-tail*)    │
;;                                  = *facts*             │
;;                                                   *facts*



(defvar *facts* '() "Extensional database: user-asserted base facts.")
(defvar *derived* '() "Intensional database: facts derived from rules.")
(defvar *derived-tail*  nil "Last cons cell belonging to the IDB part, or NIL.")
(defvar *rules* '() "Stored rules, each of the form (head ordered-body).")
(defvar *db-dirty* t "T when the EDB or rule set has changed since the last forward-chain run.")

;; data structures for efficient access
(defvar *pred-index*   (make-hash-table :test #'eq)
  "Predicate index: predicate-symbol -> list of known facts (EDB \union IDB).")
(defvar *all-fact-set*     (make-hash-table :test #'equal)
  "Hash set for O(1) duplicate detection: fact -> +base-fact+ or +derived-fact+.")

(declaim (type list *facts* *derived* *rules*)
         (type (or cons null) *derived-tail*)
         (type boolean *db-dirty*)
         (type hash-table *pred-index* *all-fact-set*))

(defconstant +unify-fail+ :fail
  "Constant returned by UNIFY-WITH-GROUND on failure.")

(defconstant +derived-fact+ :derived-fact)
(defconstant +base-fact+ :base-fact)

;;; index maintenance, for efficiency (in semi-naive evaluation below)

(defun index-add (fact pred-index)
  (declare (type hash-table pred-index))
  (assert (not (negated-literal-p fact)) (fact)
          "Negated literal ~S; cannot be added as a fact." fact)
  (push fact (gethash (literal-predicate fact) pred-index '())))

(defun facts-for (pred)
  "Get facts for predicate <pred> (hashed)"
  (declare (type symbol pred))
  (the list (gethash pred *pred-index* '())))

(defun rebuild-edb-indexes ()
  "Rebuild *pred-index* and *all-fact-set* from *facts* only."
  (clrhash *pred-index*)
  (clrhash *all-fact-set*)
  (dolist (fact *facts*)
    (index-add fact *pred-index*)
    (setf (gethash fact *all-fact-set*) +base-fact+)))

(defun snapshot-index ()
  "Shallow-copy *pred-index* into a new hash table for use as a delta."
  (let ((snap (make-hash-table :test #'eq)))
    (declare (type hash-table snap))
    (maphash (lambda (pred facts)
               (declare (type symbol pred)
                        (type list   facts))
               (setf (gethash pred snap) facts))
             *pred-index*)
    snap))

;;; core operations

(defun all-facts ()
  "Return all currently known facts (base ∪ derived) - zero allocation.
   *derived* is already physically linked to *facts* at its tail."
  (or *derived* *facts*))

(defun all-rules ()
  "Return all currently stored rules."
  *rules*)

(defun clear-dl-db ()
  "Clear the entire database: EDB, IDB, rules, and all indexes."
  (when *derived-tail*
    (setf (cdr *derived-tail*) nil))
  (setf *derived* nil *derived-tail* nil)
  (clrhash *pred-index*)
  (clrhash *all-fact-set*)
  (setf *facts* nil)
  (setf *rules* nil)
  (setf *db-dirty* t))

(defun clear-derived ()
  "Sever the IDB→EDB link and discard all derived facts.
   The EDB (*facts*) is left intact."
  (when *derived-tail*
    (setf (cdr *derived-tail*) nil))
  (setf *derived* nil
        *derived-tail* nil)
  (rebuild-edb-indexes))

(defun show-dl-db ()
  "Print the current EDB, IDB, and rule set to *standard-output*."
  (let ((derived-only
          (loop :for cell :on *derived*
                :until (eq cell *facts*)
                :collect (car cell))))
    (format *standard-output*
            "Current Datalog DB:~%Base facts:    ~S~%Derived facts: ~S~%Rules:         ~S~%"
            *facts* derived-only *rules*)))

(defun variable-p (x)
  "Return T if X is a Datalog variable (a symbol whose name begins with '?')."
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (declare (type simple-string name))
         (and (plusp (the fixnum (length name)))
              (char= (char name 0) #\?)))))

(defun negated-literal-p (literal)
  "Return T if LITERAL has the form (NOT <goal>)."
  (and (consp literal) (eq (car literal) 'not)))

(defun literal-predicate (literal)
  "Return the predicate symbol of LITERAL, stripping negation if present."
  (let ((pos (if (negated-literal-p literal) (cadr literal) literal)))
    (the symbol (if (consp pos) (car pos) pos))))

(defun add-fact (fact)
  "Assert FACT as a base fact. When a derived chain exists, the tail link is patched to keep the
   invariant: (cdr *derived-tail*) = *facts*."
  (let ((current (gethash fact *all-fact-set*)))
    (unless (eq current +base-fact+)
      (let ((new-cons (cons fact *facts*)))
        (setf *facts* new-cons)
        (when *derived-tail*
          (setf (cdr *derived-tail*) new-cons)))
      (setf (gethash fact *all-fact-set*) +base-fact+)
      ;; Fact is already in *pred-index* if it was previously derived.
      (unless (eq current +derived-fact+)
        (index-add fact *pred-index*))
      (setf *db-dirty* t))))

(defun ground-p (term)
  (cond ((variable-p term) nil)
        ((atom term) t)
        (t (and (ground-p (car term)) (ground-p (cdr term))))))


(defun add-derived-fact (fact)
  "Internal: Prepend FACT to the IDB chain, without touching the dirty flag.
   On the very first derived fact, record the cons cell as *derived-tail*
   (it is the junction between the IDB and the EDB).
   Returns T if FACT was not already known, NIL otherwise."
  (unless (gethash fact *all-fact-set*)
    (assert (ground-p fact) (fact)
            "Attempted to insert non-ground derived fact: ~S" fact)
    (let ((new-cons (cons fact (or *derived* *facts*))))
      (when (null *derived*)
        (setf *derived-tail* new-cons))
      (setf *derived* new-cons))
    (setf (gethash fact *all-fact-set*) +derived-fact+)
    (index-add fact *pred-index*)
    t))



(defun collect-variables (term)
  "Return a list of all Datalog variables in TERM."
  (cond ((variable-p term) (list term))
        ((consp term)
         (union (collect-variables (car term))
                (collect-variables (cdr term))
                :test #'eq))
        (t '())))

(defun check-rule-safety (head body)
  "Signal an error if any variable in HEAD or in a negated body literal does
   not appear in at least one positive body literal (Datalog safety condition)."
  (declare (type list body))
  (let* ((positive-body (remove-if #'negated-literal-p body))
         (safe-vars (reduce (lambda (acc lit)
                              (union acc (collect-variables lit) :test #'eq))
                            positive-body
                            :initial-value '())))
    (flet ((assert-safe (var context)
             (unless (member var safe-vars :test #'eq)
               (error "Safety violation: ~S in ~S is not bound by any positive body literal."
                      var context))))
      (dolist (var (collect-variables head))
        (assert-safe var head))
      (dolist (lit body)
        (when (negated-literal-p lit)
          (dolist (var (collect-variables (cadr lit)))
            (assert-safe var lit)))))))

(defun order-body (body)
  "Reorder BODY so that all positive literals precede negated ones.
   Combined with the safety condition this guarantees that every variable
   in a negated literal is already bound when that literal is evaluated."
  (declare (type list body))
  (append (remove-if #'negated-literal-p body)
          (remove-if-not #'negated-literal-p body)))

(defun rule-exists-p (rule)
  (member rule *rules* :test #'equal))

(defun add-rule (head body)
  "Add rule HEAD :- BODY after checking safety and reordering body literals."
  (let* ((ordered-body (order-body body))
         (rule (cons head ordered-body)))
    (check-rule-safety head ordered-body)
    (unless (rule-exists-p rule)
      (push rule *rules*)
      (setf *db-dirty* t))))

(defmacro <- (head &rest body)
  `(add-clause ',head ',body))

(defun add-clause (head body)
  (if (null body)
      (add-fact head)
      (add-rule head body)))

(defun walk (x bindings)
  "Follow the binding chain for X until reaching a non-variable
   or an unbound variable."
  (declare (type list bindings))
  (if (variable-p x)
      (let ((b (assoc x bindings :test #'eq)))
        (declare (type (or null cons) b))
        (if b (walk (cdr b) bindings) x))
      x))

(defun unify-with-ground (x ground bindings)
  "Unify X (may contain variables) with GROUND (guaranteed variable-free).
   Omits walk on GROUND and the occurs check — both are safe because GROUND
   contains no variables."
  (declare (type list bindings)
           (optimize (speed 3) (safety 1)))
  (let ((x (walk x bindings)))
    (cond
      ((equal x ground) bindings)
      ((variable-p x) (cons (cons x ground) bindings))
      ((and (consp x) (consp ground))
       (let ((b (unify-with-ground (car x) (car ground) bindings)))
         (if (eq b +unify-fail+) +unify-fail+
             (unify-with-ground (cdr x) (cdr ground) b))))
      (t +unify-fail+))))

(defun apply-substitutions (term bindings)
  "Recursively apply BINDINGS to TERM, following variable chains via walk."
  (declare (type list bindings)
           (optimize (speed 2) (safety 1)))
  (let ((walked (walk term bindings)))
    (cond
      ((variable-p walked) walked)
      ((atom walked) walked)
      (t (cons (apply-substitutions (car walked) bindings)
               (apply-substitutions (cdr walked) bindings))))))


(defun rule-head-predicate (rule)
  "Return the predicate symbol of a rule's head."
  (declare (type list rule))
  (let ((head (first rule)))
    (the symbol (if (consp head) (car head) head))))


(deftype stratum () '(and fixnum unsigned-byte))

(defun compute-strata (rules)
  "Assign a stratum number to every predicate in RULES using iterative
   relaxation (Bellman-Ford style):
     • p depends positively on q  ->  stratum(p) >= stratum(q)
     • p depends negatively on q  ->  stratum(p) > stratum(q)
   Signals an error if a negative cycle is detected."
  (declare (type list rules))
  (let ((strata (make-hash-table :test #'eq)))
    (declare (type hash-table strata))
    ;; Initialise every predicate to stratum 0.
    (dolist (rule rules)
      (setf (gethash (rule-head-predicate rule) strata) 0)
      (dolist (lit (rest rule))
        (setf (gethash (literal-predicate lit) strata) 0)))
    (let ((num-preds (the fixnum (hash-table-count strata)))
          (iterations 0)
          (changed t))
      (declare (type fixnum num-preds iterations)
               (type boolean changed))
      (loop :while changed
            :do (setf changed nil)                
                (dolist (rule rules)
                  (let ((head-pred (rule-head-predicate rule)))
                    (declare (type symbol head-pred))
                    (dolist (lit (rest rule))
                      (let* ((dep-pred (literal-predicate lit))
                             (dep-s (the stratum (gethash dep-pred strata 0)))
                             (req (the stratum (if (negated-literal-p lit) (1+ dep-s) dep-s)))
                             (cur (the stratum (gethash head-pred strata 0))))
                        (declare (type symbol dep-pred))
                        (when (> req cur)
                          (setf (gethash head-pred strata) req)
                          (setf changed t))))))
                ;; ── Cycle detection: only count iterations that made progress ─
                ;; A stratifiable programme with N predicates converges within N
                ;; progress-making iterations.  A negative cycle keeps changed = T
                ;; forever, so N+1 progress iterations implies a cycle.
                ;; (more than N iterations implies a negative cycle (pigeonhole))
                (when (and changed (> (incf iterations) num-preds))
                  (error "Unstratifiable program: a cycle through negation was detected."))))
    strata))

(defun group-rules-by-stratum (rules strata)
  "Return a SIMPLE-VECTOR V where V[s] is the list of rules at stratum s."
  (declare (type list rules)
           (type hash-table strata))
  (if (zerop (hash-table-count strata))
      #()
      (let* ((max-s (the stratum
                          (loop :for v :of-type fixnum
                                    :being :the :hash-values :of strata
                                :maximize v)))
             (groups (make-array (1+ max-s) :initial-element nil)))
        (declare (type stratum max-s)
                 (type simple-vector groups))
        (dolist (rule rules groups)
          (let ((s (the stratum (gethash (rule-head-predicate rule) strata 0))))
            (push rule (aref groups s)))))))

(defun fact-matches-p (goal)
  "Return T if any indexed fact unifies with GOAL."
  (declare (optimize (speed 3) (safety 1)))
  (dolist (fact (facts-for (literal-predicate goal)) nil)
    (unless (eq (unify-with-ground goal fact '()) +unify-fail+)
      (return t))))

(defun resolve-semi-naive (body bindings pivot delta-index)
  "Resolve BODY under BINDINGS; PIVOT selects which literal matches DELTA-INDEX."
  (declare (type list      body bindings)
           (type fixnum    pivot)
           (type hash-table delta-index)
           (optimize (speed 3) (safety 1)))
  (if (null body)
      (list bindings)
      (let* ((goal (apply-substitutions (car body) bindings))
             (rest-body (cdr body))
             (at-pivot (zerop pivot))
             (solutions '()))
        (declare (type list rest-body solutions)
                 (type boolean at-pivot))
        (if (negated-literal-p goal)
            (unless (fact-matches-p (cadr goal))
              (setq solutions
                    (resolve-semi-naive rest-body bindings (1- pivot) delta-index)))
            (let* ((pred (literal-predicate goal))
                   (candidates (if at-pivot
                                   (the list (gethash pred delta-index '()))
                                   (facts-for pred))))
              (declare (type symbol pred)
                       (type list   candidates))
              (dolist (fact candidates)
                (let ((new-b (unify-with-ground goal fact bindings)))
                  (unless (eq new-b +unify-fail+)
                    (dolist (sol (resolve-semi-naive rest-body new-b (1- pivot) delta-index))
                      (push sol solutions)))))))
        solutions)))

(defun apply-rule-semi-naive (rule delta-index next-delta-index)
  "Apply RULE under semi-naive evaluation; new facts written to NEXT-DELTA-INDEX."
  (declare (type list      rule)
           (type hash-table delta-index next-delta-index)
           (optimize (speed 2) (safety 1)))
  (let* ((head (first rule))
         (body (rest rule)))
    (declare (type list body))
    (flet ((emit (binding)
             (declare (type list binding))
             (let ((grounded (apply-substitutions head binding)))
               (when (add-derived-fact grounded)
                 (index-add grounded next-delta-index)))))
      (if (every #'negated-literal-p body)
          (dolist (binding (resolve-semi-naive body '() -1 delta-index))
            (emit binding))
          (loop :for pivot :of-type fixnum :from 0
                :for pivot-lit :in body
                :unless (negated-literal-p pivot-lit)
                  :do (when (gethash (literal-predicate pivot-lit) delta-index)
                        (dolist (binding (resolve-semi-naive body '() pivot delta-index))
                          (emit binding))))))))

(defun apply-rules-semi-naive (rules initial-delta)
  "Drive RULES to fixpoint starting from INITIAL-DELTA."
  (declare (type list rules)
           (type hash-table initial-delta))
  (let ((delta initial-delta))
    (declare (type hash-table delta))
    (loop
      (when (zerop (hash-table-count delta)) (return))
      (let ((next-delta (make-hash-table :test #'eq)))
        (declare (type hash-table next-delta))
        (dolist (rule rules)
          (apply-rule-semi-naive rule delta next-delta))
        (setf delta next-delta)))))

(defun forward-chain ()
  "Derive all consequences of the current EDB and rule set.
   The IDB is cleared and rebuilt from scratch so that removing a base
   fact that was shielding a negated body literal takes immediate effect.
   Re-derivation is skipped entirely when *db-dirty* is NIL.
   Rules are processed stratum by stratum so that lower strata are fully
   saturated before any negated literal in a higher stratum is evaluated."
  (when *db-dirty*
    (clear-derived)
    (setf *db-dirty* nil)
    (let* ((rules (all-rules))
           (strata (compute-strata rules))
           (groups (group-rules-by-stratum rules strata)))
      (declare (type list rules)
               (type hash-table strata)
               (type simple-vector groups))
      (loop :for s :of-type fixnum :from 0 :below (the fixnum (length groups))
            :for stratum-rules = (aref groups s)
            :when stratum-rules
            :do (apply-rules-semi-naive stratum-rules (snapshot-index))))))

(defun query (goal)
  "Query the database, triggering forward chaining if the database is dirty.
   Positive goal: returns a list of binding alists (one per matching fact).
   Negated goal (NOT <g>): returns (NIL) on success, NIL on failure."
  (forward-chain)
  (let* ((negated-p (negated-literal-p goal))
         (positive-goal (if negated-p (cadr goal) goal))
         (results '()))
    (declare (type boolean negated-p)
             (type list results))
    (dolist (fact (facts-for (literal-predicate positive-goal)))
      (let ((b (unify-with-ground positive-goal fact '())))
        (unless (eq b +unify-fail+)
          (push b results))))
    (if negated-p
        (if results nil (list nil))   ; invert for negated queries
        results)))

(defmacro ?- (goal)
  `(query ',goal))

