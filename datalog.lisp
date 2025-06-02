;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024 Martin Atzmueller

;;; datalog.lisp

(in-package :cl-ddb)

(declaim (optimize (debug 3)))

(defparameter *facts* '())
(defparameter *rules* '())

(defun get-facts () *facts*)
(defun get-rules () *rules*)

(defun clear-dl-db ()
  (setf *facts* nil)
  (setf *rules* nil))

(defun show-dl-db ()
  (format *standard-output* "Current Datalog DB:~%Facts:~A~%Rules:~A"
	  (get-facts)
	  (get-rules)))

(defun fact-exists-p (fact)
  (member fact *facts* :test #'equal))

(defun rule-exists-p (rule)
  (member rule *rules* :test #'equal))

(defun add-fact (fact)
  (unless (fact-exists-p fact)
    (push fact *facts*)))

(defun add-rule (head body)
  (let ((rule (cons head body)))
    (unless (rule-exists-p rule)
      (push rule *rules*))))

(defmacro <- (&rest args)
  `(assertion ,@args))

(defmacro assertion (&rest args)
  `(apply #'add-clause ',args))

(defun add-clause (head &rest body)
  (if (null body)
      (add-fact head)
      (add-rule head body)))

(defmacro iterate-with-rule (rules &body body)
  `(dolist (rule ,rules)
     ,@body))

(defmacro iterate-facts-with-fact (&body body)
  `(dolist (fact (get-facts))
     ,@body))

(defun variable-p (x)
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defparameter +unify-fail+ :fail)

(defun unify-variable (var value bindings)
  (let ((binding (assoc var bindings)))
    (cond
      ;; If the variable is already bound to the same value, return current bindings.
      ((and binding (equal (cdr binding) value)) bindings)

      ;; If the variable is bound to a different value, unification fails.
      ((and binding (not (equal (cdr binding) value))) +unify-fail+)

      ;; Otherwise, if the variable is unbound, create a new binding.
      (t (cons (cons var value) bindings)))))

(defun unify (x y bindings)
  (cond
    ((equal x y) bindings)
    ((and (null x) (null y)) bindings)
    ((variable-p x) (unify-variable x y bindings))
    ((variable-p y) (unify-variable y x bindings))
    ((and (consp x) (consp y))
     (let ((new-bindings (unify (car x) (car y) bindings)))
       (if (not (eq new-bindings +unify-fail+))
           (unify (cdr x) (cdr y) new-bindings)
           +unify-fail+)))
    (t +unify-fail+)))

(defun apply-substitutions (literal bindings)
  (cond
    ((null literal) nil)
    ((atom literal) 
     (or (cdr (assoc literal bindings)) literal))  ;; Substitute if a binding exists.
    (t (cons (apply-substitutions (car literal) bindings)
             (apply-substitutions (cdr literal) bindings)))))

(defun negated-literal-p (literal)
  (and (consp literal) (eq (car literal) 'not)))

(defun resolve-body (body bindings)
  (if (null body)
      (list bindings)
      (let ((first-goal (apply-substitutions (car body) bindings))  ;; Apply current bindings.
            (rest-goals (cdr body))                        ;; Remaining goals.
            (solutions '()))                               ;; Store solutions.
        (if (negated-literal-p first-goal)
            (let ((positive-goal (cadr first-goal)))  ;; Extract the positive form.
              (unless (fact-exists-p positive-goal)   ;; Negated goal succeeds if fact is absent.
                (dolist (solution (resolve-body rest-goals bindings))
                  (push solution solutions))))
            (iterate-facts-with-fact
              (let ((new-bindings (unify first-goal fact bindings)))  ;; Try unification.
                (when (and new-bindings (not (eq new-bindings +unify-fail+)))
                  (dolist (solution (resolve-body rest-goals new-bindings))
                    (push solution solutions))))))
        solutions)))

(defun apply-rule (rule)
  (let* ((head (car rule))
         (body (cdr rule))	 
	 (bindings (resolve-body body '()))
	 (new-fact-added nil))
    (when bindings
      (dolist (binding bindings)
	(let ((grounded-head (apply-substitutions head binding)))
          (unless (fact-exists-p grounded-head)
            (add-fact grounded-head)
	    (setf new-fact-added t)))))
    new-fact-added))

(defun apply-rules (rules)
  (let ((new-fact-added t))
    (loop 
      :while new-fact-added
      :do (setf new-fact-added nil)
          (iterate-with-rule rules
	    (when (apply-rule rule)
	      (setf new-fact-added t))))))

(defun rule-has-negation-p (rule)
  (some (lambda (literal) (and (consp literal) (eq (car literal) 'not))) (cdr rule)))

(defun stratify-rules ()
  (let ((without-negation '())
        (with-negation '()))
    (iterate-with-rule (get-rules)
      (if (rule-has-negation-p rule)
          (push rule with-negation)
          (push rule without-negation)))
    (values without-negation with-negation)))

(defun forward-chain ()
  (multiple-value-bind (without-negation with-negation) (stratify-rules)
    (apply-rules without-negation)
    (apply-rules with-negation)))

(defun query (goal)
  (forward-chain)
  (let* ((results '())
	 (negated-goal-p (negated-literal-p goal))
	 (positive-goal (if negated-goal-p (cdr goal) goal)))
    (iterate-facts-with-fact
      (let ((bindings (unify positive-goal fact '())))
        (when (and bindings (not (eq bindings +unify-fail+)))
	  (dolist (binding bindings)
	    (push binding results)))))
    results))

(defmacro ?- (goal)
  `(query ',goal))

