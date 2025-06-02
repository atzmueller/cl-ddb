(in-package :cl-ddb-user)


(defun setup-example-db ()
  (<- (parent john mary))
  (<- (parent mary alice))
  (<- (ancestor ?x ?y) (parent ?x ?y))
  (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
  (<- (not-grandparent ?x ?y) (parent ?z ?y) (not (parent ?x ?z))))

(defun run-example ()
  (setup-example-db)
  (forward-chain)
  (let ((result (?- (not-grandparent john ?who))))
    (LOOP :FOR bindings :ON result :BY 'cdr :DO
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

