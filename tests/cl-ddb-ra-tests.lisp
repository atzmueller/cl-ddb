;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb-test)

(def-suite* cl-ddb-test-ra :in cl-ddb-test-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-schema s1 (a1 a2 a3))
  (def-schema s2 (b1 b2 b3))
  
  (def-schema s3 ("a1" "a2" "a3"))
  
  (def-relation r1 s1
    ((1 2 3)
     (4 5 6)
     (7 8 9)))
  
  (def-relation r0 s1
    ((1 2 3)
     (4 8 6)
     (11 12 13)))
  
  (def-relation r2 s2
    ((11 22 33)
     (44 55 66)
     (77 88 99)))

  (def-schema s4 (c1 c2))

  (def-relation r4 s4
    ((1 "b")
     (2 "d")
     (3 "f")))

  (def-schema foo (a b))

  (def-relation foobar foo
    ((1 t)
     (2 nil))))


(test t-nil-cases
  (is (equal '((1 t)) (rows (select (= b t) foobar))))
  (is (equal '((2 nil)) (rows (select (= b nil) foobar)))))
  


;; SELECT
(test simple-select
  (is (equal '((1 2 3)) (rows (select (= a1 1) r1))))
  (is (equal '((1 2 3)) (rows (s (= a1 1) r1))))
  (is (equal nil (rows (select (= c2 "a") r4))))
  (is (equal '((1 "b")) (rows (s (= c2 "b") r4)))))

;; PROJECT
(test simple-project
  (is (equal '((1) (2) (3)) (rows (p (c1) r4))))
  (is (equal '((1) (4) (7)) (rows (p (a1) r1)))))

;; with UNION
(test simple-select-union
  (is (equal '((1 2 3) (4 5 6)) (rows (s (or (= a1 1) (= a2 5)) (u r1 r1))))))
  
;; with DIFFERENCE
(test simple-select-difference
  (is (equal '() (rows (s (or (= a1 1) (= a2 5)) (-- r1 r1)))))
  (is (equal '((4 5 6)) (rows (s (or (= a1 1) (= a2 5)) (-- r1 r0))))))
  
