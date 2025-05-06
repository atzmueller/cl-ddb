;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb-user)

#| Examples - simple schema |#

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

(print (x r1 r2))

(print (rename a111 a1 r1))

(rename r1-copy r1)

(select (= a1 1) r1)

(select (or (= a111 1) (= a2 5)) (rename a111 a1 (rename r1-copy r1)))

(select (or (= a1 1) (= a2 5)) (rel-union r1 r1))

(select (or (= a1 1) (= a2 5)) (rel-diff r1 r1))

(select (or (= a1 1) (= a2 5)) (rel-diff r1 r0))

(project (a1) r1)

(def-schema s4 (c1 c2))
(def-relation r4 s4
  ((1 "b")
   (2 "d")
   (3 "f")))

(print (select (= c1 1) r4))
(print (select (= c2 "a") r4))
(print (select (= c2 "b") r4))

(print (project(c1) r4 ))
(print (project(c2) r4 ))


;;; now, in "relational-algebra-like"-form

(print (x r1 r2))

(print (r a111 a1 r1))

(r r1-copy r1)

(s (= a1 1) r1)

(s
 (or (= a111 1) (= a2 5))
 (-- a111 a1 (r r1-copy r1)))

(s (or (= a1 1) (= a2 5)) (u r1 r1))

(s (or (= a1 1) (= a2 5)) (-- r1 r1))

(s (or (= a1 1) (= a2 5)) (-- r1 r0))

(p (a1) r1)



(def-schema requires (Predecessor Successor))

(print requires)

(def-relation r6 requires
  ((5001 5041)
   (5001 5043)
   (5001 5049)
   (5041 5216)
   (5043 5052)
   (5041 5052)))

(print r6)

(s (= Predecessor 5001) r6)



;;; Uni-DB example

(def-schema schema-prof (perno name rank room))
(def-schema schema-course (couno title sch taughtby))

(def-relation professor schema-prof
  ((2125 "Sokrates" "W3" 226)
   (2126 "Russel" "W3" 232)
   (2127 "Kopernikus" "W2" 310)
   (2133 "Popper" "W2" 52)
   (2134 "Augustinus" "W2" 309)
   (2147 "Curie" "W3" 36)
   (2137 "Kant" "W3" 7)))

(def-relation course schema-course
  ((5001 "Fundamental Principles" 4 2137)
   (5041 "Ethics" 4 2125)
   (5043 "Epistemology" 3 2126)
   (5049 "Maeeutics" 2 2125)
   (4052 "Logic" 4 2125)
   (5052 "Philosophy of Science" 3 2126)
   (5216 "Bioethics" 2 2126)
   (5259 "The Vienna Circle" 2 2133)
   (5022 "Faith and Knowledge" 2 2134)
   (4630 "The Three Critiques" 4 2137)))

(def-schema schema-attends (stuno couno))

(def-relation attends schema-attend
  ((26120 5001)
   (27550 5001)
   (27550 4052)
   (28106 5041)
   (28106 5052)
   (28106 5216)
   (28106 5259)
   (29120 5001)
   (29120 5041)
   (29120 5049)
   (29555 5022)
   (25403 5022)
   (29555 5001)))


(def-schema require-schema (predecessor successor))

(def-relation requires require-schema
  ((5001 5041)
   (5001 5043)
   (5001 5049)
   (5041 5216)
   (5043 5052)
   (5041 5052)
   (5052 5259)))


;;; (1) All titles of courses having less than four semester credit hours (SCH)
(project (title)
	 (select (< sch 4) course))

;;; (2) The names of all the professors who do not teach any courses
(-- (p (name) professor)
    (p (name)
       (s (= perno taughtby)
	  (x professor course))))

;;; (3) All requirements (as course numbers) for the course "Wissenschaftstheorie" (CouNo=5052).
(p (predecessor)
   (s (= couno 5052)
      (s (= couno successor)
	 (x course require))))



(x professor course)
(print (x professor course))

(s (= rank "W3") professor)

(s (and (= perno taughtby) (< sch 4))
   (x professor course))

(as name-title
    (p (name title)
       (select (and (= perno taughtby) (< sch 4))
	       (x professor course))))

(s (or (= name "Curie") (= name "Kant")) name-title)
(s (or (= name "Sokrates") (= name "Russel")) name-title)


