;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller, Toan Nguyen

(in-package :cl-ddb-user)

;;; Uni-DB example

;; professors

(def-schema schema-prof (perno name rank room))

(def-relation professor schema-prof
  ((2125 "Sokrates" "W3" 226)
   (2126 "Russel" "W3" 232)
   (2127 "Kopernikus" "W2" 310)
   (2133 "Poppper" "W2" 52)
   (2134 "Augustinus" "W2" 309)
   (2147 "Curie" "W3" 36)
   (2137 "Kant" "W3" 7)))
   
 ;; courses
   
(def-schema schema-course (couno title sch taughtby))

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

;; attends

(def-schema schema-attends (stuno couno))

(def-relation attends schema-attends
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


;; requires

(def-schema requires-schema (predecessor successor))

(def-relation requires requires-schema
  ((5001 5041)
   (5001 5043)
   (5001 5049)
   (5041 5216)
   (5043 5052)
   (5041 5052)
   (5052 5259)))
   
 ;; assistants
   
(def-schema assistant-schema (perno name subject boss))

(def-relation assistant assistant-schema
  ((3002 "Platon" "Theory of Ideas" 2125)
   (3003 "Aristoteles" "Syllogistics" 2125)
   (3004 "Wittgenstein" "Theory of Language" 2126)
   (3005 "Rhetikus" "Planetary Motion" 2127)
   (3006 "Newton" "Kepler's Laws" 2127)
   (3007 "Spinoza" "God and Nature" 2134)))
   
 ;; students
   
 (def-schema student-schema (stuno name semester))
 
 (def-relation student student-schema
  ((24002 "Xenokrates" 18)
   (25403 "Jonas" 12)
   (26120 "Fichte" 10)
   (26830 "Aristoxenos" 8)
   (27550 "Schopenhauer" 6)
   (28106 "Carnap" 3)
   (29120 "Theophrastos" 2)
   (29555 "Feuerbach" 2)))

;; tests

(def-schema tests-schema (stuno couno perno grade))

(def-relation tests  tests-schema
  ((28106 5001 2126 1.0)
   (25403 5041 2125 2.0)
   (27550 4630 2137 2.0)))
