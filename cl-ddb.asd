;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; cl-ddb.asd

(asdf:defsystem #:cl-ddb
  :description "Describe cl-ddb here"
  :author "Martin Atzmueller <martin@atzmueller.net>"
  :license  "BSD"
  :version "0.1"
  :depends-on ("cl-dbi" "fiveam")
  :serial t
  :components ((:file "package")
	       (:file "datalog")
	       (:file "relational-algebra")
	       (:file "sql-dbi-extensions")
               (:file "cl-ddb"))
  :in-order-to ((test-op (test-op "cl-ddb/tests"))))


(asdf:defsystem #:cl-ddb/tests
  :description "cl-ddb-test-system"
  :author "Martin Atzmueller <martin@atzmueller.net>"
  :license  "BSD"
  :serial t
  :depends-on ("uiop")
  :components ((:file "tests/package")
	       (:file "tests/cl-ddb-test-system")
	       (:file "tests/cl-ddb-ra-tests"))
  :perform (asdf:test-op (o s)
		    (uiop:symbol-call :fiveam :run! 'cl-ddb-test:cl-ddb-test-system)))
