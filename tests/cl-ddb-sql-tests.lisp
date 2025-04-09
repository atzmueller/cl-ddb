;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb-test)

(def-suite* cl-ddb-test-sql :in cl-ddb-test-system)

(test sql-select
  (flet ((setup-uni-db (connection)
	   (with-string-from-file (setup-db "tests/uni_db.sql")
	     (do-sql* connection setup-db)
	     (commit connection))))
    (is (equalp
	 (with-db-connection (connection)
	   (setup-uni-db connection)
	   (with-string-from-file (statement "tests/test-sql-select.sql")
	     (let* ((dbi-query (dbi:prepare connection statement))
		    (result (dbi:execute dbi-query)))
	       (dbi:fetch-all result))))
	 '((:|CouNo| 4052 :|Title| "Logic" :SCH 4 :|TaughtBy| 2125)
	   (:|CouNo| 4630 :|Title| "The Three Critiques" :SCH 4 :|TaughtBy| 2137)
	   (:|CouNo| 5001 :|Title| "Fundamental Principles" :SCH 4 :|TaughtBy| 2137)
	   (:|CouNo| 5022 :|Title| "Faith and Knowledge" :SCH 2 :|TaughtBy| 2134)
	   (:|CouNo| 5041 :|Title| "Ethics" :SCH 4 :|TaughtBy| 2125)
	   (:|CouNo| 5043 :|Title| "Epistemology" :SCH 3 :|TaughtBy| 2126)
	   (:|CouNo| 5049 :|Title| "Maeeutics" :SCH 2 :|TaughtBy| 2125)
	   (:|CouNo| 5052 :|Title| "Philosophy of Science" :SCH 3 :|TaughtBy| 2126)
	   (:|CouNo| 5216 :|Title| "Bioethics" :SCH 2 :|TaughtBy| 2126)
	   (:|CouNo| 5259 :|Title| "The Vienna Circle" :SCH 2 :|TaughtBy| 2133))))))


(test sql-insert-delete
  (flet ((setup-uni-db (connection)
	   (with-string-from-file (setup-db "tests/uni_db.sql")
	     (do-sql* connection setup-db)
	     (commit connection))))
    (is (equalp
	 (progn
	   (with-db-connection (connection)
	     (setup-uni-db connection)
	     (with-string-from-file (statement "tests/test-sql-insert.sql")
	       (let* ((dbi-query (dbi:prepare connection statement))
		      (result (dbi:execute dbi-query)))
		 (dbi:fetch-all result))))
	   (with-db-connection (connection)
	     (setup-uni-db connection)
	     (with-string-from-file (statement "tests/test-sql-delete.sql")
	       (let* ((dbi-query (dbi:prepare connection statement))
		      (result (dbi:execute dbi-query)))
		 (dbi:fetch-all result))))
	   (with-db-connection (connection)
	     (setup-uni-db connection)
	     (with-string-from-file (statement "tests/test-sql-select.sql")
	       (let* ((dbi-query (dbi:prepare connection statement))
		      (result (dbi:execute dbi-query)))
		 (dbi:fetch-all result)))))
	 '((:|CouNo| 4052 :|Title| "Logic" :SCH 4 :|TaughtBy| 2125)
	   (:|CouNo| 4630 :|Title| "The Three Critiques" :SCH 4 :|TaughtBy| 2137)
	   (:|CouNo| 5001 :|Title| "Fundamental Principles" :SCH 4 :|TaughtBy| 2137)
	   (:|CouNo| 5022 :|Title| "Faith and Knowledge" :SCH 2 :|TaughtBy| 2134)
	   (:|CouNo| 5041 :|Title| "Ethics" :SCH 4 :|TaughtBy| 2125)
	   (:|CouNo| 5043 :|Title| "Epistemology" :SCH 3 :|TaughtBy| 2126)
	   (:|CouNo| 5049 :|Title| "Maeeutics" :SCH 2 :|TaughtBy| 2125)
	   (:|CouNo| 5052 :|Title| "Philosophy of Science" :SCH 3 :|TaughtBy| 2126)
	   (:|CouNo| 5216 :|Title| "Bioethics" :SCH 2 :|TaughtBy| 2126)
	   (:|CouNo| 5259 :|Title| "The Vienna Circle" :SCH 2 :|TaughtBy| 2133))))))


(test with-sql-query-finishes
  (finishes
    (cl-ddb:with-sql-query (query "select sqlite_version();")
      (print (cl-ddb:fetch query)))))

(test with-sql-query-fetch-finishes
  (finishes
    (cl-ddb:with-sql-query (query "select * from pragma_compile_options();")
      (loop for row = (dbi:fetch query)
            while row
            do (format t "~A~%" row)))))
