(in-package :cl-ddb-user)

;;; Examples

(defun setup-uni-db (connection)
  (with-string-from-file (setup-db "uni_db.sql")
    (do-sql* connection setup-db)
    (commit connection)))

(with-db-connection (connection)
  (setup-uni-db connection)
  (with-string-from-file (statement "sql-dsl-select-example.sql")
    (let* ((dbi-query (dbi:prepare connection statement))
	   (result (dbi:execute dbi-query)))
      (dbi:fetch-all result))))

(with-db-connection (connection)
  (setup-uni-db connection)
  (with-string-from-file (statement "sql-dsl-select-example.sql")
    (let* ((dbi-query (dbi:prepare connection statement))
	   (result (dbi:execute dbi-query)))
      (dbi:fetch-all result))))

(with-db-connection (connection)
  (setup-uni-db connection)
  (with-string-from-file (statement "sql-dsl-select-example.sql")
    (dbi:do-sql connection statement)))

(with-db-connection (connection)
  (setup-uni-db connection)
  (with-string-from-file (statement "sql-dsl-select-example.sql")
    (do-sql* connection statement)))


(with-db-connection (connection)
  (setup-uni-db connection)
  (let* ((prep-query (dbi:prepare connection "SELECT * FROM Professor"))
         (query (dbi:execute prep-query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A (~A) in room: ~A~%"
		     (getf row :|Name|)
		     (getf row :|Rank|)
		     (getf row :|Room|)))))
