;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb)


(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro with-string-from-file ((string-variable filename) &body body)
  `(with-open-file (file ,filename :direction :input :external-format :utf-8)
     (let ((,string-variable (uiop:read-file-string file)))
       ,@body)))
       
(defmacro with-db-connection ((connection-variable
			       &key (driver :sqlite3) (db ":memory:") user password)
			      &body body)
  `(dbi:with-connection (,connection-variable ,driver :database-name ,db
					      ,@(awhen user `(:username ,it))
					      ,@(awhen password `(:password ,it)))
     ,@body))

(defun do-sql* (connection statements)
  (let* ((terminated-p
	   (or (= (length statements) 0)
	       (string-equal ";"
			     (subseq statements
				     (1- (length statements))
				     (length statements)))))
	 (split-statements (uiop:split-string statements :separator ";")))
    (unless terminated-p
      (setf split-statements (nbutlast split-statements)))
    (LOOP :FOR statement :IN split-statements
	  :DO (dbi:do-sql connection statement))))

#|
;;; Examples

(with-db-connection (connection :db "dbs2024" :user "test" :password "test")
  (with-string-from-file (statement "test-sql-dsl.sql")
    (let* ((dbi-query (dbi:prepare connection statement))
	   (result (dbi:execute dbi-query)))
      (dbi:fetch-all result))))

(with-db-connection (connection :db "dbs2024" :user "test" :password "test")
  (with-string-from-file (statement "test-sql-dsl.sql")
    (dbi:do-sql connection statement)))

(with-db-connection (connection :db "dbs2024" :user "test" :password "test")
  (with-string-from-file (statement "test-sql-dsl.sql")
    (do-sql* connection statement)))
|#
