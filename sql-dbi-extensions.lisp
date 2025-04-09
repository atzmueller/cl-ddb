;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb)

(defmacro it-if (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro it-when (test &body body)
  `(it-if ,test
	  (progn ,@body)))

(defmacro with-string-from-file ((string-variable filename) &body body)
  `(with-open-file (file ,filename :direction :input :external-format :utf-8)
     (let ((,string-variable (uiop:read-file-string file)))
       ,@body)))
       
(defmacro with-db-connection ((connection-variable
			       &key (driver :sqlite3) (db ":memory:") user password)
			      &body body)
  `(dbi:with-connection (,connection-variable ,driver :database-name ,db
					      ,@(it-when user `(:username ,it))
					      ,@(it-when password `(:password ,it)))
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


(defmacro with-sql-query ((query-variable sql-statement
			   &key (driver :sqlite3) (db ":memory:") user password)
			  &body body)
  `(with-db-connection (connection :driver ,driver :db ,db :user ,user :password ,password)
     (let* ((,query-variable (dbi:prepare connection ,sql-statement))
            (,query-variable (dbi:execute ,query-variable)))
       ,@body)))

