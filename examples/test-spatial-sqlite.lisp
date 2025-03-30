(in-package :cl-user)

;; might need to do something like this in .sbclrc
;; (setf (uiop:getenv "DYLD_LIBRARY_PATH")
;;      (uiop:strcat "/opt/homebrew/Caskroom/miniconda/base/lib/"))

;; or hotfix, or put libsqlite3 into the same directory as our file ...
(defun cffi::load-foreign-library-path (name path &optional search-path)
  "Tries to load PATH using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (let ((dirs (cffi::parse-directories cffi:*foreign-library-directories*)))
    (cffi::if-let (cffi::file (cffi::find-file path (append search-path dirs)))
      (handler-case
          (values (cffi::%load-foreign-library name (native-namestring cffi::file))
                  cffi::file)
        (simple-error (error)
          (cffi::report-simple-error name error)))
      (cffi::report-simple-error name error))))

(ql:quickload :cl-dbi)
(ql:quickload :sqlite)

;; int sqlite3_db_config(sqlite3*, int op, ...);

(cffi:defcfun sqlite3-db-config  :int
  (db sqlite-ffi:p-sqlite3)
  (op :int))


(ddb::with-db-connection (conn)
  (let* ((query (dbi:prepare conn "CREATE VIRTUAL TABLE demo_index USING rtree(
   id,              -- Integer primary key
   minX, maxX,      -- Minimum and maximum X coordinate
   minY, maxY       -- Minimum and maximum Y coordinate
);"))
         (query (dbi:execute query))
	 (query (dbi:prepare conn "INSERT INTO demo_index VALUES
  (28215, -80.781227, -80.604706, 35.208813, 35.297367),
  (28216, -80.957283, -80.840599, 35.235920, 35.367825),
  (28217, -80.960869, -80.869431, 35.133682, 35.208233),
  (28226, -80.878983, -80.778275, 35.060287, 35.154446),
  (28227, -80.745544, -80.555382, 35.130215, 35.236916),
  (28244, -80.844208, -80.841988, 35.223728, 35.225471),
  (28262, -80.809074, -80.682938, 35.276207, 35.377747),
  (28269, -80.851471, -80.735718, 35.272560, 35.407925),
  (28270, -80.794983, -80.728966, 35.059872, 35.161823),
  (28273, -80.994766, -80.875259, 35.074734, 35.172836),
  (28277, -80.876793, -80.767586, 35.001709, 35.101063),
  (28278, -81.058029, -80.956375, 35.044701, 35.223812),
  (28280, -80.844208, -80.841972, 35.225468, 35.227203),
  (28282, -80.846382, -80.844193, 35.223972, 35.225655);"))
	 (query (dbi:execute query))
	 (query (dbi:prepare conn "SELECT * FROM demo_index WHERE id=28269;"))
	 (query (dbi:execute query)))
    
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))

(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (sqlite3-db-config (sqlite::handle (dbi.driver:connection-handle conn)) 1005)
  ;;(sqlite3-enable-load-extension (sqlite::handle (dbi.driver:connection-handle conn)) 1)
  (let* ((query (dbi:prepare conn "SELECT load_extension('mod_spatialite');"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))


(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (let* ((query (dbi:prepare conn "select * from pragma_compile_options();"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))

(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (let* ((query (dbi:prepare conn "select sqlite_version();"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))


(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (let* ((query (dbi:prepare conn "CREATE VIRTUAL TABLE demo_index USING rtree(
   id,              -- Integer primary key
   minX, maxX,      -- Minimum and maximum X coordinate
   minY, maxY       -- Minimum and maximum Y coordinate
);"))
         (query (dbi:execute query))
	 (query (dbi:prepare conn "INSERT INTO demo_index VALUES
  (28215, -80.781227, -80.604706, 35.208813, 35.297367),
  (28216, -80.957283, -80.840599, 35.235920, 35.367825),
  (28217, -80.960869, -80.869431, 35.133682, 35.208233),
  (28226, -80.878983, -80.778275, 35.060287, 35.154446),
  (28227, -80.745544, -80.555382, 35.130215, 35.236916),
  (28244, -80.844208, -80.841988, 35.223728, 35.225471),
  (28262, -80.809074, -80.682938, 35.276207, 35.377747),
  (28269, -80.851471, -80.735718, 35.272560, 35.407925),
  (28270, -80.794983, -80.728966, 35.059872, 35.161823),
  (28273, -80.994766, -80.875259, 35.074734, 35.172836),
  (28277, -80.876793, -80.767586, 35.001709, 35.101063),
  (28278, -81.058029, -80.956375, 35.044701, 35.223812),
  (28280, -80.844208, -80.841972, 35.225468, 35.227203),
  (28282, -80.846382, -80.844193, 35.223972, 35.225655);"))
	 (query (dbi:execute query))
	 (query (dbi:prepare conn "SELECT * FROM demo_index WHERE id=28269;"))
	 (query (dbi:execute query)))
    
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))


(dbi:with-connection (conn :sqlite3 :database-name "sqlite-test.db")
  (let* ((query (dbi:prepare conn "SELECT * FROM test"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))

(dbi:with-connection (conn :sqlite3 :database-name "sqlite-test.db")
  (let* ((query (dbi:prepare conn "CREATE TABLE test (id INT, bar VARCHAR, baz VARCHAR);"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))


(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (let* ((query (dbi:prepare conn "CREATE TABLE test (id INT, bar VARCHAR, baz VARCHAR);"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))


(dbi:with-connection (conn :sqlite3 :database-name ":memory:")
  (let* ((query (dbi:prepare conn "SELECT * FROM load_extension"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))
