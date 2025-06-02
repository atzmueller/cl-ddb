;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; package.lisp

(unless (find-package "CL-DDB")
  (defpackage #:cl-ddb
    (:use #:cl #:dbi)
    (:export
     #:?-
     #:clear-dl-db
     #:forward-chain
     #:query
     #:show-dl-db
     #:<-
     #:def-schema
     #:def-ra-operator
     #:def-relation
     #:do-sql*
     #:attributes #:types #:name #:schema #:rows
     #:as
     #:select #:project #:rename #:x #:rel-union #:rel-diff
     #:s #:p #:r #:u #:--
     #:with-string-from-file #:with-db-connection #:with-sql-query
     )))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package "CL-DDB")
    (do-external-symbols (sym (find-package "CL-DBI"))
      (export sym (find-package "CL-DDB")))))
			      
