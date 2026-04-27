;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; package.lisp

(unless (find-package "CL-DBD")
  (defpackage #:cl-dbd
    (:use #:cl #:dbi)
    (:nicknames #:dbd)
    (:export
     #:?-
     #:clear-dl-db
     #:forward-chain
     #:query
     #:show-dl-db
     #:<-
     #:defschema
     #:defoperator
     #:defrelation
     #:do-sql*
     #:attributes #:types #:name #:schema #:rows
     #:as
     #:select #:project #:rename #:x #:rel-union #:rel-diff
     #:s #:p #:r #:u #:--
     #:with-string-from-file #:with-db-connection #:with-sql-query
     )))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package "CL-DBD")
    (do-external-symbols (sym (find-package "CL-DBI"))
      (export sym (find-package "CL-DBD")))))
			      
