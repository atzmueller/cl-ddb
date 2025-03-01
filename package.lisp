;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; package.lisp

(defpackage #:cl-ddb
  (:use #:cl #:dbi)
  (:nicknames #:ddb)
  (:export
   #:query
   #:<-
   #:def-schema
   #:def-relation
   #:attributes #:types #:name #:schema #:rows
   #:as
   #:select #:project #:rename #:x #:rel-union #:rel-diff
   #:s #:p #:r #:u #:--
   #:with-string-from-file #:with-db-connection #:do-sql*
   ))
