;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; cl-ddb.lisp

(in-package #:cl-ddb)

;;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (import 'cld:<- (find-package :cl-user)))

(defpackage #:cl-ddb-user
  (:use #:cl #:cl-ddb #:dbi)
  (:nicknames #:cld-user))
  
  
