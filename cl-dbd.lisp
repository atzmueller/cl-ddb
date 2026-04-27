;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

;;;; cl-dbd.lisp

(in-package #:cl-dbd)

;;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (import 'cld:<- (find-package :cl-user)))

(defpackage #:cl-dbd-user
  (:use #:cl #:cl-dbd #:dbi)
  (:nicknames #:dbd-user))
  
  
