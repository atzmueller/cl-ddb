;;; Semantic Information Systems Group, Osnabrück University
;;; (C) 2024-2025 Martin Atzmueller

(in-package :cl-ddb)

#+:sbcl (declaim (optimize (debug 3) (safety 3)))

(defclass schema ()
  ((attributes :initform nil :initarg :attributes :accessor
               attributes)
   (types :initform nil :initarg :types :accessor types)))

(defmethod print-object ((schema schema) stream)
  (print-unreadable-object (schema stream :type t)
    (format stream "~a: ~a ; " 'attributes (attributes schema))
    (format stream "~a: ~a" 'types (types schema))))

(defclass relation ()
  ((name :initform nil :initarg :name :accessor name)
   (schema :initform nil :initarg :schema :accessor schema)
   (rows :initform nil :initarg :rows :accessor rows)))

(defmethod print-object ((relation relation) stream)
  (print-unreadable-object (relation stream :type t)
    (format stream "~a: ~a ; " 'name (name relation))
    (format stream "~a: ~a;" 'schema (schema relation))
    (format stream "~%~a: ~a" 'rows (rows relation))))


(defmacro as (name expression)
  `(progn
     (#+:sbcl sb-ext:without-package-locks #-:sbcl progn
      (defparameter ,name ,expression))
     ,name))

(defmacro def-schema (name attributes &optional types)
  `(progn
     (assert (symbolp ',name) nil "~A is not a symbol" ',name)
     (assert (listp ',attributes) nil "~A is not a list of attributes" ',attributes)
     (as ,name (make-instance 'schema :attributes ',attributes :types ',types))))

(defmacro def-relation (name schema rows)
  `(progn
     (assert (symbolp ',name) nil "~A is not a symbol" ',name)
     (assert (eql (type-of ,schema) 'schema) nil "~A is not a schema" ,schema)
     (assert (listp ',rows) nil "~A is not a list of tuples" ',rows)
     (as ,name (make-instance 'relation :name ',name :schema ,schema :rows ',rows))))


;; necessary operations are:
;; select, project, rename, cross-product, union, diff

(defun string-starts-with-p (string prefix)
  (unless (< (length string) (length prefix))
    (equal (subseq string (length prefix)) prefix)))

(defun make-qualified-attribute-symbol (attribute relation)
  (let* ((attribute-string (symbol-name attribute))
         (relation-string (symbol-name (name relation))))
    (if (string-starts-with-p attribute-string (concatenate 'string relation-string "."))
        attribute-string
        (concatenate 'string relation-string "." attribute-string))))

(defun get-attribute-value (relation attribute row)
  (let ((position (position attribute (attributes (schema relation)))))
    (if position
        (elt row position)
        (error "Attribute ~A not found in schema ~A" attribute (schema relation)))))


(defun expand-expression (expression safe-relation)
  (cond ((eql expression '=)
         'equal)
         ((or (member expression '(> < >= <= NOT AND OR))
             (numberp expression)
             (stringp expression)
             (eq expression t)
             (eq expression nil))
         expression)
        ((symbolp expression)
         `(get-attribute-value ,safe-relation ',expression row))
        (t
         (cons (expand-expression (first expression) safe-relation)
               (mapcar #'(lambda (e) (expand-expression e safe-relation))
                       (rest expression))))))

(defmacro select (predicate relation)
  (let ((safe-relation (gensym)))
    `(let ((,safe-relation ,relation))
       (labels ((predicate-lambda (row)
                  ,(expand-expression predicate safe-relation)))
         (let ((result-rows
                 (LOOP :FOR row in (rows ,safe-relation)
                       :NCONC (let ((result (funcall #'predicate-lambda row)))
                                (when result
                                  (list row))))))
           (make-instance 'relation
                          :schema (make-instance 'schema :attributes (attributes (schema ,safe-relation)))
                          :rows result-rows))))))

;;; (defmacro σ (predicate relation) ;; σ is U+03C3
;;;  `(select ,predicate ,relation))

(defmacro s (predicate relation)
  `(select ,predicate ,relation))

(defmacro project (attributes relation)
  (let ((safe-relation (gensym)))
    `(let ((,safe-relation ,relation))
       (let* ((attribute-positions
                (mapcan #'(lambda (attribute)
                            (let ((pos (position attribute (attributes (schema ,safe-relation)))))
                              (if pos
                                  (list pos)
                                  (error "Attribute ~A not found in schema ~A" attribute (schema ,safe-relation)))))
                        ',attributes))
              (existing-attributes (mapcar #'(lambda (index)
                                               (elt (attributes (schema ,safe-relation)) index))
                                           attribute-positions))
              (projected-rows (mapcar #'(lambda (row)
                                          (mapcar #'(lambda(index)
                                                      (elt row index))
                                                  attribute-positions))
                                      (rows ,safe-relation))))
         (make-instance 'relation
                        :schema (make-instance 'schema :attributes existing-attributes)
                        :rows (remove-duplicates projected-rows :test #'equal))))))
                                                     
;;; (defmacro π (attributes relation) ;; π is U+03C0
;;;   `(project ,attributes ,relation))

(defmacro p (attributes relation)
  `(project ,attributes ,relation))

(defun do-rename (new old relation)
  (let* ((attributes (attributes (schema relation)))
         (new-attributes (substitute new old attributes))
         (new-schema (make-instance 'schema :attributes new-attributes))
         (new-relation (make-instance 'relation :schema new-schema :rows (rows relation))))
    new-relation))

(defmacro rename-attribute (new old relation)
  `(do-rename ',new ',old ,relation))

(defmacro rename-relation (new old)
  `(def-relation ,new ,(schema (symbol-value old)) ,(rows (symbol-value old))))

(#+:sbcl sb-ext:without-package-locks #-:sbcl progn
 (defmacro rename (new old &optional relation)
   (if relation
       `(rename-attribute ,new ,old ,relation)
       `(rename-relation ,new ,old))))

;;; (defmacro ρ (new old &rest relation) ;; ρ is U+03C1
;;;  `(rename ,new ,old ,@relation))

(defmacro r (new old &rest relation)
  `(rename ,new ,old ,@relation))

(defun cartesian-product (l1 l2)
  (LOOP :FOR x :IN l1 :NCONC (LOOP :FOR y :IN l2 :COLLECT (append x y))))

(defmethod x ((r1 relation) (r2 relation))
  (make-instance 'relation
                 :name (format nil "~A x ~A" (name r1) (name r2))
                 :schema (make-instance 'schema :attributes (append (attributes (schema r1))
                                                                    (attributes (schema r2))))
                 :rows (remove-duplicates (cartesian-product (rows r1) (rows r2)) :test #'equal)))

(defmethod rel-union ((r1 relation) (r2 relation))
  (assert (equalp (attributes (schema r1)) (attributes (schema r2)))
          nil "Schemas of ~A and ~A do not match" r1 r2)
  (make-instance 'relation
                 :name (format nil "~A x ~A" (name r1) (name r2))
                 :schema (make-instance 'schema :attributes (attributes (schema r1)))
                 :rows (remove-duplicates (union (rows r1) (rows r2)) :test #'equalp)))

(defmethod u ((r1 relation) (r2 relation))
  (rel-union r1 r2))

(defun get-seq1-sort-order-for-seq2 (seq1 seq2)
  (LOOP :FOR elt :IN seq1 :COLLECT (position elt seq2)))

(defun sort-seq-by-order (sequence order)
  (LOOP :FOR index :IN order :COLLECT (elt sequence index)))

(defmethod rel-diff ((r1 relation) (r2 relation))
  (assert (and (null (set-difference (attributes (schema r1)) (attributes (schema r2))))
               (null (set-difference (attributes (schema r2)) (attributes (schema r1)))))
          nil "Schemas of ~A and ~A do not match" r1 r2)
  (let* ((r2-attributes-order-by-r1
           (get-seq1-sort-order-for-seq2 (attributes (schema r1))
                                         (attributes (schema r2))))
         (r2-rows-resorted-by-r1
           (mapcar #'(lambda (row)
                       (sort-seq-by-order row r2-attributes-order-by-r1))
                   (rows r2))))
    (make-instance 'relation
                   :name (format nil "~A x ~A" (name r1) (name r2))
                   :schema (make-instance 'schema :attributes (attributes (schema r1)))
                   :rows (set-difference (rows r1) r2-rows-resorted-by-r1  :test #'equal))))

(defmethod -- ((r1 relation) (r2 relation))
  (rel-diff r1 r2))


(defun substitute-var-symbol-by-expression-recursively (list symbol expression)
  (cond
    ((null list) nil)
    ((listp (first list))
     (cons (substitute-var-symbol-by-expression-recursively
            (first list) symbol expression)
           (substitute-var-symbol-by-expression-recursively
            (rest list) symbol expression)))
    ((equalp (first list) symbol)
     (cons expression (substitute-var-symbol-by-expression-recursively
                       (rest list) symbol expression)))
    (t
     (cons (first list) (substitute-var-symbol-by-expression-recursively
                         (rest list) symbol expression)))))
;;; (substitute-var-symbol-by-expression-recursively '(a b c) 'a '("bla"))


(defmacro def-ra-operator (name (rvar1 &optional rvar2) &body body)
  `(defmacro ,name (rel1 &optional rel2)
     ,(if rvar2
          ``(let ((,',rvar1 ,rel1) (,',rvar2 ,rel2))
              ,'(progn
                 ,@body))
          ``(let ((,',rvar1 ,rel1))
              ,'(progn
                 ,@body)))))
