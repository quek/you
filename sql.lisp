(in-package :you)

(export '(do-query
          execute-sql
          *result-set*
          with-result-set
          basic-view-mixin
          text
          id
          id-mixin
          created-at
          created-at-mixin
          updated-at
          updated-at-mixin
          with-db*
          with-db))


(defmacro with-db* ((&optional (db-var 'clsql:*default-database*)) &body body)
  `(clsql:with-database (,db-var (connection-spec *env*) :make-default t :pool t
                                 :encoding :utf-8 :database-type *database-type*)
     (clsql-sys::start-sql-recording)
     (unwind-protect
          (progn
            (clsql:execute-command "set character_set_client='utf8'")
            (clsql:execute-command "set character_set_connection='utf8'")
            (clsql:execute-command "set character_set_results='utf8'")
            (clsql-sys:with-transaction (:database ,db-var)
              ,@body))
       (clsql-sys::stop-sql-recording))))

(defmacro with-db (&body body)
  `(with-db* () ,@body))


(defun ensure-database-exist ()
  (let ((exist (and (ignore-errors (with-db (clsql:query "select 1"))) t)))
    (unless exist
      (clsql-sys:create-database (connection-spec *env*) :database-type *database-type*))
    exist))
;; (clsql:destroy-database (connection-spec *env*) :database-type *database-type*)
;; (ensure-database-exist)


(defun |#q-quote-reader| (stream char)
  (declare (ignore char))
  (with-output-to-string (out)
    (loop for c = (read-char stream t nil t)
          until (and (char= #\' c)
                     (char/= #\' (peek-char nil stream nil #\a t)))
          do (progn
               (write-char c out)
               (when (char= c #\')
                 (read-char stream))))))

(defun |#q-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\, (lambda (x y) (declare (ignore x y)) :|,|))
    (set-macro-character #\' #'|#q-quote-reader|)
    `(quote ,(read stream t nil t))))

(set-dispatch-macro-character #\# #\q #'|#q-reader|)

(defgeneric >sql (x)
  (:method (x)
    (princ-to-string x))
  (:method ((x string))
    (str #\' (cl-ppcre:regex-replace-all "'" x "''") #\'))
  (:method ((x symbol))
    (substitute #\_ #\- (symbol-name x))))

(defun sexp>sql (sexp)
  (with-output-to-string (out)
    (loop for i in sexp
          do (typecase i
               (symbol (princ (>sql i) out))
               (list
                  (princ "(" out)
                  (princ (sexp>sql i) out)
                  (princ ")" out))
               (t (princ (>sql i) out)))
          do (princ " " out))))

(defun replace-query-parameter (parameter value query)
  (labels ((f (q)
             (if (atom q)
                 (if (eq parameter q)
                     value
                     q)
                 (cons (f (car q)) (f (cdr q))))))
    (f query)))

(defun m-replace-query-parameter (query parameters)
  (if parameters
      (m-replace-query-parameter
       `(replace-query-parameter ,(car parameters) ,(cadr parameters) ,query)
       (cddr parameters))
      query))

(defun make-query-result-assoc (row fields)
  (loop for r in row
        for f in fields
        collect (cons (intern (string-upcase f) :keyword) r)))

(defvar *result-set*)

(defmacro with-result-set ((result-set) &body body)
  (labels ((result-symbol-p (x)
             (and (symbolp x) (symbol-head-p x "$")))
           (key-symbol (x)
             (intern (subseq (symbol-name x) 1) :keyword))
           (collect-result-symbol (body symbols)
             (if (atom body)
                 (if (result-symbol-p body)
                     (adjoin body symbols)
                     symbols)
                 (collect-result-symbol
                  (car body)
                  (collect-result-symbol (cdr body) symbols)))))
    `(let ,(mapcar (^ `(,_ (cdr (assoc ,(key-symbol _)
                                       ,result-set))))
            (collect-result-symbol body nil))
       ,@body)))

(defmacro do-query ((query &rest params) &body body)
  (labels ((result-symbol-p (x)
             (and (symbolp x) (symbol-head-p x "$")))
           (key-symbol (x)
             (intern (subseq (symbol-name x) 1) :keyword))
           (collect-result-symbol (body symbols)
             (if (atom body)
                 (if (result-symbol-p body)
                     (adjoin body symbols)
                     symbols)
                 (collect-result-symbol
                  (car body)
                  (collect-result-symbol (cdr body) symbols)))))
    (alexandria:with-gensyms (result field-names row)
      `(multiple-value-bind (,result ,field-names)
           (clsql-sys:query (sexp>sql
                             ,(m-replace-query-parameter query params)))
         (loop for ,row in ,result
               for *result-set* = (make-query-result-assoc ,row ,field-names)
               collect (with-result-set (*result-set*)
                         ,@body))))))

(defun execute-sql (sql &rest parameters)
  (loop for (p v) on parameters by #'cddr
        do (setq sql (replace-query-parameter p v sql)))
  (clsql-sys:execute-command (sexp>sql sql)))


(deftype clsql::text ()
  'string)

(export 'clsql::text :clsql)

(defmethod clsql-sys:database-get-type-specifier ((type (eql 'clsql::text)) args database db-type)
           (declare (ignore args database db-type))
           "TEXT")

(defun print-slots (object &optional (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~{~{~a: ~a~}~^ ~}"
            (collect
                (#M(^ (list _ (slot-value object _)))
                   (choose-if
                    (complement (^ eq 'clsql-sys::view-database _))
                    (c2mop:slot-definition-name
                     (scan (c2mop:class-slots (class-of object))))))))))

(clsql:def-view-class id-mixin ()
  ((id :initarg :id :accessor id :type integer
       :db-kind :key
       :db-constraints :auto-increment)))

(clsql:def-view-class created-at-mixin ()
  ((created-at :initarg :created-at :initform (clsql:get-time)
               :accessor created-at
               :type clsql:wall-time)))

(clsql:def-view-class updated-at-mixin ()
  ((updated-at :initarg :updated-at :initform (clsql:get-time)
               :accessor updated-at
               :type clsql:wall-time)))

(clsql:def-view-class basic-view-mixin (id-mixin created-at-mixin updated-at-mixin)
  ())

(defmethod print-object ((self  basic-view-mixin) stream)
  (print-slots self stream))

(defmethod clsql:update-records-from-instance :before ((self updated-at-mixin) &rest args)
  (declare (ignore args))
  (setf (updated-at self) (clsql:get-time)))


#|
(let ((q #q(select * from todo where content like :a and done = :b)))
  (do-query (q :a "%の%" :b "t")
    ;;(print *result-set*)
    (print (list $done $id $content))))

【急募】
(let ((x 1) (y 2) (q1 "x") (q2 "y"))
  (list (xxx q1) (xxx q2)
        (let ((x 10) (y 20))
          (list (xxx q1) (xxx q2)))))
;; => (1 2 (10 20)) となる関数 xxx
というのは無理らしい。。。



(clsql-sys:connect '("localhost" "junk" "ancient" "password")
                   :database-type :postgresql-socket
                   :if-exists :new
                   :make-default t)
|#
