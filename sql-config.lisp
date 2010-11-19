(in-package :you)

(let ((host "localhost")
      (db "junk")
      (user "ancient")
      (passwd "password"))
  (defparameter *connection-spec* (list host db user passwd)))


;;(require :clsql-postgresql-socket)
;;(defparameter *database-type* :postgresql-socket)

(require :clsql-postgresql)
(defparameter *database-type* :postgresql)

