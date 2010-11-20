(in-package :you)

;; (let ((host "localhost")
;;       (db "junk")
;;       (user "ancient")
;;       (passwd "password"))
;;   (defparameter *connection-spec* (list host db user passwd)))
;;
;; ;;(require :clsql-postgresql-socket)
;; ;;(defparameter *database-type* :postgresql-socket)
;;
;; (require :clsql-postgresql)
;; (defparameter *database-type* :postgresql)

(defparameter *database-type* :mysql)

(defgeneric connection-spec (environment))

(defmethod connection-spec ((environment production))
  '("localhost" "you_production" "root" ""))

(defmethod connection-spec ((environment development))
  '("localhost" "you_development" "root" ""))

(defmethod connection-spec ((environment test))
  '("localhost" "you_test" "root" ""))
