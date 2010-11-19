(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :you))

(defpackage :arc-challeng
    (:use :cl :you :quek))

(in-package :arc-challeng)

#|
(defop said req
  (aform [w/link (pr \"you said: \" (arg _ \"foo\"))
         (pr \"click here\")]
         (input \"foo\")
         (submit)))
|#

(defmacro default-template ((&key (title "Arc Challenge")) &body body)
  `(html (:head (:title ,title))
        (:body ,@body)))

(defaction arc1 ()
  (default-template ()
    (error-messages)
    (:form :action :arc2
           "foo: " (:input :type :text :name :foo)
           (:input :type :submit))))

(defaction arc2 ()
  (default-template ()
    (:a :href (url :arc3 :foo @foo) "ここよ")))

(defvalidation arc2 (:error-action arc1)
  (foo required :message "foo を入力してください。"))

(defaction arc3 ()
  (default-template ()
    #"""you said: "#,@foo""""
    (:br)
    (:a :href 'arc1 '戻る)))
