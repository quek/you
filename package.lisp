(in-package :cl-user)

(defpackage :you
    (:use #:common-lisp #:quek #:anaphora))

(series::install :implicit-map t :pkg :you)

