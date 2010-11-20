(cl:defpackage :you.example.blog
  (:use #:common-lisp #:you #:quek))

(series::install :pkg :you.example.blog :implicit-map t)
