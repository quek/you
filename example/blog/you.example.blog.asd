;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you.example.blog
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "blog"))
  :depends-on (#:you))
