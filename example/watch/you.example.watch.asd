;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you.example.watch
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "watch"))
  :depends-on (you))
