;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you.example.todo
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "todo"))
  :depends-on (you parenscript))
