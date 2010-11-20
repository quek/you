;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "util")
               (:file "http")
               (:file "html")
               (:file "tag")
               (:file "js")
               (:file "sql-config")
               (:file "sql")
               (:file "strage")
               (:file "validation")
               (:file "action")
               (:file "you"))
  :depends-on (quek
               hunchentoot
               cl-ppcre
               clsql
               rucksack
               parenscript))
