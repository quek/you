;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you.example.chat
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "chat"))
  :depends-on (you quek))
