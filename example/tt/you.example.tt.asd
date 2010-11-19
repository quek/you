;;;; -*- Mode: LISP; -*-
(asdf:defsystem :you.example.tt
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "tt"))
  :depends-on (you parenscript simple-date-time))
