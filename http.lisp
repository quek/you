(in-package :you)

(export '(redirect))

(defvar *response-stream* *standard-output*)

(defun redirect (url)
  (hunchentoot:redirect url))

