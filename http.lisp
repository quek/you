(in-package :you)

(export '(redirect))

(defun redirect (url &optional (dir (directory-namestring (hunchentoot:request-uri*))))
  (hunchentoot:redirect (string+ dir (print-to-html url))))

