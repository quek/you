(in-package :you)

(export '(redirect))

(defvar *response-stream* *standard-output*)

(defun redirect (url &optional (dir (directory-namestring (hunchentoot:request-uri*))))
  (hunchentoot:redirect (str dir (to-html url))))

