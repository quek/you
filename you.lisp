(in-package :you)

(export '(start))

(setf hunchentoot:*hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8)
      hunchentoot:*default-content-type* "text/html; charset=utf-8"
      hunchentoot:*catch-errors-p* nil
      hunchentoot:*show-lisp-errors-p* t)

(defvar *url-prefix* "/you/")
(defvar *port* 8888)

(defgeneric dispatch ()
  (:method ()
    (call-action (hunchentoot:request-uri hunchentoot:*request*))))

(defvar *dispatch*
  (hunchentoot:create-prefix-dispatcher *url-prefix* 'dispatch))

(pushnew *dispatch* hunchentoot:*dispatch-table*)

(defvar *server* nil)

(defun start ()
  (let ((acceptor (make-instance 'hunchentoot:acceptor :port *port*)))
    (setf *server* (hunchentoot:start acceptor))))

