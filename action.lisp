(in-package :you)

(export '(defaction))

(defvar *response-stream* *standard-output*)

(defmacro defaction (name (&rest options) &body body)
  "options は権限的な何かに使えそう。"
  (declare (ignore options))
  `(progn
     (setf (get ',name :action) t)
     (defmethod ,name ()
       (with-http-parameters ,@body))))

(defun call-action-by-symbol (symbol)
  (unless (get symbol :action)
    (error "No such action(~a)." symbol))
  (funcall (symbol-function symbol)))

(defun call-action (url)
  (let ((*get-parameters* (hunchentoot:get-parameters*))
        (*post-parameters* (hunchentoot:post-parameters*)))
    ;;(with-db clsql-sys:*default-database*
    (with-output-to-string (*response-stream*)
      (with-db
        (ppcre:register-groups-bind (package symbol-name)
            ((format nil "~a([^/]+)/([^?/]+)" *url-prefix*) url)
          (iterate ((each (scan-lists-of-lists-fringe
                           (call-action-by-symbol
                            (find-symbol (string-upcase symbol-name)
                                         (intern (string-upcase package)
                                                 :keyword))))))
                   (render each *browser*)))))))
