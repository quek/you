(in-package :you)

(export '(defaction))

(defvar *default-action-class* 'action)

(defgeneric call-by-url (action url))

(defclass action ()
    ())

(defmacro defaction (name (&rest options) &body body)
  "options は権限的な何かに使えそう。"
  (declare (ignore options))
  `(progn
     (setf (get ',name :action) t)
     (defmethod ,name ((action ,*default-action-class*))
       (with-http-parameters ,@body))))

(defmethod call-by-url ((action action) url)
  (with-output-to-string (*response-stream*)
    (ppcre:register-groups-bind (package symbol-name) ("([^/]+)/([^?/]+)" url)
      (let ((action-symbol (find-symbol (string-upcase symbol-name)
                                        (find-package (string-upcase package)))))
        (iterate ((each (scan-lists-of-lists-fringe (call-by-symbol action action-symbol))))
          (render each *browser*))))))

(defmethod call-by-url :around ((action action) url)
  (with-db ()
    (call-next-method)))

(defmethod call-by-symbol ((action action) (symbol symbol))
  (unless (get symbol :action)
    (error "No such action(~a)." symbol))
  (funcall (symbol-function symbol) action))

(defun call-action (url)
  (let ((*get-parameters* (hunchentoot:get-parameters*))
        (*post-parameters* (hunchentoot:post-parameters*))
        (action (make-instance *default-action-class*)))
    (let ((res (call-by-url action (subseq url (length *url-prefix*)))))
      res)))

