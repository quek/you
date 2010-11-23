(in-package :you)

(export '(defaction))

(defvar *default-action-class* 'action)

(defgeneric call-by-url (action url))

(defclass action ()
    ())

;; TODO ^$ #anchor
(defun default-route-from-symbol (symbol)
  (format nil "~(~a/~a~)"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

;; TODO ^$ #anchor
(defmacro defaction (name (&key (route (default-route-from-symbol name))) &body body)
  (alexandria:with-gensyms (url)
    `(progn
       (setf (get ',name :action) t)
       ,(multiple-value-bind (regexp bindings) (path-to-regexp route)
          `(add-route ',name
                      (lambda (,url)
                        ,(if bindings
                             `(ppcre:register-groups-bind ,bindings (,regexp ,url)
                                (values ',name
                                        (list ,@(collect (#M(lambda (x) `(cons ',x ,x))
                                                            (scan bindings))))))
                             `(when (ppcre:scan ,regexp ,url) ',name)))))
       (defmethod ,name ((action ,*default-action-class*))
                  (with-http-parameters ,@body)))))

(defmethod call-by-url ((action action) url)
  (with-output-to-string (*response-stream*)
    (multiple-value-bind (action-symbol bindins) (get-route url)
      (let ((*get-parameters* (append bindins *get-parameters*)))
        (collect-ignore
         (render (scan-lists-of-lists-fringe (call-by-symbol action action-symbol))
                 *browser*))))))

(defmethod call-by-url :around ((action action) url)
  (with-db (call-next-method)))

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
