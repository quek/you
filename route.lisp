(in-package :you)

(export '(path-for))

(defvar *routes* nil)

(defstruct route
  symbol
  apply-function
  path-function)

(defun add-route (symbol apply-function path-function)
  (setf *routes* (delete symbol *routes* :key #'route-symbol))
  (setf *routes* (append *routes* (list (make-route :symbol symbol
                                                    :apply-function apply-function
                                                    :path-function path-function)))))

(defun get-route (url)
  (loop for x in *routes*
        do (multiple-value-bind (symbol bindings) (funcall (route-apply-function x) url)
             (when symbol
               (return (values symbol bindings))))))

(defun path-to-regexp (path)
  (let (bindings)
    (values
      (with-output-to-string (out)
        (iterate ((x (scan (ppcre:split "/" path)))
                  (y (latch (series #'values) :after 1 :post (^ write-string "/" out))))
          (funcall y)
          (if (q:string-start-p x ":")
              (let ((var (subseq x 1)))
                (write-string "([^/]+)" out)
                (push (intern (string-upcase var)) bindings))
              (write-string (ppcre:quote-meta-chars x) out))))
      (nreverse bindings))))

(defun path-to-path-function (path)
  (let (bindings)
    (let ((format (with-output-to-string (out)
                    (iterate ((x (scan (ppcre:split "/" path)))
                              (y (latch (series #'values) :after 1 :post (^ write-string "/" out))))
                      (funcall y)
                      (if (q:string-start-p x ":")
                          (let ((var (subseq x 1)))
                            (write-string "~a" out)
                            (push (intern (string-upcase var)) bindings))
                          (write-string x out))))))
      (setf bindings (nreverse bindings))
      `(lambda ,(when bindings `(&key ,@bindings))
         (format nil ,format ,@bindings)))))

(defun path-for (symbol &rest args)
  (concatenate 'string *url-prefix*
               (apply (route-path-function (find symbol *routes* :key #'route-symbol))
                      args)))

