(in-package :you)

(export '(defvalidation
          required
          int-range
          *error-messages*
          error-messages))

(defvar *error-messages* nil)

(defun error-messages (&key (messages *error-messages*)
                            key)
  (aif (if key
           (remove-if-not (^ eq key _) messages :key #'car)
           messages)
       (html
         (:ul
          (loop for i in it
                collect (html (:li (cdr i))))))))

(defmacro defvalidation (name (&key error-action) &body body)
  (alexandria:with-gensyms (result)
    `(defmethod ,name :around (action)
                (let ((*error-messages* nil))
                  ,@(mapcar (lambda (form)
                              `(let ((,result (apply ',(cadr form)
                                                       (parameter ,(print-to-html (car form)))
                                                       (list ,@(cddr form)))))
                                 (when ,result
                                   (push (cons ',(car form) ,result)
                                         *error-messages*))))
                            body)
                  (if *error-messages*
                      (,error-action action)
                      (call-next-method))))))

(defun required (value &key (message "入力してください。"))
  (if (emptyp value)
      message))

(defun int-range (value &key min max (message "範囲外です。"))
  (unless (emptyp value)
    (let ((num (ignore-errors (parse-integer value))))
      (when (or (null num)
                (not (<= min num max)))
        message))))



