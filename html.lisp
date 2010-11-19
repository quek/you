(in-package :you)

(export '(html
          html*
          url
          print-to-html
          parameter
          get-parameter
          post-parameter))

(defvar *get-parameters* nil)

(defvar *post-parameters* nil)

(defun %assoc (key alist)
  (assoc key alist :test #'string-equal))

(defun get-parameter (key)
  (cdr (%assoc key *get-parameters*)))

(defun post-parameter (key)
  (cdr (%assoc key *post-parameters*)))

(defun parameter (key)
  (or (get-parameter key) (post-parameter key)))

(defun session-value (key)
  (hunchentoot:session-value key))

(defun (setf get-parameter) (value key)
  (aif (%assoc key *get-parameters*)
       (setf (cdr it) value)
       (push (cons key value) *get-parameters*))
  value)

(defun (setf post-parameter) (value key)
  (aif (%assoc key *post-parameters*)
       (setf (cdr it) value)
       (push (cons key value) *post-parameters*))
  value)

(defun (setf parameter) (value key)
  (setf (get-parameter key) value))

(defsetf session-value (key) (value)
  `(setf (hunchentoot:session-value ,key) ,value))

(defmacro html (&body body)
  `(list ,@(parse-html-form body)))

(defun parse-html-form (form)
  (cond ((endp form)
         nil)
        ((and (consp (car form)) (keywordp (caar form)))
         (multiple-value-bind (tag-class tag-name attributes body)
             (parse-keyword-form (caar form) (cdar form))
             (cons `(make-instance
                     ',(or tag-class 'tag)
                     :name ,tag-name
                     :attributes (make-attributes (list ,@attributes))
                     :body (html ,@body))
                   (parse-html-form (cdr form)))))
        (t
         (cons (car form) (parse-html-form (cdr form))))))

(defgeneric parse-keyword-form (keyword form))

(defmethod parse-keyword-form (keyword form)
  (let* ((tag-class (gethash keyword *tags*))
         (tag-name (string-downcase (symbol-name (or tag-class keyword)))))
    (loop for (attr val . rest) on form by #'cddr
          with body = form
          while (keywordp attr)
          collect `(cons ,attr ,val) into attributs
          do (setf body rest)
          finally (return (values tag-class tag-name attributs body)))))


(defgeneric print-to-html (object)
  (:method (object)
    (prin1-to-string object)))

(defmethod print-to-html ((object null))
  "")

(defmethod print-to-html ((object string))
  object)

(defmethod print-to-html ((object symbol))
  (string-downcase (symbol-name object)))


(defun replace-request-parameters (x)
  (cond ((and (symbolp x) (< 1 (length (symbol-name x))))
         (let ((name (symbol-name x)))
           (cond ((symbol-head-p x "@")
                  `(parameter ,#1=(string-downcase (subseq name 1))))
                 ((symbol-head-p x "G@")
                  `(get-parameter ,#2=(string-downcase (subseq name 2))))
                 ((symbol-head-p x "P@")
                  `(post-parameter ,#2#))
                 ((symbol-head-p x "S@")
                  `(session-value ,(intern #2# :keyword)))
                 (t x))))
        ((atom x)
         x)
        (t (cons (replace-request-parameters (car x))
                 (replace-request-parameters (cdr x))))))

(defmacro with-http-parameters (&body body)
  `(progn ,@(replace-request-parameters body)))

#|
(html "hello")
(html (:div))
(html (:div :class :notice))
(html (:div "Hello"))
(html (:div :class :notice "かめです"))
(html (:div :class :notice "かめです") (:div (string-capitalize "hello")))

(html
  (:div :class :abc "Hello " @your-name))

(let ((x "top"))
  (html (:div :class x "トップ")))

(let ((val "まみむめも"))
  (html
    (:form :action "aa.lisp" :method :post
           "名前: "(:input :type :text :name :name :value val) (:br)
           (:input :type :submit :value :送信))))

(html
  (:table
   (loop for i in '(1 2 3)
         do (html (:tr (:td i))))))
(html
  (:table
   (values nil (mapc #'(lambda (x)
                         (html (:tr (:td x))))
                     '(1 2 3)))))

(mapc (lambda (x) (render x *browser*))
      (html
        (:table
         (loop for i in '(1 2 3)
               collect (html (:tr (:td i)))))))

|#
