(in-package :you)

(defclass name-mixin ()
  ((name :initarg :name :initform nil :accessor name-of)))

(defclass value-mixin ()
  ((value :initarg :value :initform nil :accessor value-of)))

(defclass attribute (name-mixin value-mixin) ())

(defclass attributes (value-mixin) ())

(defclass tag (name-mixin)
  ((attributes :initarg :attributes
               :initform (make-instance 'attributes)
               :accessor attributes-of)
   (body :initarg :body :initform () :accessor body-of)))

(defun make-attribute (attribute)
  (make-instance 'attribute
                 :name (car attribute)
                 :value (cdr attribute)))

(defmethod add-attribute ((self attributes) attr.value)
  (push (make-attribute attr.value) (value-of self)))

(defun make-attributes (attributes)
  (make-instance 'attributes
                 :value (loop for i in attributes
                              collect (make-attribute i))))
(defmethod add-attribute ((self tag) attr.value)
  (add-attribute (attributes-of self) attr.value))

(defmethod find-attribute ((self tag) attribute-name)
  (find attribute-name
        (value-of (attributes-of self))
        :key #'name-of))

(defmethod print-object ((object attribute) stream)
  (with-accessors ((name name-of) (value value-of)) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a=\"~a\""
              (print-to-html name)
              (print-to-html value)))))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (name-of object))))

(defmethod parameter-value ((tag tag))
  (awhen (find-attribute tag :name)
    (parameter (print-to-html (value-of it)))))

(defvar *tags* (make-hash-table))

(defmacro deftag (name &rest body)
  `(progn
     (setf (gethash ,(intern (symbol-name name) :keyword) *tags*) ',name)
     (defclass ,name ,@body)))

(deftag submit (tag)
  ())

(defmethod initialize-instance :after ((self submit) &rest args)
  (declare (ignore args))
  (setf (name-of self) "input")
  (add-attribute self '(:type . :submit)))

(deftag text (tag)
  ())

(defmethod initialize-instance :after ((self text) &rest args)
  (declare (ignore args))
  (with-accessors ((name name-of) (attributes attributes-of)) self
    (setf name "input")
    (add-attribute self '(:type . :text))
    (let ((name (find-attribute self :name))
          (value (find-attribute self :value)))
      (awhen (aand name (null value) (parameter-value self))
        (add-attribute self (cons :value it))))))

(deftag checkbox (tag) ())

(defmethod initialize-instance :after ((self checkbox) &rest args)
  (declare (ignore args))
  (with-accessors ((name name-of) (attributes attributes-of)) self
    (setf name "input")
    (add-attribute self '(:type . :checkbox))
    (let ((name (find-attribute self :name))
          (value (aif (find-attribute self :value)
                      (value-of it))))
      (when (and name
                 (equal value (parameter-value self)))
        (add-attribute self (cons :checked :true))))))

(let ((*get-parameters* '(("done" . "t") ("q" . ""))))
  (parameter :name))
(defclass browser ()
  ((name :accessor name-of)
   (doctype :initform "" :accessor doctype-of)))

(defclass html-browser (browser)
  ())

(defclass html-4.01-strict-browser (html-browser)
  ()
  (:default-initargs
      :doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"))

(defclass xhtml-browser (browser)
  ())

(defclass chtml-browser (browser)
  ())

(defgeneric render (tag browser)
  (:method ((object null) (browser browser))
    nil)
;;  (:method ((object list) (browser browser))
;;    (mapc (lambda (x) (render x browser)) object))
  (:method (object (browser browser))
    (princ object *response-stream*)))

(defmethod render ((self attribute) browser)
  (format *response-stream* " ~a=\"~a\""
          (print-to-html (name-of self))
          (print-to-html (value-of self))))

(defmethod render ((self attributes) browser)
  (loop for i in (value-of self)
        do (render i browser)))

(defmethod render ((tag tag) (browser html-browser))
  (with-accessors ((name name-of) (attributes attributes-of) (body body-of))
      tag
    (format *response-stream* "<~a" name)
    (render attributes browser)
    (format *response-stream* "~%>")
    (when body
      (iterate ((x (scan-lists-of-lists-fringe body)))
               (render x browser)))
    (format *response-stream* "</~a~&>" name)))

(defmethod render ((tag tag) (browser xhtml-browser))
  (with-accessors ((name name-of) (attributes attributes-of) (body body-of))
      tag
    (format *response-stream* "<~a" name)
    (render attributes browser)
    (if body
        (progn
          (iterate ((x (scan-lists-of-lists-fringe body)))
                   (render x browser))
          (format *response-stream* "</~a>" name))
        (format *response-stream* " />"))))

(defvar *browser* (make-instance 'html-browser))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url (path &rest query-parameters)
  (with-output-to-string (*standard-output*)
    (write-string (print-to-html path))
    (loop for (a b) on query-parameters by #'cddr
            initially (write-string "?")
          do (format *standard-output* "~a=~a"
                                       (print-to-html a)
                                       (print-to-html b)))))
