(in-package :you)

(defvar *routes* nil)

(defun add-route (symbol form)
  (setf *routes* (delete symbol *routes* :key #'car))
  (setf *routes* (acons symbol form *routes*)))

(defun get-route (url)
  (loop for x in *routes*
      do (multiple-value-bind (symbol bindings) (funcall (cdr x) url)
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
;; (path-to-regexp "a/:a-id/b/:id")
;; => "a/([^/]+)/b/([^/]+)"
;;    (A-ID ID)






#|
(defaction entry (:route "entry/:id")
  )


(defaction posts (GET "/posts")
  (with-defalut-template ()
    (loop for i in (query "select * from posts order by created_at") collect
      (with-result-set i
        (html (:div.post $name)
              (:div.post $content)
              (mapcar (lambda (x)
                        (with-result-set x
                          (html (:div.comment $name)
                                (:div.comment $content))))
                      (query "select name, content from comments where post_id = 1 /* id */
                                          order by created_at"
                             :id $id)))))))

(let ((id x))
  #q(select * from xxx weher id = 1/*id*/))

(defclass posts (standard-action)
  ((@posts)))
(defmethod action ((self posts))
  (with-slots (@posts) self
    (setf @posts (list-posts))))
(defmethod view ((self posts))
  (with-slots (@posts) self
    (loop for i in @posts
          collect (html (:div.post (list-view i))))))

(defaction posts-show (GET "/posts/:id")
  (post id))

(defclass posts-show (standard-action)
  ((id :initarg :id)))

(defaction posts-edit (GET "/posts/:id/edit")
  (post id))

(defaction posts-update (PUT "/posts/:id")
  (update-post id params))

(defaction posts-destroy (DELETE "/posts/:id")
  (delete-post :id)
  (redirect (posts-path)))
|#
