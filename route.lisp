(in-package :you)

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

(defaction posts-show (GET "/posts:id")
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
