(require :quek)
(use-package :quek)
(require :cl-cont)
(use-package :cl-cont)

(defun collect-@ (form)
  (remove-duplicates
   (collect
       (choose-if (_ and (symbolp _) (symbol-head-p _ "@"))
                  (scan-lists-of-lists-fringe form)))))

(def-page index-page ()
  (:html (:head (:title "トップページ"))
         (:body
          (:form :action :top-submit
                 (:div "名前" (:input :type :text :name 'name :value @name))
                 (:div "年齢" (:input :type :text :name 'age :value @age))
                 (:div (:input :type :submit :value "送信"))))))

(defmacro index-page (&rest args)
  `(let ((@name ,name-of-args)
         (@age ,age-of-angs))
     (alambda ()
       (setf (current-page) self)
       (html
        (:html (:head (:title "トップページ"))
               (:body
                (:form :action :go-next
                       (:div "名前" (:input :type :text :name @name))
                       (:div "年齢" (:input :type :text :name @age))
                       (:div (:input :type :submit :value "送信")))))))))

(def-action go-next (index-page)
  (funcall (link-page @name @age)))

(def-page link-page ()
  (with-default-html-template (:title "リンク")
      (:a :href :go-next "次へ")))

(def-action go-next (link-page)
  (funcall (last-page @name @age)))

(def-page last-page ()
  (with-default-html-template
      (:div #"Hello @(name)(@age)")))

(defmacro with-default-html-template ((&key (title "-")) &body body)
  `(:html
    (:head
     (:title ,title))
    (:body
     ,@body)))
