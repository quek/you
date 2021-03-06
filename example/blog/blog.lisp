;;;;-*- coding: utf-8 -*-
(in-package :you.example.blog)

(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class entry (basic-view-mixin)
  ((title :initarg :title :accessor title :type string)
   (content :initarg :content :accessor content :type clsql:text)))

(clsql:def-view-class category (basic-view-mixin)
  ((name :initarg :name :accessor name
         :type string :db-constraints :not-null)))

(clsql:def-view-class entry-categories (basic-view-mixin)
  ((entry-id :initarg :entry-id :accessor entry-id :type integer)
   (category-id :initarg :category-id :accessor category-id :type integer)))

#|
(with-db
  (clsql:update-records-from-instance (make-instance 'entry :title "まみ" :content "めも♪")))

(with-db
  (clsql:query "select * from ENTRY"))

(with-db
  (let ((entry (car (clsql:select 'entry :limit 1 :flatp t))))
    (setf (content entry) "ばーばーよ♪")
    (clsql-sys:update-records-from-instance entry)))
|#

#+ignore
(progn
  (with-db (clsql:drop-view-from-class 'entry))
  (with-db (clsql:drop-view-from-class 'category))
  (with-db (clsql:drop-view-from-class 'entry-categories)))

#+ignore
(progn
  (with-db (clsql:create-view-from-class 'entry))
  (with-db (clsql:create-view-from-class 'category))
  (with-db (clsql:create-view-from-class 'entry-categories)))


(defmacro with-default-template ((&key (title "ブログ")) &body body)
  `(html (:html
           (:head
            (:meta :charset "UTF-8")
            (:title ,title)
            (:script :type "text/javascript"
                     :src "http://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"))
           (:body
            ,@body))))

(defaction index (:route "/")
  (with-default-template ()
    (:h1 "ブログ")
    (:a :href (path-for 'new-entry) "投稿")
    (collect (#M(^ html
                   (:h3 (:a :href (path-for 'entry :id (id _)) #"""#,(title _) <#,(id _)>"""))
                   (:div :class :content (content _))
                   (:div (:a :href (path-for 'delete-entry :id (id _)) "削除")))
                (scan (clsql:select 'entry :flatp t :refresh t :order-by '(([created-at] :desc))))))))

(defaction new-entry (:route "/entry/new")
  (with-default-template ()
    (:h1 "投稿")
    (error-messages)
    (:form :action (path-for 'create-entry) :method :post
           (:div "タイトル" (:text :name :title))
           (:textarea :name :content :rows 5 :cols 40)
           (:submit :value "投稿"))
    (:a :href (path-for 'index) "戻る")))

(defaction create-entry ()
  (clsql:update-records-from-instance
   (make-instance 'entry :title @title :content @content))
  (redirect (path-for 'index)))

(defvalidation create-entry (:error-action new-entry)
  (title required :message "タイトルを入力してください。")
  (content required :message "内容を入力してください。"))

(defun find-entry (id)
  (car (clsql:select 'entry :where [= [id] id] :flatp t)))

(defaction delete-entry (:route "/entry/:id/delete")
  (let ((entry (find-entry @id)))
    (clsql:delete-instance-records entry)
    (redirect (path-for 'index))))

(defaction entry (:route "/entry/:id")
  (let ((entry (find-entry @id)))
    (with-default-template ()
      (:h1 (title entry))
      (:div (content entry))
      (:div (:a :href (path-for 'index) "戻る")))))


;; (defaction todo ()
;;   (with-default-template (:title "TODO リスト")
;;     (:h1 "TODO リスト")
;;     (:form :action 'todo
;;            (:text :name :q)
;;            (:submit :value "しぼりこみ"))
;;     (:table
;;      :border 1
;;      (do-query
;;          ((append #q(select * from todo)
;;                   (when @q #q(where content like :param))
;;                   #q(order by id))
;;           :param (string+ "%" @q "%"))
;;        (html (:tr (:td $id)
;;                   (:td (:a :href (url 'todo-edit :id $id) $content))
;;                   (:td
;;                    (values nil (setf (parameter (string+ :done $id)) $done))
;;                    (:checkbox :name (string+ :done $id) :value "t"))
;;                   (:td $done)))))
;;     (:h2 "登録")
;;     (:form
;;      :action 'todo-add :method :post
;;      (:table
;;       (:tr
;;        (:td "ID")
;;        (:td (:text :name :id)))
;;       (:tr
;;        (:td "内容")
;;        (:td (:text :name :content)))
;;       (:tr
;;        (:td (:submit :value "登録")))))))
;; 
;; (defaction todo-add ()
;;   (execute-sql
;;    #q(insert into todo(id, content, done) values(:id, :content, 'f'))
;;    :id @id :content @content)
;;   (todo))
;; 
;; 
;; (defaction todo-edit ()
;;   (setf s@id @id)
;;   (do-query (#q(select * from todo where id = :id) :id @id)
;;     (with-default-template (:title "更新")
;;       (:div "更新しますよ。")
;;       (:form
;;        :action 'todo-edit-confirm :method :post
;;        (:table
;;         (:tr
;;          (:td "内容")
;;          (:td (:text :name :content :value $content)))
;;         (:tr
;;          (:td :colspan 2
;;               (:submit :value "更新")
;;               (:button :onclick (ps:ps ((@ps history back)))
;;                        "やっぱりやめる"))))))))
;; 
;; (defaction todo-edit-confirm ()
;;   (setf s@content @content)
;;   (with-default-template (:title "確認画面")
;;     (:div "この内容でいいですか？")
;;     (:div "内容 " @content)
;;     (:form :action 'todo-edit-done :method :post
;;            (:submit :value "はい")
;;            "&nbsp;"
;;            (:button :onclick (ps:ps ((ps:@ history back)))
;;                     "いいえ"))))
;; 
;; (defaction todo-edit-done ()
;;   (with-default-template (:title "完了")
;;     (execute-sql #q(update todo set content = :c where id = :i)
;;                  :c s@content :i s@id)
;;     (:div "更新しました")
;;     (:a :href 'todo "一覧へ")))
