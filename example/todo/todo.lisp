;;;;-*- coding: utf-8 -*-
(in-package :you.example.todo)

(defmacro with-default-template ((&key (title "TODO")) &body body)
  `(html (:html
          (:head (:title ,title))
          (:body ,@body))))

(defaction todo ()
  (with-default-template (:title "TODO リスト")
    (:h1 "TODO リスト")
    (:form :action 'todo
           (:text :name :q)
           (:submit :value "しぼりこみ"))
    (:table
     :border 1
     (do-query
         ((append #q(select * from todo)
                  (when @q #q(where content like :param))
                  #q(order by id))
          :param (string+ "%" @q "%"))
       (html (:tr (:td $id)
                  (:td (:a :href (url 'todo-edit :id $id) $content))
                  (:td
                   (values nil (setf (parameter (string+ :done $id)) $done))
                   (:checkbox :name (string+ :done $id) :value "t"))
                  (:td $done)))))
    (:h2 "登録")
    (:form
     :action 'todo-add :method :post
     (:table
      (:tr
       (:td "ID")
       (:td (:text :name :id)))
      (:tr
       (:td "内容")
       (:td (:text :name :content)))
      (:tr
       (:td (:submit :value "登録")))))))

(defaction todo-add ()
  (execute-sql
   #q(insert into todo(id, content, done) values(:id, :content, 'f'))
   :id @id :content @content)
  (todo))


(defaction todo-edit ()
  (setf s@id @id)
  (do-query (#q(select * from todo where id = :id) :id @id)
    (with-default-template (:title "更新")
      (:div "更新しますよ。")
      (:form
       :action 'todo-edit-confirm :method :post
       (:table
        (:tr
         (:td "内容")
         (:td (:text :name :content :value $content)))
        (:tr
         (:td :colspan 2
              (:submit :value "更新")
              (:button :onclick (ps:ps ((@ps history back)))
                       "やっぱりやめる"))))))))

(defaction todo-edit-confirm ()
  (setf s@content @content)
  (with-default-template (:title "確認画面")
    (:div "この内容でいいですか？")
    (:div "内容 " @content)
    (:form :action 'todo-edit-done :method :post
           (:submit :value "はい")
           "&nbsp;"
           (:button :onclick (ps:ps ((ps:@ history back)))
                    "いいえ"))))

(defaction todo-edit-done ()
  (with-default-template (:title "完了")
    (execute-sql #q(update todo set content = :c where id = :i)
                 :c s@content :i s@id)
    (:div "更新しました")
    (:a :href 'todo "一覧へ")))
