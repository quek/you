;;;;-*- coding: utf-8 -*-
(in-package :you.example.chat)

(defvar *js*
  (hunchentoot:create-folder-dispatcher-and-handler
   "/js/" (merge-pathnames "js/" (directory-namestring  *load-truename*))))

(pushnew *js* hunchentoot:*dispatch-table*)

(defmacro with-default-template ((&key (title "チャット")) &body body)
  `(html (:html
           (:head
            (:script :type "text/javascript" :src "/js/jquery-1.3.2.js")
            (:link :rel "stylesheet" :href "default.css" :type "text/css")
            (:title ,title))
           (:body
            (:div :id :main
                  ,@body)
            (:div :id :footer "Powered by Common Lisp")))))

(defaction default.css ()
  (setf (hunchentoot:content-type*) "text/css")
  (html "
.name {
  font-size: 0.8em;
}
.say-ts {
  float: right;
  align: right;
  color: gray;
  font-size: 0.7em;
}
.content {
  margin-bottom: 0.3em;
}
#list {
  width: 80%;
  height: 10em;
  overflow: auto;
}
#footer {
  text-align: right;
  font-size: 0.6em;
}
"))


(defvar *loop-process* nil)

(defaction index.html ()
  (unless *loop-process*
    (setf *loop-process* (spawn (loop-process nil))))
  (with-default-template ()
    (:h1 "チャット")
    (:div :id :list)
    (:form :onsubmit (ps (return (say)))
           (:div (:textarea :id :content :rows 3 :cols 50))
           (:div (:submit :value "話す")
                 " 名前:" (:text :id :name :value "かめ")))
    (:script
     (ps (defun say ()
           ($.post "say" (create :content ((@ ($ "#content") val))
                                 :name ((@ ($ "#name") val)))
                   (lambda (data)
                     ((@
                       ((@ ($ "#content") val) "")
                       focus))))
           (return false))
         (defun refresh ()
           (let ((id ((@ ($ "#list div.entry:last") attr) "id")))
             ($.post "refresh" (create :id (if id id ""))
                     (lambda (data)
                       (if (!= data "")
                           ((@ ((@ ($ "#list") append) data)
                               scroll-top) 99999))
                       (set-timeout refresh 100)))))
         ((@ ($ "#content") keyup) (lambda (e)
                                     (if (= e.key-code 13)
                                         (say))))
         ($(refresh))))))

(defun generate-id ()
  (format nil "~s~a" (get-universal-time) (gensym)))

(defun loop-process (threads)
  (receive ()
    (:say (mapc (lambda (x)
                  (send x t))
                threads)
          (loop-process nil))
    (x (loop-process (cons x threads)))))

(defaction say ()
  (unless (or (q:emptyp @name) (q:emptyp @content))
    (execute-sql #q(insert into chat(id, name, content, say_ts)
                           values(:id, :name, :content, current_timestamp))
                 :id (generate-id)
                 :name @name
                 :content @content))
  (send *loop-process* :say))

(defaction refresh ()
  (do-query (#q(select count(*) as count from chat where id > :id)
               :id @id)
    (when (zerop $count)
      (send *loop-process* *current-thread*)
      (receive (:timeout 30)
        (_))))
  (html
    (do-query (#q(select * from chat where id > :id order by say_ts)
                 :id @id)
      (html (:div :id $id :class :entry
                  (:div :class :say-ts $say_ts)
                  (:div :class :name $name)
                  (:div :class :content $content))))))

#|
(with-db (execute-sql #q(delete from chat)))
|#