(in-package :you.example.watch)

(defmacro with-default-template ((&key (title "WATCH")) &body body)
  `(html (:head (:title ,title))
         (:body ,@body)))

(defaction login ()
  (with-default-template ()
    (:form
     :action 'authenticate
     :method :post
     (error-messages)
     (:table
      (:tr (:td "ログインID")
           (:td (:input :type :text :name :id :value @id)
                (error-messages :key 'id)))
      (:tr (:td "パスワード")
           (:td (:input :type :password :name :password)))
      (:tr (:td :rowspan 2
                (:input :type :submit :value "ログイン")))))))

(defvalidation authenticate (:error-action login)
  (id required :message "ログインIDを入力してください。")
  (password required :message "パスワードを入力してください。"))

(defaction authenticate ()
  (html*
    (do-query (#q(select count(*) as ok
                         from watch_user
                         where email = :email
                         and   password = :password)
                 :email @id
                 :password @password)
      (if (zerop $ok)
          (let ((*error-messages*
                 `((nil . "ログインIDまたはパスワードが間違っています。"))))
            (login))
          (main)))))

(defaction main ()
  (with-default-template ()
    "メイン"))