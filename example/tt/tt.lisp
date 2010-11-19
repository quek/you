;;;;-*- coding: utf-8 -*-
(in-package :you.example.tt)

(setq you::*rucksack-dir*
      (merge-pathnames "rucksack/" (directory-namestring *load-truename*)))

(defvar *js-dispatcher* (hunchentoot:create-folder-dispatcher-and-handler
                         "/js/" (merge-pathnames
                                 "js/" (directory-namestring *load-truename*))))
(pushnew *js-dispatcher* hunchentoot:*dispatch-table*)

(with-db
  (defclass place ()
    ((name :initarg :name :accessor name-of :index :string-index :unique t)
     (times :initarg :times :initform () :accessor times-of))
    (:index t)
    (:metaclass rucksack:persistent-class))
  (defclass direction ()
    ((direction :initarg :direction :accessor direction-of :index :string-index))
    (:index t)
    (:metaclass rucksack:persistent-class))
  (defclass time-entry ()
    ((place :initarg :place :initform "unknown" :accessor place-of)
     (direction :initarg :direction :initform "unknown" :accessor direction-of)
     (time :initarg :time :initform (dt:make-time 0 0 0) :accessor time-of))
    (:index t)
    (:metaclass rucksack:persistent-class))
  )

(defmethod add-time ((place place) (time time-entry))
  (with-slots (times) place
    (setf times (append times (list time))))
  (setf (place-of time) place)
  place)

(defmethod print-object ((place place) stream)
  (print-unreadable-object (place stream :type t)
    (format stream "~a has ~d times" (name-of place) (length (times-of place)))))

(defun get-place-by-name (name)
  (rucksack:rucksack-do-slot (x 'place 'name :equal name)
    (return-from get-place-by-name x)))

(defmethod print-object ((object time-entry) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a ~a ~a"
            (place-of object)
            (direction-of object)
            (dt:|hh:mm| (time-of object)))))

(defmethod to-table-row ((self time-entry))
  (html (:tr (:td (name-of (place-of self)))
             (:td (direction-of self))
             (:td (dt:|hh:mm| (time-of self))))))

#+nil
(progn
  (with-db
    (loop for ((name data)) on
          '(("ちょこぼの森" (8 23 9 12 10 43 13 01 15 00 17 30))
            ("かっぱ沼" (10 11 11 1 12 45 17 32 18 9 20 55 23 45 23 55))
            ("きのこ山" (09 11 10 11 12 33 14 21 15 01)))
          do
       (loop with place = (make-instance 'place :name name)
             for (h m) on data by #'cddr
             do (add-time place
                          (make-instance 'time-entry
                               :direction "上り"
                               :time (dt:make-time h m 0))))))
  (with-db
    (rucksack:rucksack-do-class (x 'place) (print x))
    (rucksack:rucksack-map-class rucksack:*rucksack* 'time-entry #'print))
  )

(defmacro with-default-template ((&key (title "TT")) &body body)
  `(html (:html
           (:head (:title ,title)
                  (:script :type "text/javascript" :src "/js/jquery-1.3.2.js")
                  (:script :type "text/javascript"
                           :src "/js/jquery-autocomplete/jquery.autocomplete.js")
                  (:script :type "text/javascript" :src "tt.js")
                  (:link :rel "stylesheet" :type "text/css"
                         :href "/js/jquery-autocomplete/jquery.autocomplete.css"))
           (:body
            (:div :id :main ,@body)))))

(defaction tt.js ()
  (html (js ($ (lambda ()
                 (-> $ ("#place") focus () autocomplete ("complete-place")))))))

(defaction top ()
  (with-default-template (:title "TT")
    (:h1 "リスト")
    (error-messages)
    (:form :action 'top :method :post :id :f1
           "場所 " (:text :name :place :id :place)
           " " (:submit :value "表示")
           " " (:submit :value "編集" :onclicu (js (-> $ ("#f1") attr ("action" "edit"))))
           (awhen (get-place-by-name @place)
             (html (:table
                    (:tr (:th '場所) (:th '方向) (:th '時間))
                    (loop with current = (dt:now)
                          for _x in (times-of it)
                          for x = (rucksack::maybe-dereference-proxy _x)
                          if (dt:time< current (time-of x))
                            collect (to-table-row x))))))
    (:a :href 'add "登録")))

(defaction complete-place ()
  (html (with-output-to-string (out)
          (rucksack:rucksack-do-slot (place 'place 'name)
            (when (search @q (name-of place))
              (format out "~a~%" (name-of place)))))))

(defaction add ()
  (with-default-template (:title "登録")
    (:form :action 'do-add
           (:div "場所 " (:text :name :place))
           (:div "方向 " (:text :name :direction))
           (:div "時刻 " (:text :name :hour :size 2) ":" (:text :name :minute :size 2))
           (:div (:submit :value "登録")))))

(defaction do-add ()
  (make-instance 'time-entry
                 :place @place
                 :direction @direction
                 :time (dt:make-time (parse-integer @hour)
                                     (parse-integer @minute)
                                     0))
  (redirect 'top))

(defvalidation edit (:error-action top)
  (place required :message "場所を入力してください。"))

(defaction edit ()
  (aif (get-place-by-name @place)
       (with-default-template (:title "編集")
         (:form :action 'top
                (:submit :value "編集")))
       (let ((*error-messages* '((place . "該当データがありません。"))))
         (top))))
