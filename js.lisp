(in-package :you)

(export '(js))

(defmacro js (&body form)
  `(ps:ps ,@(mapcar #'parse-js form)))

(defun ->p (x)
  (and (symbolp x)
       (string= "->" (symbol-name x))))

(defun parse-js (form)
  (cond ((atom form)
         form)
        ((->p (car form))
         (-> (cddr form) (cadr form)))
        (t
         (mapcar #'parse-js form))))

(defun -> (form acc)
  (cond ((null form)
         acc)
        ((listp (car form))
         (-> (cdr form) (if (->p (caar form))
                              `(,acc ,(parse-js (car form)))
                              `(,acc ,@(parse-js (car form))))))
        (t
         (-> (cdr form) `(ps:@ ,acc ,(car form))))))

#|
(js ((ps:@ ((ps:@ ($ "#f1") attr) "action" "edit") val)))
(js (-> $ ("#f1") attr ("action" "edit") val))
(js (-> $ ("#f1") attr ("action" "edit") val ()))
(js (-> histroy back (-> a b) p ()))
(js (defun f (x) (-> x substr (1 2)))
    (defun b () (f "baha")))

(js (-> a b (-> a (b) b)))
(js (-> a b (a (-> b c))))
(js (-> a (b)))
(js (-> a b (-> c d (-> e f))))

(ps:ps
  ((ps:@ a b) ((ps:@ c d) (ps:@ e f))))
|#
