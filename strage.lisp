(in-package :you)

(export '(with-db))

(defvar *rucksack-dir* "/tmp/rucksack/")

(defmacro ruck (&body body)
  `(rucksack:with-rucksack (rucksack:*rucksack* *rucksack-dir*)
     (rucksack:with-transaction ()
       ,@body)))

(defmacro with-db (&body body)
  `(ruck ,@body))
