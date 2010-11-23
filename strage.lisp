(in-package :you)

(export '(with-ruck))

(defvar *rucksack-dir* "/tmp/rucksack/")

(defmacro ruck (&body body)
  `(rucksack:with-rucksack (rucksack:*rucksack* *rucksack-dir*)
     (rucksack:with-transaction ()
       ,@body)))

(defmacro with-ruck (&body body)
  `(ruck ,@body))
