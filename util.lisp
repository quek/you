(in-package :you)

(defun princ* (&rest args)
  (mapcan #'princ args))
