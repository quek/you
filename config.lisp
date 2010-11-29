(in-package :you)

(defclass environment ()
  ())

(defclass production (environment)
  ())

(defclass development (environment)
  ())

(defclass test (environment)
  ())

(defvar *env* (make-instance 'development))

(defparameter *url-prefix* "/you")

(defparameter  *port* 8888)
