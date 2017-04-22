(defpackage :example
  (:use :common-lisp :eql)
  (:export
   #:start))

(in-package :example)

(defparameter *alarm* 11)

(defun start ()
  (! "show" (qnew "QLabel"
                  "text" (tr (format nil "<h3>~A ~{~R~^ ~},<br>I want to sleep until ~R."
                                     (string-capitalize (format nil "~R" 1))
                                     (loop for i from 2 to 7 collect i)
                                     *alarm*))
                  "alignment" |Qt.AlignCenter|
                  "margin" 10)))
