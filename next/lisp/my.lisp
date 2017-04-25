(defpackage :example
  (:use :common-lisp :eql)
  (:export
   #:start))

(in-package :example)

(defun start ()
  (! "show" (qnew "QLabel"
                  "text" (tr (format nil "Charlie, you're the banana king"))
                  "alignment" |Qt.AlignCenter|
                  "margin" 10)))
