(defpackage :example
  (:use :common-lisp :eql)
  (:export
   #:start))

(in-package :example)

(qrequire :webkit)

(defvar *web-view* (qnew "QWebView"))

(defun set-url (name)
  (qlet ((url (qnew "QUrl(QString)" name)))
    (|setUrl| *web-view* url)))

(defun start ()
  (|show| *web-view*)
  (set-url "http://www.google.com/"))
