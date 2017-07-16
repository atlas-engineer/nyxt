;;; base.lisp --- main entry point into nEXT

(defpackage :next
  (:use :common-lisp :eql)
  (:export
   #:start))
(in-package :next)

(qrequire :webkit)

(defvar *web-view* (qnew "QWebView"))

(defun set-url (name)
  (qlet ((url (qnew "QUrl(QString)" name)))
    (|setUrl| *web-view* url)))

(defun start ()
  (|show| *web-view*)
  (set-url "about:blank"))
