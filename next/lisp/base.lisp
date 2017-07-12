(defpackage :next
  (:use :common-lisp :eql)
  (:export
   #:start))
(in-package :next)
(qrequire :webkit)
(load "qt")
(load "keymap")

;; load the user's init file
(load "~/.next.d/init")

(defvar *web-view* (qnew "QWebView"))

(defun set-url (name)
  (qlet ((url (qnew "QUrl(QString)" name)))
    (|setUrl| *web-view* url)))

(defun start ()
  (|show| *web-view*)
  (set-url "about:blank"))

(start)
