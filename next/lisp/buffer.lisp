;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defvar *buffer-list* ()
  "A list of all existing buffers")

(defstruct buffer
  name
  web-view)

(defun generate-new-buffer (name)
  (make-buffer
   :name name
   :web-view (qnew "QWebView")))

(defun set-url (name buffer)
  (qlet ((url (qnew "QUrl(QString)" name)))
    (|setUrl| (buffer-web-view buffer) url)))
