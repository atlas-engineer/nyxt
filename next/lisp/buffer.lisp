;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defvar *buffer-list* ()
  "A list of all existing buffers")
(defvar *active-buffer* ()
  "The currently active buffer")

(defstruct buffer
  name
  mode
  view)

(defun generate-new-buffer (name)
  (make-buffer
   :name name
   :mode (document-mode)
   :view (qnew "QWebView")))

(defun set-url (name buffer)
  (qlet ((url (qnew "QUrl(QString)" name)))
    (|setUrl| (buffer-view buffer) url)))

(defun set-major-mode (mode buffer)
  (setf (buffer-mode buffer) mode))
