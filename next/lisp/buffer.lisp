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

(defun generate-new-buffer (name &optional (mode (document-mode)))
  (make-buffer
   :name name
   :mode mode
   :view (qnew "QWebView")))

(defun set-major-mode (mode buffer)
  (setf (buffer-mode buffer) mode))

(defun set-active-buffer (buffer)
  (|removeWidget| *layout* (buffer-view *active-buffer*))
  (setf *active-buffer* buffer)
  (|addWidget| *layout* (buffer-view *active-buffer*)))
