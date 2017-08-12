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

(defun generate-new-buffer (name mode)
  (make-buffer
   :name name
   :mode mode
   :view (mode-view mode)))

(defun set-major-mode (mode buffer)
  (setf (buffer-mode buffer) mode))

(defun set-active-buffer (buffer)
  (setf *active-buffer* buffer))
