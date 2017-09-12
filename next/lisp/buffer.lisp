;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defvar *buffers* ()
  "A list of all existing buffers")
(defvar *active-buffer* ()
  "The currently active buffer")

(defstruct buffer
  name
  mode
  view)

(defun generate-new-buffer (name mode &optional (add-to-stack-layout t))
  (let ((new-buffer (make-buffer
		     :name name
		     :mode mode
		     :view (mode-view mode))))
    (push new-buffer *buffers*)
    (if add-to-stack-layout
	(|addWidget| *stack-layout* (buffer-view new-buffer)))
    new-buffer))

(defun set-active-buffer (buffer)
  (setf *active-buffer* buffer))

(defun set-visible-active-buffer (buffer)
  (set-active-buffer buffer)
  (|setCurrentWidget| *stack-layout* (buffer-view *active-buffer*)))

(defun switch-buffer (input)
  (loop for buffer in *buffers* do
       (if (equalp (buffer-name buffer) input)
	   (set-visible-active-buffer buffer))))

(defun switch-buffer-complete (input)
  (fuzzy-match input (mapcar #'buffer-name *buffers*)))

(define-key global-map (kbd "C-b") (:input-complete switch-buffer switch-buffer-complete))
