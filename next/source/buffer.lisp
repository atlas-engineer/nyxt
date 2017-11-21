;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defclass buffer ()
  ((name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (view :accessor view :initarg :view)))

(defun generate-new-buffer (name mode)
  (let ((new-buffer
	 (make-instance 'buffer
			:name name
			:mode mode
			:view (mode-view mode))))
    (push new-buffer *buffers*)
    new-buffer))

(defun set-active-buffer (buffer)
  (setf *active-buffer* buffer))

(defun set-visible-active-buffer (buffer)
  (set-active-buffer buffer)
  (interface:set-visible-view (view *active-buffer*)))

(defun switch-buffer (input)
  (let ((buffer (find-if #'(lambda (element) (equalp input (name element))) *buffers*)))
    (set-visible-active-buffer buffer)))

(defun buffer-complete (input)
  (fuzzy-match input (mapcar #'name *buffers*)))

(defun delete-buffer (input)
  (let ((buffer (find-if #'(lambda (element) (equalp input (name element))) *buffers*)))
    (interface:delete-view (view buffer))
    (delete buffer *buffers*)))
