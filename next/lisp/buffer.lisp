;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defvar *buffers* ()
  "A list of all existing buffers")
(defvar *active-buffer* ()
  "The currently active buffer")

(defclass buffer ()
  ((name :accessor buffer-name :initarg :name)
   (mode :accessor buffer-mode :initarg :mode)
   (view :accessor buffer-view :initarg :view)))

(defun generate-new-buffer (name mode &optional (add-to-stack-layout t))
  (let ((new-buffer
	 (make-instance 'buffer
			:name name
			:mode mode
			:view (mode-view mode))))
    (push new-buffer *buffers*)
    (when add-to-stack-layout
      (|addWidget| *stack-layout* (buffer-view new-buffer)))
    new-buffer))

(defun set-active-buffer (buffer)
  (setf *active-buffer* buffer))

(defun set-visible-active-buffer (buffer)
  (set-active-buffer buffer)
  (|setCurrentWidget| *stack-layout* (buffer-view *active-buffer*)))

(defun switch-buffer (input)
  (let ((buffer (find-if #'(lambda (element) (equalp input (buffer-name element))) *buffers*)))
    (set-visible-active-buffer buffer)))

(defun buffer-complete (input)
  (fuzzy-match input (mapcar #'buffer-name *buffers*)))

(defun delete-buffer (input)
  (let ((buffer (find-if #'(lambda (element) (equalp input (buffer-name element))) *buffers*)))
    (qdelete (buffer-view buffer))
    (delete buffer *buffers*)))

(define-key global-map (kbd "C-x b") (:input-complete switch-buffer buffer-complete))
(define-key global-map (kbd "C-x k") (:input-complete delete-buffer buffer-complete))
