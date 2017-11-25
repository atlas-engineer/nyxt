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

(defun get-active-buffer-index ()
  (position *active-buffer* *buffers* :test #'equalp))

(defun switch-buffer-previous ()
  (let ((active-buffer-index (position *active-buffer* *buffers* :test #'equalp)))
    (if (equalp 0 active-buffer-index)
	(set-visible-active-buffer (nth (- (length *buffers*) 1) *buffers*))
	(set-visible-active-buffer (nth (- active-buffer-index 1) *buffers*)))))

(defun switch-buffer-next ()
  (let ((active-buffer-index (position *active-buffer* *buffers* :test #'equalp)))
    (if (< (+ active-buffer-index 1) (length *buffers*))
	(set-visible-active-buffer (nth (+ active-buffer-index 1) *buffers*))
	(set-visible-active-buffer (nth 0 *buffers*)))))

(defun buffer-complete (input)
  (fuzzy-match input (mapcar #'name *buffers*)))

(defun delete-buffer (input)
  (let ((buffer (find-if #'(lambda (element) (equalp input (name element))) *buffers*)))
    (interface:delete-view (view buffer))
    (delete buffer *buffers*)))

(defun delete-active-buffer ()
  (when (> (length *buffers*) 1)
    (let ((former-active-buffer *active-buffer*))
      ;; switch-buffer-next changes value of *active-buffer*
      ;; which in turn changes the value of former-active-buffer
      (switch-buffer-next)
      ;; therefore delete actually deletes the new *active-buffer*
      (setf *buffers* (delete former-active-buffer *buffers*))
      (interface:delete-view (view former-active-buffer)))))
