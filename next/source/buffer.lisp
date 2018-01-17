;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defclass buffer ()
  ((name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (view :accessor view :initarg :view)
   (modes :accessor modes :initarg :modes)))

(defmethod print-object ((buffer buffer) stream)
  (format stream "~s" (name buffer)))

(defmethod add-mode ((buffer buffer) mode &optional (overwrite nil))
  (let ((found-mode (gethash (class-name (class-of mode)) (modes buffer))))
    (when (or (not found-mode) (and found-mode overwrite))
      (setf (buffer mode) buffer)
      (setf (gethash (class-name (class-of mode)) (modes buffer)) mode))))

(defmethod switch-mode ((buffer buffer) mode)
  (let ((found-mode (gethash (class-name (class-of mode)) (modes buffer))))
    (when found-mode
      (setf (mode buffer) found-mode))))

(defmethod add-or-switch-to-mode ((buffer buffer) mode)
  (add-mode buffer mode)
  (switch-mode buffer mode))

(defun generate-new-buffer (name mode)
  (let ((new-buffer
	 (make-instance 'buffer
			:name name
			:mode mode
			:modes (make-hash-table :test 'equalp)
			:view (interface:make-web-view))))
    (push new-buffer *buffers*)
    (setup mode new-buffer)
    (setf (gethash (class-name (class-of (mode new-buffer))) (modes new-buffer)) (mode new-buffer))
    new-buffer))

(defun set-active-buffer (buffer)
  (setf *active-buffer* buffer))

(defun set-visible-active-buffer (buffer)
  (set-active-buffer buffer)
  (interface:set-visible-view (view *active-buffer*)))

(defun switch-buffer (buffer)
  (set-visible-active-buffer buffer))

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
  (fuzzy-match input *buffers* #'name))

(defun %delete-buffer (buffer)
  (setf *buffers* (delete buffer *buffers*))
  (interface:delete-view (view buffer)))

(defun delete-active-buffer ()
  (if (> (length *buffers*) 1)
      (let ((former-active-buffer *active-buffer*))
        ;; switch-buffer-next changes value of *active-buffer*
        ;; which in turn changes the value of former-active-buffer
        (switch-buffer-next)
        ;; therefore delete actually deletes the new *active-buffer*
        (%delete-buffer former-active-buffer))
      (set-url *start-page-url*)))

(defun delete-buffer (buffer)
  (if (equalp buffer *active-buffer*)
      (delete-active-buffer)
      (%delete-buffer buffer)))
