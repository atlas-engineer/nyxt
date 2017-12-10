;;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defclass buffer ()
  ((name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (view :accessor view :initarg :view)
   (modes :accessor modes :initarg :modes)))

(defmethod print-object ((self buffer) stream)
  (format stream "~s" (name self)))

(defmethod add-mode ((self buffer) mode &optional (overwrite nil))
  (let ((found-mode (find mode (modes self)
			  :test #'(lambda (class mode) (typep mode class)))))
    (unless found-mode
      (push mode (modes self)))
    (when (and found-mode overwrite)
      (setf (modes self)
	    (remove-if #'(lambda (item) (typep item 'mode)) (modes self)))
      (push mode (modes self)))))

(defmethod switch-mode ((self buffer) mode-class)
  (let ((found-mode
	 (find mode-class (modes self)
	       :test #'(lambda (class mode) (typep mode class)))))
    (if found-mode
	(setf (mode self) found-mode)
	nil)))

(defmethod add-or-switch-to-mode ((self buffer) mode)
  (add-mode self mode)
  (switch-mode self (class-of mode)))

(defun generate-new-buffer (name mode)
  (let ((new-buffer
	 (make-instance 'buffer
			:name name
			:mode mode
			:modes (list mode)
			:view (mode-view mode))))
    (push new-buffer *buffers*)
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

(defun delete-buffer (buffer)
  (setf *buffers* (delete buffer *buffers*))
  (interface:delete-view (view buffer)))

(defun delete-active-buffer ()
  (when (> (length *buffers*) 1)
    (let ((former-active-buffer *active-buffer*))
      ;; switch-buffer-next changes value of *active-buffer*
      ;; which in turn changes the value of former-active-buffer
      (switch-buffer-next)
      ;; therefore delete actually deletes the new *active-buffer*
      (setf *buffers* (delete former-active-buffer *buffers*))
      (interface:delete-view (view former-active-buffer)))))
