;;; buffer.lisp --- lisp subroutines for creating / managing buffers

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

(defun buffer-complete (input)
  (fuzzy-match input *buffers* #'name))

(defun get-active-buffer-index ()
  (position *active-buffer* *buffers* :test #'equalp))

(defun %delete-buffer (buffer)
  (setf *buffers* (delete buffer *buffers*))
  (interface:delete-view (view buffer)))

(defcommand switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        (mode *minibuffer*)
                        :completion 'buffer-complete))
    (set-visible-active-buffer buffer)))

(defcommand make-visible-new-buffer ()
  "Make a new empty buffer with the *default-new-buffer-url* loaded"
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (set-visible-active-buffer new-buffer)
    (set-url *default-new-buffer-url*)))

(defcommand switch-buffer-previous ()
  "Switch to the previous buffer in the list of *buffers*, if the
first item in the list, jump to the last item."
  (let ((active-buffer-index (position *active-buffer* *buffers* :test #'equalp)))
    (if (equalp 0 active-buffer-index)
	(set-visible-active-buffer (nth (- (length *buffers*) 1) *buffers*))
	(set-visible-active-buffer (nth (- active-buffer-index 1) *buffers*)))))

(defcommand switch-buffer-next ()
  "Switch to the next buffer in the list of *buffers*, if the last
item in the list, jump to the first item."
  (let ((active-buffer-index (position *active-buffer* *buffers* :test #'equalp)))
    (if (< (+ active-buffer-index 1) (length *buffers*))
        (set-visible-active-buffer (nth (+ active-buffer-index 1) *buffers*))
        (set-visible-active-buffer (nth 0 *buffers*)))))

(defcommand delete-active-buffer ()
  "Delete the currently active buffer, and make the next buffer
*buffers* the visible buffer. If no other buffers exist, set the url
of the current buffer to the start page."
  (if (> (length *buffers*) 1)
      (let ((former-active-buffer *active-buffer*))
        ;; switch-buffer-next changes value of *active-buffer*
        ;; which in turn changes the value of former-active-buffer
        (switch-buffer-next)
        ;; therefore delete actually deletes the new *active-buffer*
        (%delete-buffer former-active-buffer))
      (set-url *start-page-url*)))

(defcommand delete-buffer ()
  "Delete the buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        (mode *minibuffer*)
                        :completion 'buffer-complete))
    (if (equalp buffer *active-buffer*)
        (delete-active-buffer)
        (%delete-buffer buffer))))
