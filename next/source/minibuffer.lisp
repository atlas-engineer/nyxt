;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (cleanup-function :accessor cleanup-function)))

(defmethod input ((self minibuffer-mode) callback &key completion cleanup)
  (with-slots (callback-function completion-function callback-buffer cleanup-function) self
    (setf callback-function callback)
    (setf completion-function completion)
    (setf cleanup-function cleanup)
    (setf callback-buffer *active-buffer*)
    (interface:minibuffer-set-completion-function completion))
  (set-active-buffer *minibuffer*)
  (interface:minibuffer-show))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (completion-function callback-function cleanup-function) self
      (if completion-function
      (funcall callback-function
	       (nth 0 (funcall completion-function
			       (interface:minibuffer-get-input))))
      (funcall callback-function
	       (interface:minibuffer-get-input)))
      (when cleanup-function
	(funcall cleanup-function)))
  (interface:minibuffer-hide))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (cleanup-function) self
    (when cleanup-function
      (funcall cleanup-function)))
  (interface:minibuffer-hide))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap minibuffer-mode-map
		 :view (interface:make-minibuffer)))
