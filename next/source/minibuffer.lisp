;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)))

(defmethod input ((self minibuffer-mode) callback &optional completion)
  (with-slots (callback-function completion-function callback-buffer) self
    (setf callback-function callback)
    (setf completion-function completion)
    (setf callback-buffer *active-buffer*)
    (interface:minibuffer-set-completion-function completion))
  (set-active-buffer *minibuffer*)
  (interface:minibuffer-show))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (completion-function callback-function) self
      (if completion-function
      (funcall callback-function
	       (nth 0 (funcall completion-function
			       (interface:minibuffer-get-input))))
      (funcall callback-function
	       (interface:minibuffer-get-input))))
  (interface:minibuffer-hide))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (interface:minibuffer-hide))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap minibuffer-mode-map
		 :view (interface:make-minibuffer)))
