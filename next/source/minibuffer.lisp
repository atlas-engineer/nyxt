;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((minibuffer-completion-function :accessor minibuffer-completion-function)
   (minibuffer-callback-function :accessor minibuffer-callback-function)
   (minibuffer-callback-buffer :accessor minibuffer-callback-buffer)))

(defmethod input ((self minibuffer-mode) callback-function &optional completion-function)
  (with-slots (minibuffer-callback-function minibuffer-completion-function minibuffer-callback-buffer) self
    (setf minibuffer-callback-function callback-function)
    (setf minibuffer-completion-function completion-function)
    (setf minibuffer-callback-buffer *active-buffer*)
    (interface:minibuffer-set-completion-function completion-function))
  (set-active-buffer *minibuffer*)
  (interface:minibuffer-show))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (minibuffer-callback-buffer self))
  (with-slots (minibuffer-completion-function minibuffer-callback-function) self
      (if minibuffer-completion-function
      (funcall minibuffer-callback-function
	       (nth 0 (funcall minibuffer-completion-function
			       (interface:minibuffer-get-input))))
      (funcall minibuffer-callback-function
	       (interface:minibuffer-get-input))))
  (interface:minibuffer-hide))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (minibuffer-callback-buffer self))
  (interface:minibuffer-hide))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap minibuffer-mode-map
		 :view (interface:make-minibuffer)))
