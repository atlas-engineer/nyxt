;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode) ())


(defun input (callback-function &optional completion-function)
  (setf *minibuffer-callback* callback-function)
  (setf *minibuffer-completion-function* completion-function)
  (interface:minibuffer-set-completion-function completion-function)
  (setf *minibuffer-callback-buffer* *active-buffer*)
  (set-active-buffer *minibuffer*)
  (interface:minibuffer-show))

(defun return-input ()
  (set-active-buffer *minibuffer-callback-buffer*)
  (if *minibuffer-completion-function*
      (funcall *minibuffer-callback*
	       (nth 0 (funcall *minibuffer-completion-function*
			       (interface:minibuffer-get-input))))
      (funcall *minibuffer-callback*
	       (interface:minibuffer-get-input)))
  (interface:minibuffer-hide))

(defun cancel-input ()
  (set-active-buffer *minibuffer-callback-buffer*)
  (interface:minibuffer-hide))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap minibuffer-mode-map
		 :view (interface:make-minibuffer)))
