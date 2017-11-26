;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function)
   ;; empty-complete-immediate: if no completion candidates, return-immediate
   (empty-complete-immediate :accessor empty-complete-immediate)))

(defmethod input ((self minibuffer-mode) callback
		  &key completion (setup #'erase-input) cleanup empty-complete)
  (with-slots (callback-function completion-function callback-buffer
               setup-function cleanup-function empty-complete-immediate)
      self
    (setf callback-function callback)
    (setf completion-function completion)
    (setf setup-function setup)
    (setf cleanup-function cleanup)
    (setf empty-complete-immediate empty-complete)
    (setf callback-buffer *active-buffer*)
    (interface:minibuffer-set-completion-function completion)
    ;; setup function must be called before *active-buffer* is changed
    ;; to mini-buffer so that setup function may act upon *active-buffer*
    (funcall setup-function))
  (set-active-buffer *minibuffer*)
  (interface:minibuffer-show))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completion-function)
      self
    (if completion-function
	;; if there's a completion function
	(progn
	  (let ((completion (interface:minibuffer-get-input-complete)))
	    (if completion
		;; if we're able to find a completion
		(funcall callback-function completion)
		;; if we can't find a completion
		(when empty-complete-immediate
		  ;; if we accept immediate output in place of completion
		  (return-immediate self)))))
	;; if there's no completion function
	(return-immediate self))
    (when cleanup-function
      (funcall cleanup-function)))
  (interface:minibuffer-hide))

(defmethod return-immediate ((self minibuffer-mode))
  "Return without completion"
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function) self
    (funcall callback-function
	     (interface:minibuffer-get-input))
    (when cleanup-function
      (funcall cleanup-function)))
  (interface:minibuffer-hide))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (cleanup-function) self
    (when cleanup-function
      (funcall cleanup-function)))
  (interface:minibuffer-hide))

(defmethod set-input ((self minibuffer-mode) input)
  (interface:minibuffer-set-input input))

(defun erase-input ()
  (interface:minibuffer-set-input ""))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap minibuffer-mode-map
		 :view (interface:make-minibuffer)))
