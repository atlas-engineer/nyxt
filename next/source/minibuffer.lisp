;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar *minibuffer-mode-map* (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function)
   (empty-complete-immediate :accessor empty-complete-immediate)))

(defmethod read-from-minibuffer (callback (self minibuffer-mode)
                                 &key completion setup cleanup empty-complete)
  (with-slots (callback-function completion-function callback-buffer
               setup-function cleanup-function empty-complete-immediate)
      self
    (setf callback-function callback)
    (setf completion-function completion)
    (setf setup-function setup)
    (setf cleanup-function cleanup)
    (setf empty-complete-immediate empty-complete)
    (setf callback-buffer *active-buffer*)
    (erase-input self)
    (funcall setup-function))
  (set-active-buffer *minibuffer*)
  (show self))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completion-function)
      self
    (if completion-function
	;; if there's a completion function
	(progn
	  (let ((completion (get-input-complete (mode *minibuffer*))))
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
  (hide *interface*))

(defmethod return-immediate ((self minibuffer-mode))
  "Return without completion"
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function) self
    (funcall callback-function) ;; add value
    (when cleanup-function
      (funcall cleanup-function)))
  (hide *interface*))

(defmethod get-input-complete ((self minibuffer-mode)))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (cleanup-function) self
    (when cleanup-function
      (funcall cleanup-function)))
  (hide self))

(defmethod set-input ((self minibuffer-mode) input)
  (minibuffer-execute-javascript
   *interface* "0"
   (concatenate 'string "document.write('" input "');")))

(defmethod erase-input ((self minibuffer-mode))
  (minibuffer-execute-javascript
   *interface* "0"
   "document.open();document.close();"))

(defmethod show ((self minibuffer-mode))
  (minibuffer-set-height *interface* "0" 100))

(defmethod hide ((self minibuffer-mode))
  (minibuffer-set-height *interface* "0" 10))

(defmethod select-next ((self minibuffer-mode)))

(defmethod select-previous ((self minibuffer-mode)))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap *minibuffer-mode-map*))
