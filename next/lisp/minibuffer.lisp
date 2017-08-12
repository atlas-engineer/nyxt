;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defvar *mini-buffer* nil
  "A variable to store the mini-buffer")
(defparameter *minibuffer-prompt* (qnew "QLabel" "text" "input:")
  "A variable to store the current minibuffer input")
(defparameter *minibuffer-input* (qnew "QLineEdit")
  "A variable to store the current minibuffer input")
(defparameter *minibuffer-completion-callback* nil
  "A variable to store the function upon completion of the minibuffer read")
(defparameter *minibuffer-completion-callback-buffer* nil
  "A variable to store the buffer which originally requested the minibuffer read")

(defun input (callback-function)
  (setf *minibuffer-completion-callback* callback-function)
  (setf *minibuffer-completion-callback-buffer* *active-buffer*)
  (|setFocus| *minibuffer-input*)
  (set-active-buffer *mini-buffer*))

(defun return-input ()
  (set-active-buffer *minibuffer-completion-callback-buffer*)
  (funcall *minibuffer-completion-callback* (|text| *minibuffer-input*))
  (|setText| *minibuffer-input* ""))

(define-key minibuffer-mode-map (kbd "Return") #'return-input)

(defun minibuffer-mode ()
  "Base mode for input"
  (let ((widget (qnew "QWidget")) (layout (qnew "QGridLayout")))
    (|addWidget| layout *minibuffer-prompt* 0 0 0 0)
    (|addWidget| layout *minibuffer-input*  0 1 0 10)
    (|setLayout| widget layout)
    (make-mode
     :name "Minibuffer-Mode"
     :keymap minibuffer-mode-map
     :view widget)))

(qadd-event-filter *minibuffer-input* |QEvent.KeyRelease| 'update-candidates)
(defun update-candidates (obj event)
  (declare (ignore obj)) ; supress unused warnings
  (declare (ignore event)) ; supress unused warnings
  nil)
