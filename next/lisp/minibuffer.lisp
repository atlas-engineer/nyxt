;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode) ())

(defvar *minibuffer* nil
  "A variable to store the mini-buffer")
(defparameter *minibuffer-prompt* (qnew "QLabel" "text" "input:")
  "A variable to store the minibuffer prompt")
(defparameter *minibuffer-input* (qnew "QLineEdit")
  "A variable to store the current minibuffer input")
(defparameter *minibuffer-completion-function* nil
  "A variable to store the function used to generate completion candidates")
(defparameter *minibuffer-completion-model* (qnew "QStringListModel")
  " A variable to store the model which updates the QListView")
(defparameter *minibuffer-completion* (qnew "QListView")
  "A variable to store the current minibuffer completion candidates")
(defparameter *minibuffer-callback* nil
  "A variable to store the function upon completion of the minibuffer read")
(defparameter *minibuffer-callback-buffer* nil
  "A variable to store the buffer which originally requested the minibuffer read")

(defun input (callback-function &optional completion-function)
  (setf *minibuffer-callback* callback-function)
  (setf *minibuffer-completion-function* completion-function)
  (setf *minibuffer-callback-buffer* *active-buffer*)
  (|show| (buffer-view *minibuffer*))
  (|setFocus| *minibuffer-input*)
  (set-active-buffer *minibuffer*))

(defun return-input ()
  (set-active-buffer *minibuffer-callback-buffer*)
  (if *minibuffer-completion-function* 
      (progn
	(funcall *minibuffer-callback*
		 (nth 0 (funcall *minibuffer-completion-function* (|text| *minibuffer-input*)))))
      (funcall *minibuffer-callback* (|text| *minibuffer-input*)))
  (|setText| *minibuffer-input* "")
  (|hide| (buffer-view *minibuffer*)))

(defun cancel-input ()
  (set-active-buffer *minibuffer-callback-buffer*)
  (|setText| *minibuffer-input* "")
  (|hide| (buffer-view *minibuffer*)))

(defun minibuffer-mode ()
  "Base mode for input"
  (let ((widget (qnew "QWidget")) (layout (qnew "QGridLayout")))
    (|addWidget| layout *minibuffer-prompt*      0 0 1 5)
    (|addWidget| layout *minibuffer-input*       0 1 1 15)
    (|addWidget| layout *minibuffer-completion*  1 1 1 15)
    (|setLayout| widget layout)
    (|setModel| *minibuffer-completion* *minibuffer-completion-model*)    
    (make-instance 'minibuffer-mode
     :name "minibuffer"
     :keymap minibuffer-mode-map
     :view widget)))

(qadd-event-filter *minibuffer-input* |QEvent.KeyRelease| 'update-candidates)
(defun update-candidates (obj event)
  (declare (ignore obj)) ; supress unused warnings
  (declare (ignore event)) ; supress unused warnings
  (when *minibuffer-completion-function*
    (let ((candidates (funcall *minibuffer-completion-function* (|text| *minibuffer-input*))))
      (|setStringList| *minibuffer-completion-model* candidates)))
  nil)

(define-key minibuffer-mode-map (kbd "Return") #'return-input)
(define-key minibuffer-mode-map (kbd "C-g") #'cancel-input)
(define-key minibuffer-mode-map (kbd "Escape") #'cancel-input)
