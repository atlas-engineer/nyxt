;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defparameter *minibuffer-input-string* nil
  "A variable to store the current minibuffer input")
(defparameter *minibuffer-completion-callback* nil
  "A variable to store the function upon completion of the minibuffer read")

(defun return-input ()
  (funcall *minibuffer-completion-callback* *minibuffer-input-string*)
  (setf *minibuffer-input-string* nil)
  (update-display))
(define-key minibuffer-mode-map (kbd "Return") #'return-input)

(defun input (callback-function)
  (setf *minibuffer-completion-callback* callback-function))

(defun update-display ()
  (|setHtml| (buffer-view *mini-buffer*) *minibuffer-input-string*))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-mode
   :name "Minibuffer-Mode"
   :keymap minibuffer-mode-map))

(defun printy-text (text)
  (print text))

(defun print-text-read ()
  (input #'printy-text))
(define-key minibuffer-mode-map (kbd "C-a") #'print-text-read)

;; define input keys
(defun insert-a ()
  (setf *minibuffer-input-string* (concatenate 'string *minibuffer-input-string* "a"))
  (update-display))
(define-key minibuffer-mode-map (kbd "a") #'insert-a)

(defun insert-b ()
  (setf *minibuffer-input-string* (concatenate 'string *minibuffer-input-string* "b"))
  (update-display))
(define-key minibuffer-mode-map (kbd "b") #'insert-b)

(defun insert-c ()
  (setf *minibuffer-input-string* (concatenate 'string *minibuffer-input-string* "c"))
  (update-display))
(define-key minibuffer-mode-map (kbd "c") #'insert-c)
