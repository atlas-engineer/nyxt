;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar minibuffer-mode-map (make-hash-table :test 'equalp))

(defvar *mini-buffer* nil
  "A variable to store the mini-buffer")
(defparameter *minibuffer-input-string* nil
  "A variable to store the current minibuffer input")
(defparameter *minibuffer-completion-callback* nil
  "A variable to store the function upon completion of the minibuffer read")
(defparameter *minibuffer-completion-callback-buffer* nil
  "A variable to store the buffer which originally requested the minibuffer read")

(defun input (callback-function)
  (setf *minibuffer-completion-callback* callback-function)
  (setf *minibuffer-completion-callback-buffer* *active-buffer*)
  (set-active-buffer *mini-buffer*))

(defun return-input ()
  (set-active-buffer *minibuffer-completion-callback-buffer*)
  (funcall *minibuffer-completion-callback* *minibuffer-input-string*)
  (setf *minibuffer-input-string* nil)
  (update-display))

(define-key minibuffer-mode-map (kbd "Return") #'return-input)

(defun update-display ()
  (|setHtml| (buffer-view *mini-buffer*) *minibuffer-input-string*))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-mode
   :name "Minibuffer-Mode"
   :keymap minibuffer-mode-map))

;; define input keys
(defun insert-character (character)
  (setf *minibuffer-input-string* (concatenate 'string *minibuffer-input-string* character))
  (update-display))

(defun add-binding (key)
  (define-key minibuffer-mode-map (kbd key)
     #'(lambda () (insert-character key))))

(loop for key in
     (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
	   "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
   do (add-binding key))

;; add bindings for puncuation
(define-key minibuffer-mode-map (kbd "Period") #'(lambda () (insert-character ".")))
(define-key minibuffer-mode-map (kbd "Colon") #'(lambda () (insert-character ":")))
(define-key minibuffer-mode-map (kbd "Slash") #'(lambda () (insert-character "/")))
