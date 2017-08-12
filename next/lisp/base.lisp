;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *layout* (qnew "QGridLayout"))

(setf *mini-buffer* (generate-new-buffer "mini-buffer" (minibuffer-mode)))
(setf *active-buffer* (generate-new-buffer "default"))

;; Used by QT to capture key presses
(qadd-event-filter *window* |QEvent.KeyPress| 'key-press)
(qadd-event-filter *window* |QEvent.KeyRelease| 'key-release)

(defun start ()
  ;; addWidget(*Widget, row, column, rowspan, colspan)
  ;; webview
  (|addWidget| *layout* (buffer-view *active-buffer*) 0 0 9 0)
  (set-url "http://www.google.com")
  
  ;; minibuffer layout
  (setf *minibuffer-prompt* (qnew "QLabel" "text" "input:"))
  (setf *minibuffer-input* (qnew "QLineEdit"))
  
  (|addWidget| *layout* *minibuffer-prompt* 9 0 1 0)
  (|addWidget| *layout* *minibuffer-input*  9 1 1 10)
  
  (|setLayout| *window* *layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
