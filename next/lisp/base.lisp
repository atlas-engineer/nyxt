;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *layout* (qnew "QVBoxLayout"))

(setf *mini-buffer* (generate-new-buffer "mini-buffer" (minibuffer-mode)))
(setf *active-buffer* (generate-new-buffer "default"))

(defun start ()
  (set-url "http://www.google.com")
  (|setMaximumHeight| (buffer-view *mini-buffer*) 25)
  (|setMinimumHeight| (buffer-view *mini-buffer*) 25)
  (|addWidget| *layout* (buffer-view *active-buffer*))
  (|addStretch| *layout*)
  (|addWidget| *layout* (buffer-view *mini-buffer*))
  (|setLayout| *window* *layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
