;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *layout* (qnew "QVBoxLayout"))

(setf *mini-buffer* (generate-new-buffer "mini-buffer" (minibuffer-mode)))
(setf *active-buffer* (generate-new-buffer "default"))

(defun start ()
  (|addWidget| *layout* (buffer-view *active-buffer*))
  (|setLayout| *window* *layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
