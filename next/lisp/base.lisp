;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defparameter *window* (qnew "QWidget" "windowTitle" "nEXT"))
(defparameter *layout* (qnew "QVBoxLayout"))
(setf *mini-buffer* (generate-new-buffer "default" (minibuffer-mode)))
(setf *active-buffer* *mini-buffer*)

(defun set-active-buffer (buffer)
  (|removeWidget| *layout* (buffer-view *active-buffer*))
  (setf *active-buffer* buffer)
  (|addWidget| *layout* (buffer-view *active-buffer*)))

(defun start ()
  (|addWidget| *layout* (buffer-view *active-buffer*))
  (|setLayout| *window* *layout*)
  (|show| *window*))

;; start nEXT
(start)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
