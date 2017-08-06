;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(setf *active-buffer* (generate-new-buffer "default"))

(defun start ()
  (|show| (buffer-view *active-buffer*))
  (set-url "about:blank" *active-buffer*))

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)

(start)
