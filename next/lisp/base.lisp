;;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)

(defvar *web-view* (generate-new-buffer "default"))

(defun start ()
  (|show| (buffer-web-view *web-view*))
  (set-url "http://www.google.com" *web-view*))

(start)
