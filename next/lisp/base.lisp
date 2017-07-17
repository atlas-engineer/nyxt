;;; base.lisp --- main entry point into nEXT

(in-package :next)

(qrequire :webkit)

(defvar *web-view* (generate-new-buffer "default"))

(defun start ()
  (|show| (buffer-web-view *web-view*))
  (set-url "http://www.google.com" *web-view*))
