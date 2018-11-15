;;; application-mode.lisp --- major mode for "web applications"
;;; has no history or bindings to avoid interference with "web applications"'

(in-package :next)

(defvar *application-mode-map* (make-hash-table :test 'equalp))

(defclass application-mode (mode) ())

(defun application-mode ()
  (make-instance 'application-mode
		 :name "Application-Mode"
		 :keymap *application-mode-map*))
