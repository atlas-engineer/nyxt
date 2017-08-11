;;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar document-mode-hook nil)
(defvar document-mode-map (make-hash-table :test 'equalp))

(defstruct mode
  name
  keymap)

(defun scroll-down ()
  (print "scroll-down"))
(define-key document-mode-map (kbd "C-n") #'scroll-down)

(defun scroll-up ()
  (print "scroll-up"))
(define-key document-mode-map (kbd "C-p") #'scroll-up)

(defun document-mode ()
  "Base mode for interacting with documents"
  (make-mode
   :name "Document-Mode"
   :keymap document-mode-map))
