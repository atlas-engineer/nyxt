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

(defun set-url-buffer (input-url buffer)
  (qlet ((url (qnew "QUrl(QString)" input-url)))
	(|setUrl| (buffer-view buffer) url)))

(defun set-url (input-url)
  (set-url-buffer input-url *active-buffer*))

(defun set-url-read ()
  (input #'set-url))

(define-key document-mode-map (kbd "C-l") #'set-url-read)

(defun document-mode ()
  "Base mode for interacting with documents"
  (make-mode
   :name "Document-Mode"
   :keymap document-mode-map))
