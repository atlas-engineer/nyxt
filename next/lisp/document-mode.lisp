;;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar document-mode-hook nil)
(defvar document-mode-map (make-hash-table :test 'equalp))

(defstruct mode
  name
  keymap
  view)

(defun scroll-down ()
  (print "scroll-down"))

(defun scroll-up ()
  (print "scroll-up"))

(defun set-url-new-buffer ()
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (set-visible-active-buffer new-buffer)
    (set-url-read)))

(defun set-url-buffer (input-url buffer)
  (qlet ((url (qnew "QUrl(QString)" input-url)))
	(|setUrl| (buffer-view buffer) url)))

(defun set-url (input-url)
  (set-url-buffer input-url *active-buffer*))

(defun set-url-read ()
  (input #'set-url))

(defun document-mode ()
  "Base mode for interacting with documents"
  (make-mode
   :name "Document-Mode"
   :keymap document-mode-map
   :view (qnew "QWebView")))

(define-key document-mode-map (kbd "C-p") #'scroll-up)
(define-key document-mode-map (kbd "C-n") #'scroll-down)
(define-key document-mode-map (kbd "C-l") #'set-url-read)
(define-key document-mode-map (kbd "S-l") #'set-url-new-buffer)
