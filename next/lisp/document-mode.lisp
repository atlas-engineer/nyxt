;;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar document-mode-hook nil)
(defvar document-mode-map (make-hash-table :test 'equalp))

(defclass document-mode (mode)
  ((history-tree :accessor mode-history-tree :initform ())
   (history-active-node :accessor mode-history-active-node :initform (make-node))))

(defun scroll-down ()
  (|scroll| (|mainFrame| (|page| (buffer-view *active-buffer*))) 0 30))

(defun scroll-up ()
  (|scroll| (|mainFrame| (|page| (buffer-view *active-buffer*))) 0 -30))

(defun add-or-traverse-history (mode)
  ;; get url from mode-view's qwebview
  (let ((url (|toString| (|url| (mode-view mode))))
	(active-node (mode-history-active-node mode)))
    ;; only add element to the history if it is different than the current
    (when (equalp url (node-data active-node))
      (return-from add-or-traverse-history t))
    ;; check if parent exists
    (when (node-parent active-node)
      ;; check if parent node's url is equal
      (when (equalp url (node-data (node-parent active-node)))
    	;; set active-node to parent
    	(setf (mode-history-active-node mode) (node-parent active-node))
    	(return-from add-or-traverse-history t)))
    ;; loop through children to make sure node does not exist in children
    (loop for child in (node-children active-node) do
    	 (when (equalp (node-data child) url)
    	   (setf (mode-history-active-node mode) child)
    	   (return-from add-or-traverse-history t)))
    ;; if we made it this far, we must create a new node
    (let ((new-node (make-node :parent active-node :data url)))
      (push new-node (node-children active-node))
      (setf (mode-history-active-node mode) new-node)
      (return-from add-or-traverse-history t))))

(defun set-url-new-buffer (input-url)
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (set-visible-active-buffer new-buffer)
    (set-url input-url)))

(defun set-url-buffer (input-url buffer)
  (setf (buffer-name buffer) input-url)
  (qlet ((url (qnew "QUrl(QString)" input-url)))
	(|setUrl| (buffer-view buffer) url)))

(defun set-url (input-url)
  (let ((url (normalize-url input-url)))
    (set-url-buffer url *active-buffer*)))

(defun normalize-url (input-url)
  "Will convert example.com to http://www.example.com"
  (let ((url (quri:uri input-url)))
    (if (quri:uri-scheme url)
        input-url
        (concatenate 'string "http://" input-url ))))

(defun document-mode ()
  "Base mode for interacting with documents"
  (let ((mode 
	 (make-instance 'document-mode
			:name "Document-Mode"
			:keymap document-mode-map
			:view (qnew "QWebView"))))
    (qconnect (|mainFrame| (|page| (mode-view mode))) "loadFinished(bool)"
	      (lambda (ok) (add-or-traverse-history mode)))
    ;; return instance of mode
    mode))

(define-key document-mode-map (kbd "C-p") #'scroll-up)
(define-key document-mode-map (kbd "C-n") #'scroll-down)
(define-key document-mode-map (kbd "C-l") (:input set-url))
(define-key document-mode-map (kbd "S-l") (:input set-url-new-buffer))
