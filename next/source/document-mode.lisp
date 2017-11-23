;;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar document-mode-map (make-hash-table :test 'equalp))

(defclass document-mode (mode)
  ((active-history-node :accessor active-history-node :initarg :active-node)
   (link-hints :accessor link-hints)))

(defun scroll-down ()
  (interface:web-view-scroll-down (view *active-buffer*) *scroll-distance*))

(defun scroll-up ()
  (interface:web-view-scroll-up (view *active-buffer*) *scroll-distance*))

(defun history-backwards ()
  ;; move up to parent node to iterate backwards in history tree
  (let ((parent (node-parent (active-history-node (mode *active-buffer*)))))
    (when parent
	(set-url (node-data parent)))))

(defun history-forwards ()
  ;; move forwards in history selecting the first child
  (let ((children (node-children (active-history-node (mode *active-buffer*)))))
    (unless (null children)
      (set-url (node-data (nth 0 children))))))

(defun history-forwards-query (input)
  ;; move forwards in history querying if more than one child present
  (let ((children (node-children (active-history-node (mode *active-buffer*)))))
    (loop for child in children do
	 (when (equalp (node-data child) input)
	   (set-url (node-data child))))))

(defun history-fowards-query-complete (input)
  ;; provide completion candidates to the history-forwards-query function
  (let ((children
	 ;; Find children of active document-mode instance
	 (node-children (active-history-node
			 ;; Find active document-mode instance from minibuffer callback
			 (mode (callback-buffer (mode *minibuffer*)))))))
    (when children
      (fuzzy-match input (mapcar #'node-data children)))))

(defun add-or-traverse-history (mode)
  ;; get url from mode-view's webview
  (let ((url (interface:web-view-get-url (mode-view mode)))
	(active-node (active-history-node mode)))
    ;; only add element to the history if it is different than the current
    (when (equalp url (node-data active-node))
      (return-from add-or-traverse-history t))
    ;; check if parent exists
    (when (node-parent active-node)
      ;; check if parent node's url is equal
      (when (equalp url (node-data (node-parent active-node)))
    	;; set active-node to parent
    	(setf (active-history-node mode) (node-parent active-node))
    	(return-from add-or-traverse-history t)))
    ;; loop through children to make sure node does not exist in children
    (loop for child in (node-children active-node) do
    	 (when (equalp (node-data child) url)
    	   (setf (active-history-node mode) child)
    	   (return-from add-or-traverse-history t)))
    ;; if we made it this far, we must create a new node
    (let ((new-node (make-node :parent active-node :data url)))
      (push new-node (node-children active-node))
      (setf (active-history-node mode) new-node)
      (return-from add-or-traverse-history t))))

(defun set-url-new-buffer (input-url)
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (set-visible-active-buffer new-buffer)
    (set-url input-url)))

(defun set-url-buffer (input-url buffer)
  (setf (name buffer) input-url)
  (interface:web-view-set-url (view buffer) input-url))

(defun setup-url ()
  (set-input (mode *minibuffer*)
	     (interface:web-view-get-url (view *active-buffer*))))

(defun set-url (input-url)
  (let ((url (normalize-url input-url)))
    (set-url-buffer url *active-buffer*)))

(defun normalize-url (input-url)
  "Will convert example.com to https://www.example.com"
  (let ((url (puri:parse-uri input-url)))
    (if (puri:uri-scheme url)
        input-url
        (concatenate 'string "https://" input-url))))

(defun setup-anchor ()
  (erase-input)
  (setf (link-hints (mode *active-buffer*)) (add-link-hints)))

(defun go-anchor (input)
  (loop for hint in (link-hints (mode *active-buffer*))
     do (when (equalp (nth 0 hint) input)
	  (set-url (nth 1 hint)))))

(defun go-anchor-new-buffer (input)
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (loop for hint in (link-hints (mode *active-buffer*))
       do (when (equalp (nth 0 hint) input)
	    (set-url-buffer (nth 1 hint) new-buffer)))))

(defun document-mode ()
  "Base mode for interacting with documents"
  (let* ((root (make-node :data "about:blank"))
	 (mode (make-instance 'document-mode
			      :name "Document-Mode"
			      :keymap document-mode-map
			      :view (interface:make-web-view)
			      :active-node root)))
    (interface:web-view-set-url-loaded-callback
     (mode-view mode)
     (lambda () (add-or-traverse-history mode)))
    ;; return instance of mode
    mode))
