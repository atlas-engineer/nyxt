;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar *document-mode-map* (make-hash-table :test 'equalp))

(define-mode document-mode (mode)
  ((active-history-node :accessor active-history-node :initarg :active-node)
   (link-hints :accessor link-hints))
  (define-key *document-mode-map* (kbd "M-f") 'history-forwards-query)
  (define-key *document-mode-map* (kbd "M-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-g") 'go-anchor)
  (define-key *document-mode-map* (kbd "M-g") 'go-anchor-new-buffer)
  (define-key *document-mode-map* (kbd "S-g") 'go-anchor-new-buffer-focus)
  (define-key *document-mode-map* (kbd "C-f") 'history-forwards)
  (define-key *document-mode-map* (kbd "C-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-p") 'scroll-up)
  (define-key *document-mode-map* (kbd "C-n") 'scroll-down)
  (define-key *document-mode-map* (kbd "C-x C-=") 'zoom-in-page)
  (define-key *document-mode-map* (kbd "C-x C-HYPHEN") 'zoom-out-page)
  (define-key *document-mode-map* (kbd "C-x C-0") 'unzoom-page)
  (define-key *document-mode-map* (kbd "C-l") 'set-url-current-buffer)
  (define-key *document-mode-map* (kbd "S-b o") 'set-url-from-bookmark)
  (define-key *document-mode-map* (kbd "S-b s") 'bookmark-current-page)
  (define-key *document-mode-map* (kbd "S-b g") 'bookmark-anchor)
  (define-key *document-mode-map* (kbd "C-[") 'switch-buffer-previous)
  (define-key *document-mode-map* (kbd "C-]") 'switch-buffer-next)
  (define-key *document-mode-map* (kbd "S-s s") 'add-search-boxes)
  (define-key *document-mode-map* (kbd "S-s n") 'next-search-hint)
  (define-key *document-mode-map* (kbd "S-s p") 'previous-search-hint)
  (define-key *document-mode-map* (kbd "S-s k") 'remove-search-hints)
  (define-key *document-mode-map* (kbd "C-.") 'jump-to-heading)
  (define-key *document-mode-map* (kbd "M-s->") 'scroll-to-bottom)
  (define-key *document-mode-map* (kbd "M-s-<") 'scroll-to-top))

(define-command history-backwards ()
  "Move up to parent node to iterate backwards in history tree."
  (let ((parent (node-parent (active-history-node 
                              (mode (active-buffer *interface*))))))
    (when parent
      (set-url (node-data parent) t))))

(define-command history-forwards ()
  "Move forwards in history selecting the first child."
  (let ((children (node-children (active-history-node
                                  (mode (active-buffer *interface*))))))
    (unless (null children)
      (set-url (node-data (nth 0 children)) t))))

(defun history-forwards-completion-fn ()
  ;; provide completion candidates to the history-forwards-query function
  (let* ((mode (mode (active-buffer *interface*)))
         (children (node-children (active-history-node mode))))
    (lambda (input)
      (if children
          (fuzzy-match input children :accessor-function #'node-data)
          (list "Cannot navigate forwards.")))))

(define-command history-forwards-query ()
  "Move forwards in history querying if more than one child present."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :input-prompt "Navigate forwards to:"
                       :completion-function (history-forwards-completion-fn)))
    (unless (equal input "Cannot navigate forwards.")
      (set-url (node-data input)))))

(defmethod add-or-traverse-history ((mode mode) url)
  (let ((active-node (active-history-node mode)))
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
    (when url
      (history-add url)) ; add to history database
    (let ((new-node (make-instance 'node
                                   :parent active-node
                                   :data url)))
      (push new-node (node-children active-node))
      (setf (active-history-node mode) new-node)
      (return-from add-or-traverse-history t))))

(defmethod did-commit-navigation ((mode document-mode) url)
  (add-or-traverse-history mode url))

(defun document-mode ()
  "Base mode for interacting with documents"
  (let* ((root (make-instance 'node
                              :data "about:blank"))
	 (mode (make-instance 'document-mode
			      :name "Document-Mode"
			      :keymap *document-mode-map*
			      :active-node root)))
    mode))
