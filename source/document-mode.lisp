;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(define-mode document-mode ()
    "Base mode for interacting with documents."
    ((active-history-node :accessor active-history-node :initarg :active-node
                          :initform (make-instance 'node :data "about:blank"))
     (link-hints :accessor link-hints)
     (keymap
      :initform
      (let ((map (make-keymap)))
        (define-key "M-f" 'history-forwards-query
          "M-b" 'history-backwards
          "C-g" 'go-anchor
          "M-g" 'go-anchor-new-buffer-focus
          "C-u M-g" 'go-anchor-new-buffer
          "C-x C-w" 'copy-anchor-url
          "C-f" 'history-forwards
          "C-b" 'history-backwards
          "button9" 'history-forwards
          "button8" 'history-backwards
          "C-p" 'scroll-up
          "C-n" 'scroll-down
          "C-x C-=" 'zoom-in-page
          "C-x C-HYPHEN" 'zoom-out-page
          "C-x C-0" 'unzoom-page
          "C-r" 'reload-current-buffer
          "C-m o" 'set-url-from-bookmark
          "C-m s" 'bookmark-current-page
          "C-m g" 'bookmark-anchor
          "C-s s" 'add-search-hints
          "C-s n" 'next-search-hint
          "C-s p" 'previous-search-hint
          "C-s k" 'remove-search-hints
          "C-." 'jump-to-heading
          "M-s->" 'scroll-to-bottom
          "M-s-<" 'scroll-to-top
          "M->" 'scroll-to-bottom
          "M-<" 'scroll-to-top
          "C-w" 'copy-url
          "M-w" 'copy-title
          :keymap map)
        map))))

(define-command history-backwards (document-mode)
  "Move up to parent node to iterate backwards in history tree."
  (let ((parent (node-parent (active-history-node
                              (mode (active-buffer *interface*))))))
    (when parent
      (set-url (node-data parent) t))))

(define-command history-forwards (document-mode)
  "Move forwards in history selecting the first child."
  (let ((children (node-children (active-history-node
                                  (mode (active-buffer *interface*))))))
    (unless (null children)
      (set-url (node-data (nth 0 children)) t))))

(defun history-forwards-completion-fn ()
  "Provide completion candidates to the `history-forwards-query' function."
  (let* ((mode (mode (active-buffer *interface*)))
         (children (node-children (active-history-node mode))))
    (lambda (input)
      (if children
          (fuzzy-match input children :accessor-function #'node-data)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "Cannot navigate forwards.")))))

(define-command history-forwards-query (document-mode)
  "Move forwards in history querying if more than one child present."
  (with-result (input (read-from-minibuffer
                       (minibuffer *interface*)
                       :input-prompt "Navigate forwards to:"
                       :completion-function (history-forwards-completion-fn)))
    (unless (equal input "Cannot navigate forwards.")
      (set-url (node-data input)))))

(defmethod add-or-traverse-history ((mode root-mode) url)
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

(define-command set-default-window-title (document-mode)
  "Set current window title to 'Next - TITLE - URL."
  (with-result* ((url (buffer-get-url))
                 (title (buffer-get-title)))
    (%%window-set-title *interface* (%%window-active *interface*)
                        (concatenate 'string "Next - " title " - " url))))

(define-command copy-url (document-mode)
  "Save current URL to clipboard."
  (with-result (url (buffer-get-url))
    (trivial-clipboard:text url)))

(define-command copy-title (document-mode)
  "Save current page title to clipboard."
  (with-result (title (buffer-get-title))
    (trivial-clipboard:text title)))

(defmethod did-commit-navigation ((mode document-mode) url)
  (set-default-window-title mode)
  (add-or-traverse-history mode url)
  (echo (minibuffer *interface*) (concatenate 'string "Loading: " url ".")))

(defmethod did-finish-navigation ((mode document-mode) url)
  (echo (minibuffer *interface*) (concatenate 'string "Finished loading: " url "."))
  ;; TODO: Wait some time before dismissing the minibuffer.
  (echo-dismiss (minibuffer *interface*)))

(defmethod setup ((mode document-mode) (buffer buffer))
  (set-url-buffer (default-new-buffer-url buffer) buffer)
  (call-next-method))
