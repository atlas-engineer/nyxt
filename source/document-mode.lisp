;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(define-mode document-mode ()
  "Base mode for interacting with documents."
  ((active-history-node :accessor active-history-node :initarg :active-node
                        :initform (make-instance 'node :data "about:blank"))
   (link-hints :accessor link-hints)
   (keymap-schemes
    :initform
    (let ((emacs-map (make-keymap))
          (vi-map (make-keymap)))
      (define-key :keymap emacs-map
        "M-f" 'history-forwards-query
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
        "C-s s" 'add-search-hints
        "C-s n" 'next-search-hint
        "C-s p" 'previous-search-hint
        "C-s k" 'remove-search-hints
        "C-." 'jump-to-heading
        "M-s->" 'scroll-to-bottom
        "M-s-<" 'scroll-to-top
        "M->" 'scroll-to-bottom
        "M-<" 'scroll-to-top
        "C-v" 'scroll-page-down
        "M-v" 'scroll-page-up
        "C-w" 'copy-url
        "M-w" 'copy-title
        "SPACE" 'scroll-page-down
        "s-SPACE" 'scroll-page-up
        "C-m o" 'bookmark-open-bookmark
        "C-m m" 'bookmark-current-page
        "C-m a" 'bookmark-anchor
        "C-m u" 'bookmark-url
        "C-m k" 'bookmark-delete)
      (define-key :keymap vi-map
        "H" 'history-backwards
        "L" 'history-forwards
        "f" 'go-anchor
        "F" 'go-anchor-new-buffer-focus
        "; f" 'go-anchor-new-buffer
        "button9" 'history-forwards
        "button8" 'history-backwards
        "h" 'scroll-left
        "j" 'scroll-down
        "k" 'scroll-up
        "l" 'scroll-right
        "Left" 'scroll-left
        "Down" 'scroll-down
        "Up" 'scroll-up
        "Right" 'scroll-right
        "z i" 'zoom-in-page
        "z o" 'zoom-out-page
        "z z" 'unzoom-page
        "r" 'reload-current-buffer
        "y u" 'copy-url
        "y t" 'copy-title
        "g h" 'jump-to-heading        ; TODO: VI binding for this?
        "/" 'add-search-hints
        "n" 'next-search-hint
        "N" 'previous-search-hint
        "?" 'remove-search-hints
        "G" 'scroll-to-bottom
        "g g" 'scroll-to-top
        "C-f" 'scroll-page-down
        "C-b" 'scroll-page-up
        "SPACE" 'scroll-page-down
        "s-SPACE" 'scroll-page-up
        "m o" 'bookmark-open-bookmark
        "m m" 'bookmark-current-page
        "m k" 'bookmark-delete
        "m u" 'bookmark-url
        "m a" 'bookmark-anchor)
      (list :emacs emacs-map
            :vi-normal vi-map))))
  ;; Init.
  ;; TODO: Do we need to set the default URL?  Maybe not.
  ;; (set-url-buffer (default-new-buffer-url (buffer %mode))
  ;;                 (buffer %mode))
  )

(define-command history-backwards (document-mode)
  "Move up to parent node to iterate backwards in history tree."
  (let ((parent (node-parent (active-history-node document-mode
                                                  ;; TODO: Test!
                              ;; (mode (active-buffer *interface*))
                                                  ))))
    (when parent
      (set-url (node-data parent) t))))

(define-command history-forwards (document-mode)
  "Move forwards in history selecting the first child."
  (let ((children (node-children (active-history-node
                                  document-mode
                                  ;; (mode (active-buffer *interface*))
                                  ))))
    (unless (null children)
      (set-url (node-data (nth 0 children)) t))))

(defun history-forwards-completion-fn ()
  "Provide completion candidates to the `history-forwards-query' function."
  ;; TODO: Find right mode.
  (let* ((mode (first (modes (active-buffer *interface*))))
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

(defmethod add-or-traverse-history ((mode document-mode) url)
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
    (setf title (if (str:emptyp title) "<untitled>" title))
    (setf url (if (str:emptyp url) "<no url>" url))
    (rpc-window-set-title *interface* (rpc-window-active *interface*)
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
  (echo "Loading: ~a." url))

(defmethod did-finish-navigation ((mode document-mode) url)
  (echo "Finished loading: ~a." url)
  ;; TODO: Wait some time before dismissing the minibuffer.
  (echo-dismiss (minibuffer *interface*)))
