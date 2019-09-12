(uiop:define-package :next/document-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Mode for web pages"))
(in-package :next/document-mode)

(defclass node ()
  ((parent :accessor node-parent :initarg :parent :initform nil)
   (children :accessor node-children :initform nil)
   (data :accessor node-data :initarg :data :initform nil))
  (:documentation "Data node used to represent tree history."))

(defmethod object-string ((node node))
  (node-data node))

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
          "M-f" #'history-forwards-query
          "M-b" #'history-backwards
          "C-g" #'follow-hint
          "M-g" #'follow-hint-new-buffer-focus
          "C-u M-g" #'follow-hint-new-buffer
          "C-x C-w" #'copy-hint-url
          "C-f" #'history-forwards
          "C-b" #'history-backwards
          "C-v" #'paste
          "C-c" #'copy
          "button9" #'history-forwards
          "button8" #'history-backwards
          "C-p" #'scroll-up
          "C-n" #'scroll-down
          "C-x C-=" #'zoom-in-page
          "C-x C-+" #'zoom-in-page
          "C-x +" #'zoom-in-page
          "C-x C-HYPHEN" #'zoom-out-page
          "C-x HYPHEN" #'zoom-out-page
          "C-x C-0" #'unzoom-page
          "C-x 0" #'unzoom-page
          "C-r" #'reload-current-buffer
          "C-m o" #'set-url-from-bookmark
          "C-m s" #'bookmark-current-page
          "C-m g" #'bookmark-hint
          "C-s s" #'search-buffer
          "C-s n" #'next-search-hint
          "C-s p" #'previous-search-hint
          "C-s k" #'remove-search-hints
          "C-." #'jump-to-heading
          "M-s->" #'scroll-to-bottom
          "M-s-<" #'scroll-to-top
          "M->" #'scroll-to-bottom
          "M-<" #'scroll-to-top
          ;; "C-v" #'scroll-page-down
          "M-v" #'scroll-page-up
          "C-w" #'copy-url
          "M-w" #'copy-title
          ;; Leave SPACE unbound so that the paltform port decides wether to
          ;; insert of scroll.
          "s-SPACE" #'scroll-page-up

          ;; keypad:
          "Page_Up" #'scroll-page-up
          "Page_Down" #'scroll-page-down
          "Page_End" #'scroll-to-bottom
          "Page_Home" #'scroll-to-top
          ;; keypad, gtk:
          "KP_Left" #'scroll-left
          "KP_Down" #'scroll-down
          "KP_Up" #'scroll-up
          "KP_Right" #'scroll-right
          "KP_End" #'scroll-to-bottom
          "KP_Home" #'scroll-to-top
          "KP_Next" #'scroll-page-down
          "KP_Page_Up" #'scroll-page-up
          "KP_Prior" #'scroll-page-up)

        (define-key :keymap vi-map
          "H" #'history-backwards
          "L" #'history-forwards
          "f" #'follow-hint
          "F" #'follow-hint-new-buffer-focus
          "; f" #'follow-hint-new-buffer
          "button9" #'history-forwards
          "button8" #'history-backwards

          "h" #'scroll-left
          "j" #'scroll-down
          "k" #'scroll-up
          "l" #'scroll-right
          "Left" #'scroll-left
          "Down" #'scroll-down
          "Up" #'scroll-up
          "Right" #'scroll-right
          ;; keypad:
          "Page_End" #'scroll-to-bottom
          "Page_Home" #'scroll-to-top
          ;; keypad, gtk:
          "KP_Left" #'scroll-left
          "KP_Down" #'scroll-down
          "KP_Up" #'scroll-up
          "KP_Right" #'scroll-right
          "KP_End" #'scroll-to-bottom
          "KP_Home" #'scroll-to-top
          "KP_Next" #'scroll-page-down
          "KP_Page_Up" #'scroll-page-up
          "KP_Prior" #'scroll-page-up

          "z i" #'zoom-in-page
          "z o" #'zoom-out-page
          "z z" #'unzoom-page
          "r" #'reload-current-buffer
          "m o" #'set-url-from-bookmark
          "m m" #'bookmark-current-page
          "m f" #'bookmark-hint
          "y u" #'copy-url
          "y t" #'copy-title
          "g h" #'jump-to-heading        ; TODO: VI binding for this?
          "/" #'search-buffer
          "n" #'next-search-hint
          "N" #'previous-search-hint
          "?" #'remove-search-hints
          "G" #'scroll-to-bottom
          "g g" #'scroll-to-top
          "C-f" #'scroll-page-down
          "C-b" #'scroll-page-up
          "SPACE" #'scroll-page-down
          "s-SPACE" #'scroll-page-up
          "Page_Up" #'scroll-page-up
          "Page_Down" #'scroll-page-down)
        (list :emacs emacs-map
              :vi-normal vi-map))))
  ;; Init.
  ;; TODO: Do we need to set the default URL?  Maybe not.
  ;; (set-url (default-new-buffer-url (buffer %mode))
  ;;                 (buffer %mode))
  )

(define-command history-backwards (&optional (buffer (active-buffer *interface*)))
  "Move up to parent node to iterate backwards in history tree."
  (let* ((document-mode (find-mode buffer 'document-mode))
         (parent (node-parent (active-history-node document-mode
                                                   ;; TODO: Test!
                                                   ;; (mode (active-buffer *interface*))
                                                   ))))
    (when parent
      (set-url (node-data parent)))))

(define-command history-forwards (&optional (buffer (active-buffer *interface*)))
  "Move forwards in history selecting the first child."
  (let* ((document-mode (find-mode buffer 'document-mode))
         (children (node-children (active-history-node
                                   document-mode
                                   ;; (mode (active-buffer *interface*))
                                   ))))
    (unless (null children)
      (set-url (node-data (nth 0 children))))))

(defun history-forwards-completion-fn (&optional (mode (find-mode
                                                        (active-buffer *interface*)
                                                        'document-mode)))
  "Provide completion candidates to the `history-forwards-query' function."
  (let ((children (node-children (active-history-node mode))))
    (lambda (input)
      (if children
          (fuzzy-match input children)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "Cannot navigate forwards.")))))

(define-command history-forwards-query ()
  "Move forwards in history querying if more than one child present."
  (with-result (input (read-from-minibuffer
                       (make-instance 'minibuffer
                                      :input-prompt "Navigate forwards to:"
                                      :completion-function (history-forwards-completion-fn))))
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
      (history-typed-add url)) ; add to history database
    (let ((new-node (make-instance 'node
                                   :parent active-node
                                   :data url)))
      (push new-node (node-children active-node))
      (setf (active-history-node mode) new-node)
      (return-from add-or-traverse-history t))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (with-result (url (buffer-get-url))
    (copy-to-clipboard url)
    (echo "~a copied to clipboard." url)))

(define-command copy-title ()
  "Save current page title to clipboard."
  (with-result (title (buffer-get-title))
    (copy-to-clipboard title)
    (echo "~a copied to clipboard." title)))

(define-parenscript %paste ((input-text (ring-insert-clipboard (clipboard-ring *interface*))))
  (let* ((active-element (ps:chain document active-element))
         (start-position (ps:chain active-element selection-start))
         (end-position (ps:chain active-element selection-end)))
    (setf (ps:chain active-element value)
          (+ (ps:chain active-element value (substring 0 start-position))
             (ps:lisp input-text)
             (ps:chain active-element value
                       (substring end-position
                                  (ps:chain active-element value length)))))))

(define-command paste ()
  "Paste from clipboard into active-element."
  (%paste))

(defun ring-completion-fn (ring)
  (let ((ring-items (ring:recent-list ring)))
    (lambda (input)
      (fuzzy-match input ring-items))))

(define-command paste-from-ring ()
  "Show `*interface*' clipboard ring and paste selected entry."
  (with-result (ring-item (read-from-minibuffer
                           (make-instance 'minibuffer
                                          :completion-function (ring-completion-fn
                                                                (clipboard-ring *interface*)))))
    (%paste :input-text ring-item)))

(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

(defun copy-to-clipboard (input)
  "Save INPUT text to clipboard, and ring."
  (ring:insert (clipboard-ring *interface*) (trivial-clipboard:text input)))

(define-command copy ()
  "Copy selected text to clipboard."
  (with-result (input (%copy))
    (copy-to-clipboard input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warning: To specialize `did-commit-navigation' we must be in the right package.
(in-package :next)
(defmethod did-commit-navigation ((mode next/document-mode::document-mode) url)
  (echo "Loading: ~a." url))

(defmethod did-finish-navigation ((mode next/document-mode::document-mode) url)
  (let ((active-window (rpc-window-active *interface*)))
    (set-window-title *interface*
                      active-window
                      (active-buffer active-window))
    (next/document-mode::add-or-traverse-history mode url)
    (funcall (session-store-function active-window)))
  (echo "Finished loading: ~a." url)
  ;; TODO: Wait some time before dismissing the minibuffer.
  (echo-dismiss))
