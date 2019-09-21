(uiop:define-package :next/web-mode
    (:use :common-lisp :trivia :next :annot.class)
  (:documentation "Mode for web pages"))
(in-package :next/web-mode)
(annot:enable-annot-syntax)

;; TODO: Remove web-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generting commands open a new buffer.

(define-mode web-mode ()
  "Base mode for interacting with documents."
  ((history :accessor history
            :type htree:history-tree
            :initarg :history
            :initform (htree:make))
   (keymap-schemes
    :initform
    (let ((emacs-map (make-keymap))
          (vi-map (make-keymap)))
      (define-key :keymap emacs-map
        "M-f" #'history-forwards-all-query
        "M-b" #'history-all-query
        "C-g" #'follow-hint
        "M-g" #'follow-hint-new-buffer-focus
        "C-u M-g" #'follow-hint-new-buffer
        "C-x C-w" #'copy-hint-url
        "C-f" #'history-forwards-query
        "C-b" #'history-backwards-query
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
        "H" #'history-backwards-query
        "L" #'history-forwards-query
        "M-h" #'history-all-query
        "M-l" #'history-forwards-all-query
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
        "g h" #'jump-to-heading         ; TODO: VI binding for this?
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

(declaim (ftype (function (htree:node &optional buffer)) set-url-from-history))
(defun set-url-from-history (history-node &optional (buffer (current-buffer)))
  "Go to HISTORY-NODE's URL."
  (setf (htree:current (history (find-mode buffer 'web-mode))) history-node)
  (set-url (htree:data history-node)))

(define-command history-backwards (&optional (buffer (current-buffer)))
  "Go to parent URL in history."
  (let* ((mode (find-mode buffer 'web-mode)))
    (htree:back (history mode))
    (match (htree:current (history mode))
      ((guard n n) (set-url (htree:data n))))))

(define-command history-forwards (&optional (buffer (current-buffer)))
  "Go to forward URL in history."
  (let* ((mode (find-mode buffer 'web-mode)))
    (htree:forward (history mode))
    (match (htree:current (history mode))
      ((guard n n) (set-url (htree:data n))))))

(defun history-backwards-completion-fn (&optional (mode (find-mode
                                                        (current-buffer)
                                                        'web-mode)))
  "Completion function over all parent URLs."
  (let ((parents (htree:parent-nodes (history mode))))
    (lambda (input)
      (if parents
          (fuzzy-match input parents)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "Cannot navigate backwards.")))))

(define-command history-backwards-query ()
  "Query parent URL to navigate back to."
  (with-result (input (read-from-minibuffer
                       (make-instance 'next::minibuffer
                                      :input-prompt "Navigate backwards to:"
                                      :completion-function (history-backwards-completion-fn))))
    (unless (equal input "Cannot navigate backwards.")
      (set-url-from-history input))))

(defun history-forwards-completion-fn (&optional (mode (find-mode
                                                        (current-buffer)
                                                        'web-mode)))
  "Completion function over forward-children URL."
  (let ((children (htree:forward-children-nodes (history mode))))
    (lambda (input)
      (if children
          (fuzzy-match input children)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "Cannot navigate forwards.")))))

(define-command history-forwards-query ()
  "Query forward-URL to navigate to."
  (with-result (input (read-from-minibuffer
                       (make-instance 'next::minibuffer
                                      :input-prompt "Navigate forwards to:"
                                      :completion-function (history-forwards-completion-fn))))
    (unless (equal input "Cannot navigate forwards.")
      (set-url-from-history input))))

(defun history-forwards-all-completion-fn (&optional (mode (find-mode
                                                        (current-buffer)
                                                        'web-mode)))
  "Completion function over children URL from all branches."
  (let ((children (htree:children-nodes (history mode))))
    (lambda (input)
      (if children
          (fuzzy-match input children)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "Cannot navigate forwards.")))))

(define-command history-forwards-all-query ()
  "Query URL to forward to, from all child branches."
  (with-result (input (read-from-minibuffer
                       (make-instance 'next::minibuffer
                                      :input-prompt "Navigate forwards to (all branches):"
                                      :completion-function (history-forwards-all-completion-fn))))
    (unless (equal input "Cannot navigate forwards.")
      (set-url-from-history input))))

(defun history-all-completion-fn (&optional (mode (find-mode
                                                   (current-buffer)
                                                   'web-mode)))
  "Completion function over all history URLs."
  (let ((urls (htree:all-nodes (history mode))))
    (lambda (input)
      (if urls
          (fuzzy-match input urls)
          ;; TODO: Echo error instead of listing it in candidates.
          (list "No history.")))))

(define-command history-all-query ()
  "Query URL to go to, from the whole history."
  (with-result (input (read-from-minibuffer
                       (make-instance 'next::minibuffer
                                      :input-prompt "Navigate to:"
                                      :completion-function (history-all-completion-fn))))
    (unless (equal input "No history.")
      (set-url-from-history input))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (copy-to-clipboard (url (current-buffer)))
  (echo "~a copied to clipboard." (url (current-buffer))))

(define-command copy-title ()
  "Save current page title to clipboard."
  (copy-to-clipboard (title (current-buffer)))
  (echo "~a copied to clipboard." (title (current-buffer))))

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
(defmethod did-commit-navigation ((mode next/web-mode::web-mode) url)
  (echo "Loading: ~a." url))

(defmethod did-finish-navigation ((mode next/web-mode::web-mode) url)
  (let ((active-window (rpc-window-active)))
    (set-window-title active-window
                      (active-buffer active-window))
    ;; TODO: Include title in buffer history.
    (htree:add-child url (next/web-mode::history mode))
    (when url
      (history-typed-add url))
    (funcall (session-store-function *interface*)))
  (echo "Finished loading: ~a." url)
  ;; TODO: Wait some time before dismissing the minibuffer.
  (echo-dismiss))

(defmethod object-string ((node htree:node))
  (format nil "~a" (htree:data node)))
