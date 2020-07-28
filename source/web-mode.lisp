(uiop:define-package :nyxt/web-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for web pages"))
(in-package :nyxt/web-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

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
   (history-blacklist :accessor history-blacklist
                      :type list-of-strings
                      :initarg :history-blacklist
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
                      :initform '("https://duckduckgo.com/l/")
                      :documentation "URI prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the duckduckgo
search.")
   (keymap-scheme
    :initform
    (define-scheme "web"
      scheme:cua
      (list
       "C-M-right" 'history-forwards-all-query
       "C-M-left" 'history-all-query
       "C-shift-h" 'history-all-query
       "C-shift-H" 'history-all-query
       "M-shift-right" 'history-forwards-query
       "M-shift-left" 'history-backwards-query
       "M-right" 'history-forwards
       "M-left" 'history-backwards
       "M-]" 'history-forwards
       "M-[" 'history-backwards
       "C-g" 'follow-hint
       "M-g" 'follow-hint-new-buffer-focus
       "C-u M-g" 'follow-hint-new-buffer
       "C-x C-w" 'copy-hint-url
       "C-v" 'paste
       "button2" 'paste-or-set-url
       "C-c" 'copy
       "button9" 'history-forwards
       "button8" 'history-backwards
       "C-+" 'zoom-in-page
       "C-=" 'zoom-in-page              ; Because + shifted = on QWERTY.
       "C-hyphen" 'zoom-out-page
       "C-0" 'unzoom-page
       "C-button4" 'zoom-in-page
       "C-button5" 'zoom-out-page
       "C-M-c" 'open-inspector
       "C-m g" 'bookmark-hint
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-hints
       "C-." 'jump-to-heading
       "end" 'maybe-scroll-to-bottom
       "home" 'maybe-scroll-to-top
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       "C-i" 'autofill
       "C-c '" 'fill-input-from-external-editor
       ;; Leave SPACE and arrow keys unbound so that the renderer decides wether to
       ;; navigate textboxes (arrows), insert or scroll (space).
       "pageup" 'scroll-page-up
       "pagedown" 'scroll-page-down
       "pageend" 'scroll-to-bottom
       "pagehome" 'scroll-to-top
       ;; keypad, gtk:
       "keypadleft" 'scroll-left
       "keypaddown" 'scroll-down
       "keypadup" 'scroll-up
       "keypadright" 'scroll-right
       "keypadend" 'scroll-to-bottom
       "keypadhome" 'scroll-to-top
       "keypadnext" 'scroll-page-down
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up)

      scheme:emacs
      (list
       "C-M-f" 'history-forwards-all-query
       "C-M-b" 'history-all-query
       "M-f" 'history-forwards-query
       "M-b" 'history-backwards-query
       "C-f" 'history-forwards
       "C-b" 'history-backwards
       "C-g" 'noop                      ; Emacs users may hit C-g out of habit.
       "M-g M-g" 'follow-hint           ; Corresponds to Emacs' `goto-line'.
       "M-g g" 'follow-hint-new-buffer-focus
       "C-u M-g M-g" 'follow-hint-new-buffer
       "C-u M-g g" 'follow-hint-new-buffer
       "C-x C-w" 'copy-hint-url
       "C-y" 'paste
       "M-w" 'copy
       "button9" 'history-forwards
       "button8" 'history-backwards
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "C-x C-+" 'zoom-in-page
       "C-x C-=" 'zoom-in-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'zoom-out-page
       "C-x C-0" 'unzoom-page
       "C-m g" 'bookmark-hint
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-hints
       "C-." 'jump-to-heading
       "M-s->" 'scroll-to-bottom
       "M-s-<" 'scroll-to-top
       "M->" 'scroll-to-bottom
       "M-<" 'scroll-to-top
       "C-v" 'scroll-page-down
       "M-v" 'scroll-page-up)

      scheme:vi-normal
      (list
       "H" 'history-backwards
       "L" 'history-forwards
       "M-h" 'history-backwards-query
       "M-l" 'history-forwards-query
       "M-H" 'history-all-query
       "M-L" 'history-forwards-all-query
       "f" 'follow-hint
       "F" 'follow-hint-new-buffer-focus
       "; f" 'follow-hint-new-buffer
       "button9" 'history-forwards
       "button8" 'history-backwards
       "C-v" 'paste
       "+" 'zoom-in-page
       "hyphen" 'zoom-out-page
       "0" 'unzoom-page
       "z i" 'zoom-in-page
       "z o" 'zoom-out-page
       "z z" 'unzoom-page
       "g h" 'jump-to-heading ; REVIEW: VI binding?  "gh" is probably good enough.
       "/" 'search-buffer
       "?" 'remove-search-hints
       "m f" 'bookmark-hint
       "h" 'scroll-left
       "j" 'scroll-down
       "k" 'scroll-up
       "l" 'scroll-right
       "G" 'scroll-to-bottom
       "g g" 'scroll-to-top
       "C-f" 'scroll-page-down
       "C-b" 'scroll-page-up
       "space" 'scroll-page-down
       "s-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "pagedown" 'scroll-page-down))))
  ;; Init.
  ;; TODO: Do we need to set the default URL?  Maybe not.
  ;; (buffer-load (default-new-buffer-url (buffer %mode))
  ;;                 (buffer %mode))
  )

(sera:export-always '%clicked-in-input?)
(define-parenscript %clicked-in-input? ()
  (ps:chain document active-element tag-name))

(sera:export-always 'input-tag-p)
(declaim (ftype (function ((or string null)) boolean) input-tag-p))
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(defun call-non-input-command-or-forward (command &key (buffer (current-buffer))
                                                    (window (current-window)))
  (with-result (response (%clicked-in-input?))
    (if (input-tag-p response)
        (ffi-generate-input-event
         window
         (nyxt::last-event buffer))
        (funcall-safely command))))

(define-command paste-or-set-url (&optional (buffer (current-buffer)))
  "Paste text if active element is an input tag, forward event otherwise."
  (with-result (response (%clicked-in-input?))
    (let ((url-empty (url-empty-p (url-at-point buffer))))
      (if (and (input-tag-p response) url-empty)
          (funcall-safely #'paste)
          (unless url-empty
            (buffer-load (url-at-point buffer) :buffer (make-buffer-focus)))))))

(define-command maybe-scroll-to-bottom (&optional (buffer (current-buffer)))
  "Scroll to bottom if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-bottom :buffer buffer))

(define-command maybe-scroll-to-top (&optional (buffer (current-buffer)))
  "Scroll to top if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-top :buffer buffer))

(declaim (ftype (function (htree:node &optional buffer)) set-url-from-history))
(defun set-url-from-history (history-node &optional (buffer (current-buffer)))
  "Go to HISTORY-NODE's URL."
  (let ((history (history (find-submode buffer 'web-mode))))
    (if (eq history-node (htree:current history))
        (echo "History entry is already the current URL.")
        (progn
          (setf (htree:current history) history-node)
          (buffer-load (url (htree:data history-node)))))))

(define-command history-backwards (&optional (buffer (current-buffer)))
  "Go to parent URL in history."
  (let* ((mode (find-submode buffer 'web-mode)))
    (if (eq (htree:root (history mode)) (htree:current (history mode)))
        (echo "No backward history.")
        (progn
          (htree:back (history mode))
          (match (htree:current (history mode))
            ((guard n n) (buffer-load (url (htree:data n)))))))))

(define-command history-forwards (&optional (buffer (current-buffer)))
  "Go to forward URL in history."
  (let* ((mode (find-submode buffer 'web-mode)))
    (if (htree:children-nodes (history mode))
        (progn
          (htree:forward (history mode))
          (match (htree:current (history mode))
            ((guard n n) (buffer-load (url (htree:data n))))))
        (echo "No forward history."))))

(defun history-backwards-suggestion-filter (&optional (mode (find-submode
                                                             (current-buffer)
                                                             'web-mode)))
  "Suggestion function over all parent URLs."
  (let ((parents (htree:parent-nodes (history mode))))
    (lambda (minibuffer)
      (if parents
          (fuzzy-match (input-buffer minibuffer) parents)
          (error "Cannot navigate backwards.")))))

(define-command history-backwards-query ()
  "Query parent URL to navigate back to."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Navigate backwards to"
                        :suggestion-function (history-backwards-suggestion-filter))))
    (when input
      (set-url-from-history input))))

(defun history-forwards-suggestion-filter (&optional (mode (find-submode
                                                            (current-buffer)
                                                            'web-mode)))
  "Suggestion function over forward-children URL."
  (let ((children (htree:forward-children-nodes (history mode))))
    (lambda (minibuffer)
      (if children
          (fuzzy-match (input-buffer minibuffer) children)
          (error "Cannot navigate forwards.")))))

(define-command history-forwards-query ()
  "Query forward-URL to navigate to."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Navigate forwards to"
                        :suggestion-function (history-forwards-suggestion-filter))))
    (when input
      (set-url-from-history input))))

(define-command history-forwards-maybe-query (&optional (mode (find-submode
                                                               (current-buffer)
                                                               'web-mode)))
  "If current node has multiple chidren, query forward-URL to navigate to.
Otherwise go forward to the only child."
  (if (<= 2 (length (htree:children-nodes (history mode))))
      (history-forwards-all-query)
      (history-forwards)))

(defun history-forwards-all-suggestion-filter (&optional (mode (find-submode
                                                                (current-buffer)
                                                                'web-mode)))
  "Suggestion function over children URL from all branches."
  (let ((children (htree:children-nodes (history mode))))
    (lambda (minibuffer)
      (if children
          (fuzzy-match (input-buffer minibuffer) children)
          (error "Cannot navigate forwards.")))))

(define-command history-forwards-all-query ()
  "Query URL to forward to, from all child branches."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Navigate forwards to (all branches)"
                        :suggestion-function (history-forwards-all-suggestion-filter))))
    (when input
      (set-url-from-history input))))

(defun history-all-suggestion-filter (&optional (mode (find-submode
                                                       (current-buffer)
                                                       'web-mode)))
  "Suggestion function over all history URLs."
  (let ((urls (htree:all-nodes (history mode))))
    (lambda (minibuffer)
      (if urls
          (fuzzy-match (input-buffer minibuffer) urls)
          (error "No history.")))))

(define-command history-all-query ()
  "Query URL to go to, from the whole history."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Navigate to"
                        :suggestion-function (history-all-suggestion-filter))))
    (when input
      (set-url-from-history input))))

(define-command buffer-history-tree (&optional (buffer (current-buffer)))
  "Open a new buffer displaying the whole history tree."
  (labels ((traverse (node current)
             (when node
               `(:li (:a :href ,(object-string (url (htree:data node)))
                         ,(let ((title (or (match (title (htree:data node))
                                             ((guard e (not (str:emptyp e))) e))
                                           (object-display (url (htree:data node))))))
                            (if (eq node current)
                                `(:b ,title)
                                title)))
                     ,(match (mapcar (lambda (n) (traverse n current))
                                     (htree:children node))
                        ((guard l l) `(:ul ,@l)))))))
    (let* ((buffer-name (format nil "*History-~a*" (id buffer)))
           (output-buffer (or (find-if (lambda (b) (string= buffer-name (title b)))
                                       (buffer-list))
                              (nyxt/help-mode:help-mode
                               :activate t :buffer (make-buffer :title buffer-name))))
           (history (history (find-submode buffer 'web-mode)))
           (tree `(:ul ,(traverse (htree:root history)
                                  (htree:current history))))
           (content (markup:markup*
                     '(:h1 "History")
                     tree))
           (insert-content (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript output-buffer insert-content)
      (set-current-buffer output-buffer))))

(define-command paste ()
  "Paste from clipboard into active-element."
  ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
  ;; copy something from the context menu 'Copy' action, upon pasting we will
  ;; retrieved the text from the GTK thread.  This is prone to create
  ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
  ;;
  ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
  ;; is present the clipboard and need not be retrieved from the GTK thread.
  (bt:make-thread
   (lambda ()
     (trivial-clipboard:text (trivial-clipboard:text))
     (ffi-within-renderer-thread *browser* #'%paste))))

(defun ring-suggestion-filter (ring)
  (let ((ring-items (containers:container->list ring)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) ring-items))))

(define-command paste-from-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (with-result (ring-item (read-from-minibuffer
                           (make-minibuffer
                            :suggestion-function (ring-suggestion-filter
                                                  (nyxt::clipboard-ring *browser*)))))
    (%paste :input-text ring-item)))

(define-command copy ()
  "Copy selected text to clipboard."
  (with-result (input (%copy))
    (copy-to-clipboard input)
    (echo "Text copied.")))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (with-result (selected-fill (read-from-minibuffer
                               (make-minibuffer
                                :input-prompt "Autofill"
                                :suggestion-function
                                (lambda (minibuffer)
                                  (fuzzy-match (input-buffer minibuffer)
                                               (autofills *browser*))))))
    (cond ((stringp (autofill-fill selected-fill))
           (%paste :input-text (autofill-fill selected-fill)))
          ((functionp (autofill-fill selected-fill))
           (%paste :input-text (funcall (autofill-fill selected-fill)))))))

(defmethod nyxt:on-signal-notify-uri ((mode web-mode) url)
  (declare (type quri:uri url))
  (unless (or (url-empty-p url)
              (find-if (alex:rcurry #'str:starts-with? (object-string url))
                       (history-blacklist mode)))
    (htree:add-child (make-instance 'buffer-description
                                    :url url
                                    :title (title (buffer mode)))
                     (history mode)
                     :test #'equals)

    (history-add url :title (title (buffer mode))))

  (match (session-store-function *browser*)
    ((guard f f) (funcall-safely f)))
  url)

(defmethod nyxt:on-signal-notify-title ((mode web-mode) title)
  ;; Title may be updated after the URI, so we need to set the history entry again
  ;; with `on-signal-notify-uri'.
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod nyxt:on-signal-load-committed ((mode web-mode) url)
  (declare (ignore mode url))
  nil)

(defmethod nyxt:on-signal-load-finished ((mode web-mode) url)
  ;; TODO: Setting the default zoom level works with pure Javascript, but it
  ;; can only be done after the URL has been loaded which is a bit of a
  ;; kludge.  Instead we could add an FFI endpoint,
  ;; e.g. webkit_web_view_set_zoom_level.
  (unzoom-page :buffer (buffer mode)
               :ratio (current-zoom-ratio (buffer mode)))
  url)

(defmethod nyxt:object-string ((node htree:node))
  (object-string (when node (htree:data node))))
(defmethod nyxt:object-display ((node htree:node))
  (object-display (when node (htree:data node))))
