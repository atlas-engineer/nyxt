;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:documentation "Mode for web pages"))
(in-package :nyxt/web-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; TODO: Remove web-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode web-mode ()
  "Base mode for interacting with documents."
  ((history-blocklist '("https://duckduckgo.com/l/")
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
                      :type list-of-strings
                      :documentation "URI prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the duckduckgo
search.")
   (conservative-history-movement-p
    nil
    :type boolean
    :documentation "Whether history navigation is restricted by buffer-local history.")
   (box-style (cl-css:css
               '((".nyxt-hint"
                  :background "linear-gradient(#fcff9e, #efcc00)"
                  :color "black"
                  :border "1px black solid"
                  :padding "1px 3px 1px 3px"
                  :border-radius "2px"
                  :z-index #.(1- (expt 2 31)))))
              :documentation "The style of the boxes, e.g. link hints.")
   (highlighted-box-style (cl-css:css
                           '((".nyxt-hint.nyxt-highlight-hint"
                              :font-weight "500"
                              :background "#fcff9e")))
                          :documentation "The style of highlighted boxes, e.g. link hints.")
   (hints-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   :type string
                   :documentation "The alphabet (charset) to use for hints.
Order matters -- the ones that go first are more likely to appear more often
and to index the top of the page.")
   (keymap-scheme
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
       "C-M-g" 'follow-hint-nosave-buffer-focus
       "C-u C-M-g" 'follow-hint-nosave-buffer
       "C-x C-w" 'copy-hint-url
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
       ;; Leave SPACE and arrow keys unbound so that the renderer decides whether to
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
       "C-M-g C-M-g" 'follow-hint-nosave-buffer-focus
       "C-M-g g" 'follow-hint-nosave-buffer
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
       "g f" 'follow-hint-nosave-buffer
       "g F" 'follow-hint-nosave-buffer-focus
       "button9" 'history-forwards
       "button8" 'history-backwards
       "+" 'zoom-in-page
       "hyphen" 'zoom-out-page
       "0" 'unzoom-page
       "z i" 'zoom-in-page
       "z o" 'zoom-out-page
       "z z" 'unzoom-page
       "g h" 'jump-to-heading
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
       "pagedown" 'scroll-page-down)))))

(defun current-web-mode ()
  (find-submode (current-buffer) 'web-mode))

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
  (let ((response (%clicked-in-input?)))
    (if (input-tag-p response)
        (ffi-generate-input-event
         window
         (nyxt::last-event buffer))
        (funcall-safely command))))

(define-command paste-or-set-url (&optional (buffer (current-buffer)))
  "Paste text if active element is an input tag, forward event otherwise."
  (let ((response (%clicked-in-input?)))
    (let ((url-empty (url-empty-p (url-at-point buffer))))
      (if (and (input-tag-p response) url-empty)
          (funcall-safely #'paste)
          (unless url-empty
            (make-buffer-focus :url (url-at-point buffer)
                               :nosave-buffer-p (nosave-buffer-p buffer)))))))

(define-command maybe-scroll-to-bottom (&optional (buffer (current-buffer)))
  "Scroll to bottom if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-bottom :buffer buffer))

(define-command maybe-scroll-to-top (&optional (buffer (current-buffer)))
  "Scroll to top if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-top :buffer buffer))

(defun load-history-url (url-or-node
                         &key (buffer (current-buffer))
                           (message "History entry is already the current URL."))
  "Go to HISTORY-NODE's URL."
  (unless (quri:uri-p url-or-node)
    (setf url-or-node (url (htree:data url-or-node))))
  (if (quri:uri= url-or-node (url buffer))
      (echo message)
      (buffer-load url-or-node)))

(define-command history-backwards (&optional (buffer (current-buffer)))
  "Go to parent URL in history."
  (let ((new-node
          (with-data-access (history (history-path buffer))
            (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                (htree:backward-owned-parents history)
                (htree:backward history))
            (htree:current-owner-node history))))
    (load-history-url new-node
                      :message "No backward history.")))

(define-command history-forwards (&optional (buffer (current-buffer)))
  "Go to forward URL in history."
  (let ((new-node
          (with-data-access (history (history-path buffer))
            (htree:forward history)
            (htree:current-owner-node history))))
    (load-history-url new-node
                      :message "No forward history.")))

(defun history-backwards-suggestion-filter (&optional (buffer (current-buffer)))
  "Suggestion function over all parent URLs."
  (with-data-unsafe (parents (history-path buffer)
                     :key (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                              #'htree:all-contiguous-owned-parents
                              #'htree:all-parents))
    (lambda (minibuffer)
      (if parents
          (fuzzy-match (input-buffer minibuffer) parents)
          (echo "Cannot navigate backwards.")))))

(define-command history-backwards-query (&optional (buffer (current-buffer)))
  "Query parent URL to navigate back to."
  (let ((input (prompt-minibuffer
                :input-prompt "Navigate backwards to"
                :suggestion-function (history-backwards-suggestion-filter))))
    (when input
      (with-data-access (history (history-path buffer))
        ;; See `history-forwards-query' comment.
        (loop until (eq input (htree:current-owner-node history))
              do (htree:backward history)))
      (load-history-url input))))

(defun history-forwards-direct-children-suggestion-filter (&optional (buffer (current-buffer)))
  "Suggestion function over forward-children URL."
  (with-data-unsafe (children (history-path buffer)
                     :key (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                              (alex:compose #'htree:owned-children #'htree:current-owner)
                              (alex:compose #'htree:children #'htree:current-owner-node)))
    (lambda (minibuffer)
      (if children
          (fuzzy-match (input-buffer minibuffer) children)
          (echo "Cannot navigate forwards.")))))

(define-command history-forwards-direct-children (&optional (buffer (current-buffer)))
  "Query child URL to navigate to."
  (let ((input (prompt-minibuffer
                :input-prompt "Navigate forwards to"
                :suggestion-function (history-forwards-direct-children-suggestion-filter))))
    (when input
      (with-data-access (history (history-path buffer))
        (htree:go-to-child (htree:data input) history))
      (load-history-url input))))

(define-command history-forwards-maybe-query (&optional (buffer (current-buffer)))
  "If current node has multiple children, query which one to navigate to.
Otherwise go forward to the only child."
  (let ((history (get-data (history-path buffer))))
    (if (<= 2 (length
               (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                   (htree:owned-children (htree:current-owner history))
                   (htree:children (htree:current-owner-node history)))))
        (history-forwards-direct-children)
        (history-forwards))))

(defun history-forwards-suggestion-filter (&optional (buffer (current-buffer)))
  "Suggestion function over forward-children URL."
  (with-data-unsafe (children (history-path buffer)
                     :key #'htree:all-forward-children)
    (lambda (minibuffer)
      (if children
          (fuzzy-match (input-buffer minibuffer) children)
          (echo "Cannot navigate forwards.")))))

(define-command history-forwards-query (&optional (buffer (current-buffer)))
  "Query forward-URL to navigate to."
  (let ((input (prompt-minibuffer
                :input-prompt "Navigate forwards to"
                :suggestion-function (history-forwards-suggestion-filter))))
    (when input
      (with-data-access (history (history-path buffer))
        ;; REVIEW: Alternatively, we could use the COUNT argument with
        ;; (1+ (position input (htree:all-forward-children history)))
        ;; Same with `history-backwards-query'.
        (loop until (eq input (htree:current-owner-node history))
                    do (htree:forward history)))
      (load-history-url input))))

(defun history-forwards-all-suggestion-filter (&optional (buffer (current-buffer)))
  "Suggestion function over children URL from all branches."
  (with-data-unsafe (children (history-path buffer)
                     :key (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                              (alex:compose #'htree:all-contiguous-owned-children #'htree:current-owner)
                              #'htree:all-children))
    (lambda (minibuffer)
      (if children
          (fuzzy-match (input-buffer minibuffer) children)
          (echo "Cannot navigate forwards.")))))

(define-command history-forwards-all-query (&optional (buffer (current-buffer)))
  "Query URL to forward to, from all child branches."
  (let ((input (prompt-minibuffer
                :input-prompt "Navigate forwards to (all branches)"
                :suggestion-function (history-forwards-all-suggestion-filter))))
    (when input
      (with-data-access (history (history-path buffer))
        (htree:forward history))
      (load-history-url input))))

(defun history-all-suggestion-filter (&optional (buffer (current-buffer)))
  "Suggestion function over all history URLs."
  (with-data-unsafe (urls (history-path buffer)
                     :key (if (conservative-history-movement-p (find-mode buffer 'web-mode))
                              #'htree:all-current-owner-nodes
                              #'htree:all-current-branch-nodes))
    (lambda (minibuffer)
      (if urls
          (fuzzy-match (input-buffer minibuffer) urls)
          (echo "No history.")))))

(define-command history-all-query (&optional (buffer (current-buffer)))
  "Query URL to go to, from the whole history."
  (let ((input (prompt-minibuffer
                :input-prompt "Navigate to"
                :suggestion-function (history-all-suggestion-filter))))
    (when input
      (with-data-access (history (history-path buffer))
        (htree:visit-all history input))
      (load-history-url input))))

(define-command buffer-history-tree (&optional (buffer (current-buffer)))
  "Open a new buffer displaying the whole history tree of a buffer."
  (with-current-html-buffer (output-buffer (format nil "*History-~a*" (id buffer))
                                                 'nyxt/history-tree-mode:history-tree-mode)
    (with-data-unsafe (history (history-path buffer))
      (let* ((markup:*auto-escape* nil)
             (mode (find-submode output-buffer 'nyxt/history-tree-mode:history-tree-mode))
             (tree `(:ul ,(htree:map-owned-tree
                           #'(lambda (node)
                               `(:li
                                 (:a :href ,(object-string (url (htree:data node)))
                                     ,(let ((title (or (match (title (htree:data node))
                                                         ((guard e (not (str:emptyp e))) e))
                                                       (object-display (url (htree:data node))))))
                                        (if (eq node (htree:current-owner-node history))
                                            `(:b ,title)
                                            title)))))
                           history
                           :include-root t
                           :collect-function #'(lambda (a b) `(,@a ,(when b `(:ul ,@b))))))))
        (markup:markup
         (:body (:h1 "History")
                (:style (style output-buffer))
                (:style (style mode))
                (:div (markup:raw
                       (markup:markup*
                        tree)))))))))

(define-command history-tree ()         ; TODO: Factor this with `buffer-history-tree'.
  "Open a new buffer displaying the whole history tree."
  (nyxt::with-current-html-buffer (output-buffer "*History*"
                                                 'nyxt/history-tree-mode:history-tree-mode)
    (with-data-unsafe (history (let ((dummy-buffer (make-buffer)))
                                 (prog1
                                     (history-path dummy-buffer)
                                   (delete-buffer :id (id dummy-buffer)))))
      (let ((markup:*auto-escape* nil)
            (mode (find-submode output-buffer 'nyxt/history-tree-mode:history-tree-mode))
            (tree `(:ul ,(htree:map-tree
                          #'(lambda (node)
                              `(:li (:a :href ,(object-string (url (htree:data node)))
                                        ,(let ((title (or (match (title (htree:data node))
                                                            ((guard e (not (str:emptyp e))) e))
                                                          (object-display (url (htree:data node))))))
                                           (cond
                                             ((eq node (htree:current-owner-node history))
                                              `(:i (:b ,title)))
                                             ((htree:owned-p (htree:current-owner history) node)
                                              `(:b ,title))
                                             (t title)))))) ; Color?  Smaller?
                          history
                          :include-root t
                          :collect-function #'(lambda (a b) `(,@a ,(when b `(:ul ,@b))))))))
        (markup:markup
         (:body (:h1 "History")
                (:style (style output-buffer))
                (:style (style mode))
                (:div (markup:raw
                       (markup:markup*
                        tree)))))))))

(define-command list-history (&key (limit 100))
  "Print the user history as a list."
  (with-current-html-buffer (buffer "*History list*" 'nyxt/list-history-mode:list-history-mode)
    (markup:markup
     (:style (style buffer))
     (:style (cl-css:css
              '((a
                 :color "black")
                ("a:hover"
                 :color "gray"))))
     (:h1 "History")
     (:ul (nyxt::history-html-list :limit limit)))))

(define-command paste ()
  "Paste from clipboard into active-element."
  ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
  ;; copy something from the context menu 'Copy' action, upon pasting we will
  ;; retrieve the text from the GTK thread.  This is prone to create
  ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
  ;;
  ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
  ;; is present the clipboard and need not be retrieved from the GTK thread.
  ;; TODO: Do we still need to flush now that we have multiple threads?
  ;; (trivial-clipboard:text (trivial-clipboard:text))
  (%paste))

(defun ring-suggestion-filter (ring)
  (let ((ring-items (containers:container->list ring)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) ring-items))))

(define-command paste-from-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (let ((ring-item (prompt-minibuffer
                    :suggestion-function (ring-suggestion-filter
                                          (nyxt::clipboard-ring *browser*)))))
    (%paste :input-text ring-item)))

(define-command copy ()
  "Copy selected text to clipboard."
  (let ((input (%copy)))
    (copy-to-clipboard input)
    (echo "Text copied.")))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (let ((selected-fill (prompt-minibuffer
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
                       (history-blocklist mode)))
    (log:debug "Notify URI ~a for buffer ~a with load status ~a"
               url
               (buffer mode)
               (slot-value (buffer mode) 'nyxt::load-status))
    ;; TODO: This dirty hack prevents a middle-click from adding URL to the
    ;; history of the parent buffer.  Find a more reliable way to store URLs in
    ;; history.
    (unless (eq (slot-value (buffer mode) 'nyxt::load-status) :unloaded)
      (with-current-buffer (buffer mode)
        (nyxt::history-add url :title (title (buffer mode))
                               :buffer (buffer mode)))))

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
  (unzoom-page :buffer (buffer mode)
               :ratio (current-zoom-ratio (buffer mode)))
  url)

(defmethod nyxt:object-string ((node htree:node))
  (object-string (when node (htree:data node))))
(defmethod nyxt:object-display ((node htree:node))
  (object-display (when node (htree:data node))))
