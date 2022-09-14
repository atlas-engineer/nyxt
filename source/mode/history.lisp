;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/history-mode
    (:documentation "Mode to store current buffer navigation into the global history."))
(in-package :nyxt/history-mode)

(define-mode history-mode ()
  "Mode to manage navigation history."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (history-blocklist '("https://duckduckgo.com/l/")
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
                      :type (list-of string)
                      :documentation "URL prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the DuckDuckGo
search.")
   (conservative-history-movement-p
    nil
    :type boolean
    :documentation "Whether history navigation is restricted by buffer-local history.")
   (keyscheme-map
    (define-keyscheme-map "history-mode" ()
      keyscheme:default
      (list
       "M-left" 'history-backwards
       ;; this should be C-[
       "M-[" 'history-backwards
       "button8" 'history-backwards
       "M-right" 'history-forwards
       ;; this should be C-]
       "M-]" 'history-forwards
       "button9" 'history-forwards
       "M-shift-left" 'history-backwards-query
       "M-shift-right" 'history-forwards-query
       "C-M-right" 'history-forwards-all-query
       "C-M-left" 'history-all-query
       "C-shift-h" 'history-all-query
       "C-shift-H" 'history-all-query)
      keyscheme:emacs
      (list
       "C-b" 'history-backwards
       "M-b" 'history-backwards-query
       "M-f" 'history-forwards-query
       "C-M-f" 'history-forwards-all-query
       "C-f" 'history-forwards-maybe-query
       "C-M-b" 'history-all-query)
      keyscheme:vi-normal
      (list
       "H" 'history-backwards
       "M-h" 'history-backwards-query
       "M-l" 'history-forwards-query
       "M-L" 'history-forwards-all-query
       "L" 'history-forwards-maybe-query
       "M-H" 'history-all-query)))))

(defun load-history-url (url-or-node
                         &key (buffer (current-buffer))
                              (message "History entry is already the current URL."))
  "Go to HISTORY-NODE's URL."
  (let ((url (if (typep url-or-node 'htree:node)
                 (url (htree:data url-or-node))
                 url-or-node)))
    (cond
      ((not (quri:uri-p url))
       (funcall
        (if nyxt::*run-from-repl-p*
            'error
            'echo-warning )
        "Not a URL nor a history node: ~a" url-or-node))
      ((quri:uri= url (url buffer))
       (echo message))
      (t (buffer-load url)))))

(defmacro with-history ((history-sym buffer) &body body)
  "Run body if BUFFER has history entries, that is, if it owns some nodes.
This does not save the history to disk."
  `(let ((,history-sym (buffer-history (or ,buffer (current-buffer)))))
     (if (and ,history-sym
              (or (not ,buffer)
                  (htree:owner ,history-sym (id ,buffer))))
         (progn ,@body)
         (echo "Buffer ~a has no history." (id ,buffer)))))

(defmacro with-history-access ((history-sym buffer) &body body)
  "Run body if BUFFER has history entries, that is, if it owns some nodes.
This saves the history to disk when BODY exits."
  `(files:with-file-content (,history-sym (history-file (or ,buffer (current-buffer))))
     (if (and ,history-sym
              (or (not ,buffer)
                  (htree:owner ,history-sym (id ,buffer))))
         (progn ,@body)
         (echo "Buffer ~a has no history." (id ,buffer)))))

(define-command history-backwards (&optional (buffer (current-buffer)))
  "Go to parent URL in history."
  (alex:when-let ((new-node
                   (with-history-access (history buffer)
                     (cond
                       ;; Skip going backwards if the current URL is not in history. If
                       ;; it's not in history, then going backwards means canceling the
                       ;; current load operation.
                       ((and (htree:owner-node history (id buffer))
                             (not (quri:uri= (url buffer)
                                             (url (htree:data (htree:owner-node history (id buffer)))))))
                        nil)
                       ((conservative-history-movement-p (find-submode 'history-mode buffer))
                        (htree:backward-owned-parents history (id buffer)))
                       (t
                        (htree:backward history (id buffer))))
                     (htree:owner-node history (id buffer)))))
    (load-history-url new-node
                      :message "No backward history.")))

(define-command history-forwards (&optional (buffer (current-buffer)))
  "Go to forward URL in history."
  (let ((new-node
          (with-history-access (history buffer)
            (htree:forward history (id buffer))
            (htree:owner-node history (id buffer)))))
    (load-history-url new-node
                      :message "No forward history.")))

(define-class history-backwards-source (prompter:source)
  ((prompter:name "Parent URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (let ((owner (htree:owner history (id (buffer source)))))
          (if (conservative-history-movement-p (find-submode 'history-mode (buffer source)))
              (htree:all-contiguous-owned-parents history owner)
              (htree:all-parents history :owner owner)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((node history-tree:node) (source prompter:source))
  (declare (ignore source))
  (let ((entry (htree:data (history-tree:entry node))))
    `(("URL" ,(render-url (url entry)))
      ("Title" ,(title entry)))))

(define-command history-backwards-query (&optional (buffer (current-buffer)))
  "Query parent URL to navigate back to."
  (let ((input (prompt1
                :prompt "Navigate backwards to"
                :sources (make-instance 'history-backwards-source
                                        :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (loop until (eq input (htree:owner-node history (id buffer)))
              do (htree:backward history (id buffer))))
      (load-history-url input))))

(define-class direct-history-forwards-source (prompter:source)
  ((prompter:name "First child of all forward-branches")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (if (conservative-history-movement-p (find-submode 'history-mode (buffer source)))
            (htree:owned-children (htree:owner history (id (buffer source))))
            (htree:children (htree:owner-node history (id (buffer source)))))))))
  (:documentation "Direct children of the current history node.")
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-forwards-direct-children (&optional (buffer (current-buffer)))
  "Query child URL to navigate to."
  (let ((input (prompt1
                :prompt "Navigate forwards to"
                :sources (make-instance 'direct-history-forwards-source
                                        :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:go-to-child (htree:data input) history (id buffer)))
      (load-history-url input))))

(define-command history-forwards-maybe-query (&optional (buffer (current-buffer)))
  "If current node has multiple children, query which one to navigate to.
Otherwise go forward to the only child."
  (with-history (history buffer)
    (if (<= 2 (length
               (if (conservative-history-movement-p (find-submode 'history-mode buffer))
                   (htree:owned-children (htree:owner history (id buffer)))
                   (htree:children (htree:owner-node history (id buffer))))))
        (history-forwards-direct-children)
        (history-forwards))))

(define-class history-forwards-source (prompter:source)
  ((prompter:name "All children URLs of the current forward-branch")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (htree:all-forward-children history (id (buffer source)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-forwards-query (&optional (buffer (current-buffer)))
  "Query forward-URL to navigate to."
  (let ((input (prompt1 :prompt "Navigate forwards to"
                        :sources (make-instance 'history-forwards-source
                                                :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        ;; REVIEW: Alternatively, we could use the COUNT argument with
        ;; (1+ (position input (htree:all-forward-children history (id buffer))))
        ;; Same with `history-backwards-query'.
        (loop until (eq input (htree:owner-node history (id buffer)))
              do (htree:forward history (id buffer))))
      (load-history-url input))))

(define-class all-history-forwards-source (prompter:source)
  ((prompter:name "Child URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (let ((owner (htree:owner history (id (buffer source)))))
          (if (conservative-history-movement-p (find-submode 'history-mode (buffer source)))
              (htree:all-contiguous-owned-children history owner)
              (htree:all-children history :owner owner)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-forwards-all-query (&optional (buffer (current-buffer)))
  "Query URL to forward to, from all child branches."
  (let ((input (prompt1 :prompt "Navigate forwards to (all branches)"
                        :sources (make-instance 'all-history-forwards-source
                                                :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:visit-all history (id buffer) input))
      (load-history-url input))))

(define-class history-all-source (prompter:source)
  ((prompter:name "All history URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (funcall (if (conservative-history-movement-p (find-submode 'history-mode (buffer source)))
                     #'htree:all-owner-nodes
                     #'htree:all-branch-nodes)
                 history
                 (htree:owner history (id (buffer source))))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-all-query (&optional (buffer (current-buffer)))
  "Query URL to go to, from the whole history."
  (let ((input (prompt1 :prompt "Navigate to"
                        :sources (make-instance 'history-all-source :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:visit-all history (id buffer) input))
      (load-history-url input))))

(defun title-or-fallback (history-entry)
  "Return HISTORY-ENTRY title or, if empty, the URL."
  (let ((title (title history-entry)))
    (if (str:emptyp title)
        (render-url (url history-entry))
        title)))

(defun render-buffer-history-tree (buffer)
  "Return the HTML presentation of BUFFER history.
Clicking on a link navigates the history in the corresponding buffer."
  (with-history (history buffer)
    (let ((current-buffer-id (id (current-buffer))))
      (spinneret:with-html-string
        (:ul (:raw (or (htree:map-owned-tree
                        #'(lambda (node)
                            (spinneret:with-html-string
                              (:li
                               (:button :class "link"
                                        :title (render-url (url (htree:data node)))
                                        :onclick (ps:ps (nyxt/ps:lisp-eval
                                                         (:title "visit-history-for-buffer")
                                                         (let ((url (url (htree:data node))))
                                                           (with-current-buffer buffer
                                                             (with-history (history buffer)
                                                               (htree:visit-all history (id buffer) node))
                                                             (load-history-url url))
                                                           (switch-buffer :buffer buffer))))
                                        (let ((title (title-or-fallback (htree:data node))))
                                          (cond
                                            ((eq node (htree:owner-node history current-buffer-id))
                                             (:i (:b title)))
                                            ((eq node (htree:owner-node history (id buffer)))
                                             (:i title))
                                            ((htree:owned-p (htree:owner history current-buffer-id) node)
                                             (:b title))
                                            (t title)))))))
                        history
                        (htree:owner history (id buffer))
                        :include-root t
                        :collect-function #'(lambda (a b) (str:concat a (when b
                                                                          (spinneret:with-html-string
                                                                            (:ul (:raw (str:join "" b))))))))
                       "")))))))

(define-internal-page buffer-history-tree (&key (id (id (current-buffer))))
    (:title "*History Tree*" :page-mode 'nyxt/history-tree-mode:history-tree-mode)
  "Display the history tree of a buffer.
ID is a buffer `id'."
  (let ((buffer (nyxt::buffers-get id))
        (mode (find-submode 'nyxt/history-tree-mode:history-tree-mode)))
    (spinneret:with-html-string
      (:style (style mode))
      (:h1 (format nil "History of ~a" buffer))
      (:div (if buffer
                (:raw (render-buffer-history-tree buffer))
                "Buffer no longer exists.")))))

(define-command-global buffer-history-tree (&key (buffer (current-buffer)))
  "Display the history tree of a buffer."
  (set-current-buffer
   (buffer-load (nyxt-url 'buffer-history-tree :id (id buffer))
                :buffer (ensure-internal-page-buffer 'buffer-history-tree))))

(define-internal-page-command-global history-tree ()
  (output-buffer "*History*" 'nyxt/history-tree-mode:history-tree-mode)
  "Display the whole, global history tree.
It displays one branch per buffer.

As such, when nodes are shared by multiple buffers, they are displayed multiple
times.

Thus it is not representative of how the Global History Tree deduplicates nodes
internally, but this display is clearer and more navigable."
  (let ((mode (find-submode 'nyxt/history-tree-mode:history-tree-mode output-buffer)))
    (spinneret:with-html-string
      (:body (:h1 "History")
             (:style (style output-buffer))
             (:style (:raw (style mode)))
             (dolist (buffer (buffer-list))
               (:div (:raw (render-buffer-history-tree buffer))))))))

(define-internal-page-command-global list-history (&key (limit 100))
  (buffer "*History list*" 'nyxt/list-history-mode:list-history-mode) ; TODO: Remove list-history-mode if we add a style slot to `internal-page'.
  "Print the user history as a list."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (style (find-submode 'nyxt/list-history-mode:list-history-mode buffer)))
    (:h1 "History")
    (:p (format nil "The last ~a history entries:" limit))
    (:ul (:raw (nyxt::history-html-list :limit limit)))))

(defun add-url-to-history (url buffer mode)
  (unless (or (url-empty-p url)
              (find-if (rcurry #'str:starts-with? (render-url url))
                       (history-blocklist mode)))
    (log:debug "Notify URL ~a for buffer ~a with load status ~a"
               url
               buffer
               (slot-value buffer 'nyxt::status))
    (unless (quri:uri= url (url buffer))
      (log:debug "Picking different buffer URL instead: ~a"
                 (url buffer)))
    (when (eq (slot-value buffer 'nyxt::status) :finished)
      ;; We also add history entries here when URL changes without initiating
      ;; any load, e.g. when clicking on an anchor.
      (with-current-buffer buffer
        ;; WARNING: We add buffer's URL instead of the URL argument because in
        ;; case of some redirects (like Wikipedia's) the buffer URL is the final
        ;; one.
        (nyxt::history-add (url buffer) :title (title buffer)
                                        :buffer buffer)))
    url))

(defmethod nyxt:on-signal-notify-uri ((mode history-mode) url)
  (declare (type quri:uri url))
  (let ((buffer (buffer mode)))
    (when (web-buffer-p buffer)
      (when (eq :finished (slot-value buffer 'nyxt::status))
        (add-url-to-history url buffer mode))))
  url)

(defmethod nyxt:on-signal-notify-title ((mode history-mode) title)
  ;; Title may be updated after the URL, so we need to set the history entry again
  ;; with `on-signal-notify-uri'.
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url)
  (add-url-to-history url (buffer mode) mode)
  url)
