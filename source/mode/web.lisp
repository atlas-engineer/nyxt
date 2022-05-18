;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-mode
  (:use :common-lisp :nyxt)
  (:shadow #:focus-first-input-field)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Mode for web pages"))
(in-package :nyxt/web-mode)
(use-nyxt-package-nicknames)

;; TODO: Remove web-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode web-mode ()
  "Base mode for interacting with documents."
  ((rememberable-p nil)
   (history-blocklist '("https://duckduckgo.com/l/")
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
                      :type list-of-strings
                      :documentation "URL prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the duckduckgo
search.")
   (conservative-history-movement-p
    nil
    :type boolean
    :documentation "Whether history navigation is restricted by buffer-local history.")
   (auto-follow-hints-p
    nil
    :type boolean
    :documentation "Whether the hints are automatically followed when matching user input.")
   (box-style (theme:themed-css (theme *browser*)
                  (".nyxt-hint"
                   :background-color theme:primary
                   :opacity 0.8
                   :color "white"
                   :font-weight "bold"
                   :padding "0px 3px 0px 3px"
                   :border-radius "2px"
                   :z-index #.(1- (expt 2 31))))
              :documentation "The style of the boxes, e.g. link hints.")
   (highlighted-box-style (theme:themed-css (theme *browser*)
                           (".nyxt-hint.nyxt-highlight-hint"
                            :background-color theme:accent
                            :color theme:background))
                          :documentation "The style of highlighted boxes, e.g. link hints.")
   (hints-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   :type string
                   :documentation "The alphabet (charset) to use for hints.
Order matters -- the ones that go first are more likely to appear more often
and to index the top of the page.")
   (hints-selector "a, button, input, textarea, details, select, img:not([alt=\"\"])"
                   :type string
                   :documentation "Defines which elements are to be hinted. The
hints-selector syntax is that of CLSS, and broadly, that of CSS. Use it to
define which elements are picked up by element hinting.")
   (keymap-scheme
    (define-scheme "web"
      scheme:cua
      (list
       "C-M-Z" 'nyxt/passthrough-mode:passthrough-mode
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
       "M-i" 'focus-first-input-field
       "C-j" 'follow-hint
       "C-u C-j" 'follow-hint-new-buffer-focus
       "C-J" 'follow-hint-new-buffer
       "C-M-j" 'follow-hint-nosave-buffer-focus
       "C-u C-M-j" 'follow-hint-nosave-buffer
       "M-c h" 'copy-hint-url
       "C-c" 'copy
       "C-v" 'paste
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "button9" 'history-forwards
       "button8" 'history-backwards
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-hyphen" 'unzoom-page
       "C-0" 'reset-page-zoom
       "C-button4" 'zoom-page
       "C-button5" 'unzoom-page
       "C-M-c" 'open-inspector
       "C-m g" 'bookmark-hint
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-hints
       "C-." 'jump-to-heading
       "M-{" 'previous-heading
       "M-}" 'next-heading
       "end" 'maybe-scroll-to-bottom
       "home" 'maybe-scroll-to-top
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       "C-i" 'autofill
       "C-u C-o" 'edit-with-external-editor
       ;; Leave SPACE and arrow keys unbound so that the renderer decides whether to
       ;; navigate textboxes (arrows), insert or scroll (space).
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
       "C-f" 'history-forwards-maybe-query
       "C-b" 'history-backwards
       "C-g" 'nothing              ; Emacs users may hit C-g out of habit.
       "M-g M-g" 'follow-hint              ; Corresponds to Emacs' `goto-line'.
       "M-g g" 'follow-hint-new-buffer-focus
       "C-u M-g M-g" 'follow-hint-new-buffer
       "C-u M-g g" 'follow-hint-new-buffer
       "C-M-g C-M-g" 'follow-hint-nosave-buffer-focus
       "C-M-g g" 'follow-hint-nosave-buffer
       "C-x C-w" 'copy-hint-url
       "C-y" 'paste
       "M-w" 'copy
       "C-/" 'undo
       "C-?" 'redo ; / shifted on QWERTY
       "C-w" 'cut
       "C-x h" 'select-all
       "button9" 'history-forwards
       "button8" 'history-backwards
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "C-x C-+" 'zoom-page
       "C-x C-=" 'zoom-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'unzoom-page
       "C-x C-0" 'reset-page-zoom
       "C-m g" 'bookmark-hint
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-hints
       "C-." 'jump-to-heading
       "M->" 'scroll-to-bottom
       "M-<" 'scroll-to-top
       "C-v" 'scroll-page-down
       "M-v" 'scroll-page-up
       "C-u C-x C-f" 'edit-with-external-editor)

      scheme:vi-normal
      (list
       "H" 'history-backwards
       "L" 'history-forwards-maybe-query
       "y y" 'copy
       "p" 'paste
       "d d" 'cut
       "u" 'undo
       "C-r" 'redo
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
       "+" 'zoom-page
       "hyphen" 'unzoom-page
       "0" 'reset-page-zoom
       "z i" 'zoom-page
       "z o" 'unzoom-page
       "z z" 'reset-page-zoom
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
       "{" 'previous-heading
       "}" 'next-heading
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

(sera:export-always '%clicked-in-input?)
(defun %clicked-in-input? (&optional (buffer (current-buffer)))
  ;; We don't use define-parenscript because we need to control over which
  ;; buffer we query.
  (nyxt/ffi:buffer-evaluate-javascript buffer
                                  (ps:ps
                                    (ps:chain document active-element
                                              tag-name))))

(sera:export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(defun call-non-input-command-or-forward (command &key (buffer (current-buffer))
                                                       (window (current-window)))
  (let ((response (%clicked-in-input?)))
    (if (input-tag-p response)
        (forward-to-renderer :window window :buffer buffer)
        (funcall command))))

(define-command maybe-scroll-to-bottom (&optional (buffer (current-buffer)))
  "Scroll to bottom if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-bottom :buffer buffer))

(define-command maybe-scroll-to-top (&optional (buffer (current-buffer)))
  "Scroll to top if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-top :buffer buffer))

(define-command go-next ()
  "Navigate to the next element according to the HTML 'rel' attribute."
  (peval (ps:chain (nyxt/ps:qsa document "rel=next") 0 (click))))

(define-command go-previous ()
  "Navigate to the previous element according to the HTML 'rel' attribute."
  (peval (ps:chain (nyxt/ps:qsa document "rel=prev") 0 (click))))

(define-command go-to-homepage ()
  "Navigate to the homepage."
  (let* ((url (url (current-buffer)))
         (authority (quri:uri-authority url))
         (scheme (quri:uri-scheme url)))
    (buffer-load (str:concat scheme "://" authority))))

(define-command go-up ()
  "Navigate to the upper level in the URL path hierarchy."
  (let* ((url (url (current-buffer)))
         (path (quri:uri-path url))
         (path-splited (str:split "/" path :omit-nulls t))
         (new-path-splited (butlast path-splited))
         (scheme (quri:uri-scheme url))
         (authority (quri:uri-authority url))
         (new-path (reduce #'(lambda (x e) (str:concat x e "/"))
                           new-path-splited
                           :initial-value "/")))
    (buffer-load (str:concat scheme "://" authority new-path))))

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
  (let ((new-node
          (with-history-access (history buffer)
            (if (conservative-history-movement-p (find-submode 'web-mode buffer))
                (htree:backward-owned-parents history (id buffer))
                (htree:backward history (id buffer)))
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
          (if (conservative-history-movement-p (find-submode 'nyxt/web-mode:web-mode (buffer source)))
              (htree:all-contiguous-owned-parents history owner)
              (htree:all-parents history :owner owner)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((node history-tree:node))
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
        (if (conservative-history-movement-p (find-submode 'nyxt/web-mode:web-mode (buffer source)))
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
               (if (conservative-history-movement-p (find-submode 'nyxt/web-mode:web-mode buffer))
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
  (let ((input (prompt1
                 :prompt "Navigate forwards to"
                 :sources (list (make-instance 'history-forwards-source
                                               :buffer buffer)))))
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
          (if (conservative-history-movement-p (find-submode 'nyxt/web-mode:web-mode (buffer source)))
              (htree:all-contiguous-owned-children history owner)
              (htree:all-children history :owner owner)))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-forwards-all-query (&optional (buffer (current-buffer)))
  "Query URL to forward to, from all child branches."
  (let ((input (prompt1
                 :prompt "Navigate forwards to (all branches)"
                 :sources (list (make-instance 'all-history-forwards-source
                                               :buffer buffer)))))
    (when input
      (with-history (history buffer)
        (htree:forward history (id buffer)))
      (load-history-url input))))

(define-class history-all-source (prompter:source)
  ((prompter:name "All history URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (funcall (if (conservative-history-movement-p (find-submode 'nyxt/web-mode:web-mode (buffer source)))
                     #'htree:all-owner-nodes
                     #'htree:all-branch-nodes)
                 history
                 (htree:owner history (id (buffer source))))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-all-query (&optional (buffer (current-buffer)))
  "Query URL to go to, from the whole history."
  (let ((input (prompt1
                 :prompt "Navigate to"
                 :sources (list (make-instance 'history-all-source
                                               :buffer buffer)))))
    (when input
      (with-history (history buffer)
        (htree:visit-all history (id buffer) input))
      (load-history-url input))))

(defun title-or-fallback (history-entry)
  "Return HISTORY-ENTRY title or, if empty, the URL."
  (let ((title (title history-entry)))
    (if (str:emptyp title)
        (render-url (url history-entry))
        title)))

(define-internal-page-command-global buffer-history-tree (&key (id (id (current-buffer))))
  (output-buffer (format nil "*History-~a*" id)
                 'nyxt/history-tree-mode:history-tree-mode)
  "Open a new buffer displaying the whole history tree of a buffer."
  (with-history (history (nyxt::buffers-get id))
    (let ((mode (find-submode 'nyxt/history-tree-mode:history-tree-mode output-buffer))
          (tree (spinneret:with-html-string
                  (:ul (:raw (htree:map-owned-tree
                              #'(lambda (node)
                                  (spinneret:with-html-string
                                    (:li
                                     (:a :href (render-url (url (htree:data node)))
                                         (let ((title (title-or-fallback (htree:data node))))
                                           (if (eq node (htree:owner-node history id))
                                               (:b title)
                                               title))))))
                              history
                              (htree:owner history id)
                              :include-root t
                              :collect-function #'(lambda (a b) (str:concat a (when b
                                                                                (spinneret:with-html-string
                                                                                  (:ul (:raw (str:join "" b)))))))))))))
      (when tree
        (spinneret:with-html-string
          (:style (style mode))
          (:div (:raw tree)))))))

;; TODO: Factor this with `buffer-history-tree'.
(define-internal-page-command-global history-tree (&key (current-buffer-id (id (current-buffer))))
    (output-buffer "*History*"
                   'nyxt/history-tree-mode:history-tree-mode)
  "Open a new buffer displaying the whole history branch the current buffer is on."
  (with-history (history (nyxt::buffers-get current-buffer-id))
    (let ((mode (find-submode 'nyxt/history-tree-mode:history-tree-mode output-buffer))
          (tree (spinneret:with-html-string
                  (:ul (:raw (htree:map-tree
                              #'(lambda (node)
                                  (spinneret:with-html-string
                                    (:li (:a :href (render-url (url (htree:data node)))
                                             (let ((title (title-or-fallback (htree:data node))))
                                               (cond
                                                 ((eq node (htree:owner-node history current-buffer-id))
                                                  (:i (:b title)))
                                                 ((htree:owned-p (htree:owner history current-buffer-id) node)
                                                  (:b title))
                                                 (t title))))))) ; Color?  Smaller?
                              history
                              :owner (htree:owner history current-buffer-id)
                              :include-root t
                              :collect-function #'(lambda (a b) (str:concat a (when b
                                                                                (spinneret:with-html-string
                                                                                  (:ul (:raw (str:join "" b)))))))))))))
      (when tree
        (spinneret:with-html-string
          (:body (:h1 "History")
                 (:style (style output-buffer))
                 (:style (:raw (style mode)))
                 (:div (:raw tree))))))))

(define-internal-page-command-global list-history (&key (limit 100))
  (buffer "*History list*" 'nyxt/list-history-mode:list-history-mode)
  "Print the user history as a list."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (style (find-submode 'nyxt/list-history-mode:list-history-mode buffer)))
    (:h1 "History")
    (:ul (:raw (nyxt::history-html-list :limit limit)))))

(define-command paste (&optional (buffer (current-buffer)))
  "Paste from clipboard into active element."
  (nyxt/ffi:buffer-paste buffer))

(define-class ring-source (prompter:source)
  ((prompter:name "Clipboard ring")
   (ring :initarg :ring :accessor ring :initform nil)
   (prompter:constructor
    (lambda (source)
      (containers:container->list (ring source))))
   (prompter:actions
    (list (lambda-command paste* (ring-items)
            (%paste :input-text (first ring-items))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (prompt
   :prompt "Paste from ring"
   :sources (list (make-instance 'ring-source
                                 :ring (nyxt::clipboard-ring *browser*)))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (nyxt/ffi:buffer-copy buffer))

(define-command copy-placeholder ()
  "Copy placeholder text to clipboard."
  (let ((current-value (peval (ps:@ document active-element placeholder))))
    (if (eq current-value :undefined)
        (echo "No active selected placeholder.")
        (progn (copy-to-clipboard current-value)
               (echo "Placeholder copied.")))))

(define-command cut (&optional (buffer (current-buffer)))
  "Cut the selected text in BUFFER."
  (nyxt/ffi:buffer-cut buffer))

(define-command undo (&optional (buffer (current-buffer)))
  "Undo the last editing action."
  (nyxt/ffi:buffer-undo buffer))

(define-command redo (&optional (buffer (current-buffer)))
  "Redo the last editing action."
  (nyxt/ffi:buffer-redo buffer))

(define-command select-all (&optional (buffer (current-buffer)))
  "Select all the text in the text field."
  (nyxt/ffi:buffer-select-all buffer))

(define-class autofill-source (prompter:source)
  ((prompter:name "Autofills")
   (prompter:constructor (autofills *browser*))
   (prompter:actions
    (list (lambda-command autofill* (autofills)
            (let ((selected-fill (first autofills)))
              (cond ((stringp (autofill-fill selected-fill))
                     (%paste :input-text (autofill-fill selected-fill)))
                    ((functionp (autofill-fill selected-fill))
                     (%paste :input-text (funcall (autofill-fill selected-fill))))))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (prompt
   :prompt "Autofill"
   :sources (make-instance 'autofill-source)))

(export-always 'element-focused)
(defgeneric element-focused (mode) ; TODO: Make hook instead?  Or use both, have the default method call hook.
  (:method ((mode t))
    nil)
  (:documentation "Method run when `focus-element' is called."))

(defmacro focus-element ((&optional (buffer '(current-buffer))) &body element-script)
  "Select the element pointed to by ELEMENT-SCRIPT.
ELEMENT-SCRIPT is a Parenscript script that is passed to `ps:ps'."
  (alex:with-gensyms (element)
    (alex:once-only (buffer)
      `(progn
         (nyxt/ffi:buffer-evaluate-javascript ,buffer
                                         (ps:ps (let ((,element (progn ,@element-script)))
                                                  (ps:chain ,element (focus))
                                                  (ps:chain ,element (select)))))
         (dolist (mode (modes ,buffer))
           (element-focused mode))))))

(define-command focus-first-input-field (&key (type-blacklist '("hidden"
                                                                "checkbox"
                                                                "button")))
  "Move the focus to the first input field of `buffer'."
  ;; The list of input types can be found below.
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
  ;; TODO: The following results in 2 DOM traversal.  We should probably do the
  ;; whole thing in a single Parenscript instead.
  (pflet ((nth-input-type (i)
                          (let* ((input (ps:chain document
                                                  (get-elements-by-tag-name "INPUT")))
                                 (item (when input (ps:chain input (item (ps:lisp i))))))
                            (when item
                              (ps:chain item type)))))
    (let ((i (do ((i 0 (1+ i)))
                 ((notany
                   (lambda (type) (equalp (nth-input-type i) type))
                   type-blacklist)
                  i))))
      (focus-element ()
        (let* ((input (ps:chain document
                                (get-elements-by-tag-name "INPUT")))
               (item (when input (ps:chain input (item (ps:lisp i))))))
          (when item
            item))))))

(defun add-url-to-history (url buffer mode)
  (unless (or (url-empty-p url)
              (find-if (alex:rcurry #'str:starts-with? (render-url url))
                       (history-blocklist mode)))
    (log:debug "Notify URL ~a for buffer ~a with load status ~a"
               url
               buffer
               (slot-value buffer 'nyxt::status))
    (when (eq (slot-value buffer 'nyxt::status) :finished)
      ;; We also add history entries here when URL changes without initiating
      ;; any load, e.g. when clicking on an anchor.
      (with-current-buffer buffer
        (nyxt::history-add url :title (title buffer)
                               :buffer buffer)))
    url))

(defmethod nyxt:on-signal-notify-uri ((mode web-mode) url)
  (declare (type quri:uri url))
  (let ((buffer (buffer mode)))
    (when (web-buffer-p buffer)
      (when (eq :finished (slot-value buffer 'nyxt::status))
        (add-url-to-history url buffer mode))))
  url)

(defmethod nyxt:on-signal-notify-title ((mode web-mode) title)
  ;; Title may be updated after the URL, so we need to set the history entry again
  ;; with `on-signal-notify-uri'.
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod nyxt:on-signal-load-committed ((mode web-mode) url)
  (declare (ignore mode url))
  nil)

(defmethod nyxt:on-signal-load-finished ((mode web-mode) url)
  (add-url-to-history url (buffer mode) mode)
  (reset-page-zoom :buffer (buffer mode)
                   :ratio (current-zoom-ratio (buffer mode)))
  url)

;; REVIEW: Shorten the name to e.g., `show-url-qr'? It's no longer current URL only.
(define-internal-page-command-global show-qrcode-of-current-url
    (&key (buffer-id (id (current-buffer)))
     (url (quri:render-uri (url (nyxt::buffers-get buffer-id)))))
    (buffer (format nil "*Buffer ~a (~a) QRcode*" buffer-id url) 'base-mode)
  "In a new buffer, show the QR code containing the URL for the current buffer."
  (let* ((stream (flexi-streams:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:p (:u url))
      (:p (:img :src (str:concat "data:image/png;base64,"
                                 (cl-base64:usb8-array-to-base64-string
                                  (flexi-streams:get-output-stream-sequence stream)))
                :alt url)))))

(define-internal-page-command-global view-source (&key (url (render-url (url (current-buffer)))))
  (source-buffer (format nil "*Source of ~a" url) 'base-mode)
  "View source of the URL (by default current page) in a separate buffer."
  (let ((buffer (or (find (quri:uri url) (buffer-list) :test #'quri:uri= :key #'url)
                    (make-background-buffer :url url))))
    (unwind-protect
         (spinneret:with-html-string
           (:pre (if (web-buffer-p buffer)
                     (plump:serialize (document-model buffer) nil)
                     (nyxt/ffi:buffer-get-document buffer))))
      (when (background-buffer-p buffer)
        (nyxt/ffi:buffer-delete buffer)))))

(pushnew 'web-mode nyxt::%default-modes)
