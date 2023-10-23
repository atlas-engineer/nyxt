;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Find better name for "cached pages".

;; TODO: Compress content using montezuma :stored :compress?  Looks like
;; `montezuma::compress' is a no-op.

;; TODO: Note that field can be accessed programmatically too:
;; `montezuma::field-name', `montezuma::all-fields', `montezuma:field-data'.
;; Not needed though?

;; TODO: Add tests for garbage collection, caching history, exact matches.

(nyxt:define-package :nyxt/mode/remembrance
  (:documentation "Package for `remembrance-mode' to cache visited content to disk.
The textual content can be searched and displayed.

It leverages `montezuma' as a full-text-search powered database."))
(in-package :nyxt/mode/remembrance)

;; We superclass with `files:read-only-file' instead of `files:virtual-file'
;; because the latter does not expand to a path on disk.
(define-class cache-path (files:cache-file nyxt-file files:read-only-file)
  ((files:base-path #p"remembrance.cache")
   (files:name "remembrance"))
  (:export-class-name-p t))

(defmethod deserialize :around ((profile nyxt-profile) (file cache-path) stream &key)
  ;; Must be an `:around' method to overrule other possible `:around' specialization.
  (declare (ignore stream))
  nil)

(defmethod read-file ((profile nyxt-profile) (file cache-path) &key)
  "Don't load anything for cache, Montezuma is in charge."
  nil)

(define-mode remembrance-mode ()
  "Cache the textual content of visited pages.

Commands include:
- `remember-buffer': Manually cache the page of a buffer.
- `toggle-auto-cache': Cache on load.
- `recollect-visited-page': Search the cache for some content.
- `view-page-changes': Show the differences between a buffer and its cache.

Options include:
- `auto-cache-on-load-p': Whether the caching should be done automatically or manually."
  ((visible-in-status-p nil)
   (cache-path
    (make-instance 'cache-path)
    :type cache-path
    :documentation "Cache path where page content is saved.
See also `cache'.")
   (cache
    nil
    :type (maybe montezuma:index)
    :export nil
    :documentation "`cache' is separate from `cache-path' because it is managed by `montezuma'.")
   (auto-cache-on-load-p
    t
    :type boolean
    :documentation "Whether to automatically cache the page content on load.")
   (update-interval
    #.(* 60 60 24)
    :type alex:non-negative-integer
    ;; TODO: 0 to disable?  Or use separate slot, e.g. `auto-update-p'?
    :documentation "Update entry if its `last-update' is older than this value.
Set to 0 to disable.")
   (discard-interval
    #.(* 30 60 60 24)
    :type alex:non-negative-integer
    :documentation "Discard cached entry if its `last-update' is older than this.")
   ;; TODO: `style' belongs to a separate mode, doesn't it?  Same question as with `history-tree-mode'.
   (style (theme:themed-css (theme *browser*)
            `(* :margin 0
                :padding 0
                :list-style "none")))))

(defmethod initialize-instance :after ((mode remembrance-mode) &key)
  (let ((path (files:expand (cache-path mode))))
    (log:info "Remembrance cache at ~s" path)
    (setf (cache mode)
          (make-instance 'montezuma:index
                         :path path
                         ;; Unless otherwise specified, queries will search all
                         ;; these fields simultaneously.
                         :default-field "*"))))

(defmethod cache-size ((mode remembrance-mode))
  (length (all-cache-entries mode)))

(defun make-term (field value)
  (make-instance 'montezuma:term-query
                 :term (montezuma:make-term field value)))

(defun make-term* (field value)
  (make-instance 'montezuma::wildcard-query ; TODO: Not external!
                 :term (montezuma:make-term field value)))

(defmethod search-cache ((mode remembrance-mode) query)
  "Return the entries matching QUERY."
  (let ((cache (cache mode)))
    (mapcar (curry #'montezuma:get-document cache)
            (mapcar #'montezuma:doc
                    (montezuma:each
                     (montezuma:search cache
                                       query
                                       ;; TODO: Is there a way to remove the limit?
                                       :num-docs 10000)
                     #'identity)))))

(defmethod all-cache-entries ((mode remembrance-mode))
  "Return all entries in cache."
  (search-cache mode (make-instance 'montezuma:match-all-query)))

(defun find-url (url remembrance-mode)
  "Return cache entry matching URL"
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (montezuma:add-query query (make-term "url" (render-url url)) :should-occur)
    (first (search-cache remembrance-mode query))))

(defun lookup (input remembrance-mode
               &key suffix-matching-p)
  "Return cache entries matching INPUT."
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (mapc (lambda (term)
            (dolist (field '("title" "content" "url" "keywords"))
              (montezuma:add-query query (make-term* field term) :should-occur)))
          (mapcar (sera:op (str:concat (when suffix-matching-p "*")
                                       _ "*"))
                  (mapcar #'string-downcase (sera:words input))))
    (search-cache remembrance-mode query)))

(defun safe-document-value (doc field)
  "Like `montezuma:document-value' but return NIL when field is missing."
  (when (montezuma:document-field doc field)
    (montezuma:document-value doc field)))

;; We define functions to protect ourselves from typing the string keys manually
;; (and risking typos).
(defun page-url-string (page-doc)
  (safe-document-value page-doc "url"))

(defun page-url (page-doc)
  (alex:when-let ((url-string (page-url-string page-doc)))
    (quri:uri url-string )))

(defun page-title (page-doc)
  (safe-document-value page-doc "title"))

(defun page-content (page-doc)
  (safe-document-value page-doc "content"))

(defun page-html-content (page-doc)
  (safe-document-value page-doc "html-content"))

(defun page-last-update (page-doc)
  (alex:when-let ((last-update (montezuma:document-value page-doc "last-update")))
    (local-time:parse-timestring last-update)))

(defun page-keywords (page-doc)
  (safe-document-value page-doc "keywords"))

(defun buffer-content (buffer)
  ;; TODO: Use cl-readability for better results.
  (ps-eval :buffer buffer
    (ps:@ document body |innerText|)))

(defun buffer-html-content (buffer)
  (ps-eval :buffer buffer
    (ps:@ document body |innerHTML|)))

(defun timestamp->string (timestamp)
  (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+))

(defun out-of-date-p (page mode)
  (< (discard-interval mode)
     (local-time:timestamp-difference (local-time:now)
                                      (page-last-update page))))

(defun out-of-date-pages (mode)
  (sera:filter (rcurry #'out-of-date-p mode) (all-cache-entries mode)))

(defun delete-cached-pages (urls &optional (mode (find-submode 'remembrance-mode)))
  (dolist (url (uiop:ensure-list urls))
    (montezuma:query-delete (cache mode)
                            (make-term "url" (render-url url))))
  ;; Without optimization (or flush?), the deletion is not persisted to disk
  ;; until next addition.
  (montezuma:optimize (cache mode)))

(defun delete-out-of-date-pages (&optional (mode (find-submode 'remembrance-mode)))
  (delete-cached-pages (out-of-date-pages mode) mode))

(defun buffer->cache (buffer remembrance-mode &key force)
  "BUFFER is indexed by URL.
It's only cached if its `last-update' is older than `update-interval'.

Cached pages older than `discard-interval' are automatically purged.

Return cached page.
Return NIL if URL is not cached, for instance if it's on
`nyxt/mode/history:history-mode' `history-blocklist'.
Return NIL if page content is empty."
  ;; We drop the fragment as it does not change the page content.
  (let* ((url (quri:copy-uri (url buffer) :fragment nil))
         (page (find-url url remembrance-mode)))
    (let ((history-mode (find-submode 'nyxt/mode/history:history-mode)))
      (unless (or (internal-url-p url)
                  (and history-mode
                       (nyxt/mode/history:blocked-p url history-mode)))
        (prog1
            (if (or force
                    (not page)
                    (< (update-interval remembrance-mode)
                       (local-time:timestamp-difference (local-time:now) (page-last-update page))))
                (let ((content (buffer-content buffer))
                      (doc (make-instance 'montezuma:document)))
                  (unless (uiop:emptyp content)
                    (when page
                      (delete-cached-pages url remembrance-mode))
                    (flet ((add-field (field value &rest options)
                             (montezuma:add-field doc
                                                  (apply #'montezuma:make-field field value options))))
                      (add-field "url" (render-url url) :index :untokenized)
                      (add-field "title" (title buffer))
                      (add-field "content" (buffer-content buffer))
                      (add-field "html-content" (buffer-html-content buffer)
                                 :index :untokenized)
                      (add-field "last-update"
                                 (timestamp->string (local-time:now))
                                 :index :untokenized)
                      (add-field "keywords"
                                 (format nil "~:{~a~^ ~}" (nyxt::keywords buffer))))
                    (montezuma:add-document-to-index (cache remembrance-mode) doc)
                    doc))
                page)
          (delete-out-of-date-pages remembrance-mode))))))

(define-class remembrance-source (prompter:source)
  ((prompter:name "Pages")
   (prompter:constructor (all-cache-entries (find-submode 'remembrance-mode)))
   (prompter:filter nil)
   (prompter:filter-preprocessor (lambda (suggestions source input)
                                   (if (uiop:emptyp input)
                                       suggestions
                                       (lookup input (find-submode 'remembrance-mode)
                                               :suffix-matching-p (suffix-matching-p source)))))
   (prompter:enable-marks-p t)
   (prompter:active-attributes-keys
    '("URL" "Title" "Keywords")
    :accessor nil)
   (suffix-matching-p
    t
    :type boolean
    :documentation "Whether to match suffixes in queries.
For instance 'nana' will match 'banana'.
This induces a performance cost."))
  (:export-class-name-p t)
  (:documentation "The source for all the remembrance cache entries."))

(define-class remembrance-exact-source (remembrance-source)
  ((prompter:name "Exact page matches")
   (prompter:constructor (all-cache-entries (find-submode 'remembrance-mode)))
   (prompter:filter (lambda (suggestion source input)
                      (declare (ignore source))
                      (when (search input (montezuma:document-value
                                           (prompter:value suggestion) "content")
                                    :test #'string-equal)
                        suggestion)))
   (prompter:filter-preprocessor nil)
   (prompter:enable-marks-p t)
   (prompter:active-attributes-keys
    '("URL" "Title" "Keywords")
    :accessor nil))
  (:export-class-name-p t)
  (:documentation "The source for the cache entries that contain the exact input text."))

(defmethod prompter:object-attributes ((doc montezuma:document) (source remembrance-source))
  (declare (ignore source))
  `(("URL" ,(page-url-string doc) nil 3)
    ("Title" ,(page-title doc) nil 2)
    ("Keywords" ,(page-keywords doc))))

(defun add-search-mark (buffer term)
  ;; TODO: Add this to the  `nyxt/mode/search-buffer' API?
  (unless (uiop:emptyp term)
    (with-current-buffer buffer
      (nyxt/mode/search-buffer:search-document
       term
       :buffer buffer
       :node (elt (clss:select "body" (document-model buffer)) 0)
       :test (smart-case-test term)
       :mark-p t))))

(define-internal-scheme "view-remembered-page"
    (lambda (url buffer)
      (let ((url (quri:uri url)))
        (flet ((query-param (url param)
                 (quri:url-decode
                  (alex:assoc-value (quri:uri-query-params url)
                                    param
                                    :test #'string=))))
          (alex:if-let ((origin-buffer (ignore-errors (nyxt::buffers-get (parse-integer (query-param url "origin-buffer-id"))))))
            (let* ((mode (find-submode 'remembrance-mode origin-buffer))
                   (query (query-param url "query"))
                   (url-string (quri:url-decode (quri:uri-path url)))
                   (doc (find-url url-string mode)))
              (hooks:once-on (buffer-loaded-hook buffer) (_)
                (dolist (term (cons query (uiop:split-string query)))
                  (add-search-mark buffer term)))
              (spinneret:with-html-string
                (:nstyle (style mode))
                (:h1 "[Cache] " (:a :href url-string (if (uiop:emptyp (page-title doc))
                                                         url-string
                                                         (page-title doc))))
                (:hr)
                (:div (:raw (page-html-content doc)))))
            (echo-warning "Origin buffer does not exist.")))))
  :no-access-p t)

(define-command recollect-visited-page
    (&key (actions-on-return (list (lambda-command view-remembered-content (suggestions)
                                     "View content in new buffer."
                                     (let ((query (prompter:input (current-prompt-buffer))))
                                       (make-buffer-focus
                                        :url (quri:make-uri
                                              :scheme "view-remembered-page"
                                              :path (quri:url-encode (render-url (page-url-string (first suggestions))))
                                              :query (list (cons "query" (quri:url-encode query))
                                                           (cons "origin-buffer-id"
                                                                 (quri:url-encode (write-to-string (id (current-buffer))))))))))
                                   #'buffer-load*
                                   (lambda-command new-buffer-load (suggestion-values)
                                     "Load cache entries in new buffer(s)."
                                     (mapc (lambda (page-doc) (make-buffer :url (page-url page-doc))) (rest suggestion-values))
                                     (make-buffer-focus :url (page-url (first suggestion-values))))
                                   (lambda-command copy-url* (suggestions)
                                     "Copy bookmark URL."
                                     (let ((url (page-url-string (first suggestions))))
                                       (trivial-clipboard:text url)
                                       (echo "Copied to clipboard: ~s" url))))))
  "Search the local cache for URL, title and content.
It lists multiple suggestion sources:

- One set of suggestions comes from natural language processing of the query;

- Another set is the suggestions that contain an exact match of the whole query,
  as a setence.

The `view-remembered-content' action displays the cached content (with highlighted
query terms), even when offline."
  ;; TODO: Add current suggestion action to preview the search matches.
  (prompt
   :prompt "Search cache"
   :input (ffi-buffer-copy (current-buffer))
   :sources (list (make-instance 'remembrance-source :actions-on-return actions-on-return)
                  (make-instance 'remembrance-exact-source :actions-on-return actions-on-return))))

(define-command toggle-auto-cache (&key (buffer (current-buffer)))
  "Whether to cache on URL load.
See also `auto-cache-on-load-p' in `remembrance-mode'."
  (let ((mode (find-submode 'remembrance-mode buffer)))
    (setf (auto-cache-on-load-p mode)
          (not (auto-cache-on-load-p mode)))
    (if (auto-cache-on-load-p mode)
        (echo "Auto-cache enabled.")
        (echo "Auto-cache disabled."))))

(define-internal-page view-diff (&key diff (id (id (current-buffer))))
    (:title "*Cache diff*")
  "Display HTML DIFF associated to buffer with ID."
  (let* ((buffer (nyxt::buffers-get id))
         (mode (find-submode 'remembrance-mode buffer)))
    (spinneret:with-html-string
      (:nstyle (style mode))
      ;; TODO: Add button to switch to buffer.
      (:h1 "[Cache diff] " (:button :class "link"
                                    :onclick
                                    (ps:ps (nyxt/ps:lisp-eval
                                            (:title "switch-to-buffer")
                                            (switch-buffer :buffer buffer)))
                                    (title buffer)))
      (:h2 (render-url (url buffer)))
      (:hr)
      (:div (:raw diff)))))

(define-command view-page-changes (&key buffer)
  "View BUFFER changes (diff) compared to the last content that was cached."
  (let ((buffer (or buffer
                    (prompt1 :prompt "View changes for buffer"
                             :sources (make-instance 'buffer-source
                                                     :actions-on-return #'identity))))
        (mode (find-submode 'remembrance-mode)))
    (alex:when-let ((doc (find-url (url buffer) mode)))
      ;; TODO: Display in internal page.
      (buffer-load-internal-page-focus 'view-diff
                                       :id (id buffer)
                                       :diff (html-diff:html-diff (page-html-content doc)
                                                                  (buffer-html-content buffer))))))

(define-command remember-buffer (&key buffer)
  "Cache the BUFFER URL, title and textual content.
BUFFER can be a list of buffers."
  (let ((buffers (or (alex:ensure-list buffer)
                     (prompt :prompt "Cache content of buffers"
                             :sources (make-instance 'buffer-source
                                                     :actions-on-return #'identity)))))
    (dolist (buffer buffers)
      (buffer->cache buffer (find-submode 'remembrance-mode)))))

(defun url->cache (url)
  "Like `buffer->cache' but works on URL even if it is not currently loaded in a
buffer."
  (unless (internal-url-p url)
    (let ((background-buffer (make-instance 'background-buffer))
          (promise (lpara:promise)))
      ;; TODO: Use nhooks helper macro instead.
      (hooks:once-on (buffer-loaded-hook background-buffer) (_)
        (lpara:fulfill promise))
      (buffer-load url :buffer background-buffer)
      (lpara:force promise)

      (enable-modes* 'remembrance-mode background-buffer)
      (buffer->cache background-buffer (find-submode 'remembrance-mode background-buffer))
      (nyxt::buffer-delete background-buffer))))

(define-command remember-bookmark (&key bookmark)
  "Cache the BOOKMARK URL, title and textual content.
BOOKMARK can be a list of bookmarks."
  (let ((bookmarks (or (alex:ensure-list bookmark)
                       (prompt
                        :prompt "Cache content of bookmarks"
                        :sources (make-instance 'nyxt/mode/bookmark:bookmark-source
                                                :enable-marks-p t)))))
    (mapc (compose #'url->cache #'url) bookmarks)))

(define-command remember-history-entry ()
  "Cache the queried history entries URL, title and textual content."
  (let ((history-nodes (prompt :prompt "Cache history entries"
                               :sources (make-instance 'nyxt/mode/history:history-all-source
                                                       :buffer (current-buffer)))))
    (mapc (compose #'url->cache #'url) history-nodes)))

(defmethod nyxt:on-signal-notify-uri ((mode remembrance-mode) url)
  (declare (type quri:uri url))
  (when (auto-cache-on-load-p mode)
    (let ((buffer (buffer mode)))
      (when (web-buffer-p buffer)
        (when (eq :finished (slot-value buffer 'nyxt::status))
          (buffer->cache buffer mode)))))
  url)

(defmethod nyxt:on-signal-notify-title ((mode remembrance-mode) title)
  ;; Title may be updated after the URL, so we need to set the entry again
  ;; with `on-signal-notify-uri'.
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod nyxt:on-signal-load-finished ((mode remembrance-mode) url)
  (when (auto-cache-on-load-p mode)
    (buffer->cache (buffer mode) mode))
  url)
