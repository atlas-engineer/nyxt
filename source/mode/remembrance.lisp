;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Find better name for "cached pages".

;; TODO: Compress content using montezuma :stored :compress?  Looks like
;; `montezuma::compress' is a no-op.

;; TODO: Note that field access can be programmatic too:
;; `montezuma::field-name', `montezuma::all-fields', `montezuma:field-data'.
;; Not needed though?

;; TODO: Diff and validation?
;; TODO: Add action / command to cache bookmarks / history entries manually.

(nyxt:define-package :nyxt/remembrance-mode
    (:documentation "Mode to cache visited content to disk.
The textual content can be searched and displayed."))
(in-package :nyxt/remembrance-mode)

(define-class cache-path (files:cache-file files:virtual-file nyxt-file)
  ((files:base-path #p"remembrance.cache")
   (files:name "remembrance"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-mode remembrance-mode ()
  "Cache the textual content of visited pages.
The caching can be done automatically or manually, see `auto-cache-on-load-p'."
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
    ;; TODO: 0 to disable?
    :documentation "Update entry if its `last-update' is older than this value.
Set to 0 to disable.")
   ;; (auto-update-p
   ;;  t
   ;;  :type boolean
   ;;  :documentation "Automatically refresh the cache when entry is older than `update-interval'.")
   (discard-interval
    #.(* 30 60 60 24)
    :type alex:non-negative-integer
    :documentation "Discard cached entry if its `last-update' is older than this.")
   ;; TODO: `style' belongs to a separate mode, doesn't it?  Same question as with `history-tree-mode'.
   (style (theme:themed-css (theme *browser*)
            (* :margin 0
               :padding 0
               :list-style "none")))))

(defmethod cache-size ((mode remembrance-mode))
  (length (all-cache-entries (cache mode))))

(defmethod initialize-instance :after ((mode remembrance-mode) &key)
  (setf (cache mode)
        (make-instance 'montezuma:index
                       :path (files:expand (cache-path mode))
                       ;; Unless otherwise specified, queries will search all
                       ;; these fields simultaneously.
                       :default-field "*")))

(defun make-term (field value)
  (make-instance 'montezuma:term-query
                 :term (montezuma:make-term field value)))

(defun make-term* (field value)
  (make-instance 'montezuma::wildcard-query ; TODO: Not external!
                 :term (montezuma:make-term field value)))

(defun search-cache (cache query)
  "Return the list of articles for QUERY."
  (mapcar (alex:curry #'montezuma:get-document cache)
          (mapcar #'montezuma:doc
                  (montezuma:each
                   (montezuma:search cache
                                     query
                                     ;; TODO: Is there a way to remove the limit?
                                     :num-docs 10000)
                   #'identity))))

(defun all-cache-entries (cache)
  (search-cache cache (make-instance 'montezuma:match-all-query)))

(defun find-url (url remembrance-mode)
  "Return entry matching URL"
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (montezuma:add-query query (make-term "url" (render-url url)) :should-occur)
    (first (search-cache (cache remembrance-mode) query))))

(defun lookup (input remembrance-mode
               &key suffix-matching-p)
  "Return entry matching document."
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (mapc (lambda (term)
            (dolist (field '("title" "content" "url" "keywords"))
              (montezuma:add-query query (make-term* field term) :should-occur)))
          (mapcar (sera:op (str:concat (when suffix-matching-p "*")
                                       _ "*"))
                  (mapcar #'string-downcase (sera:words input))))
    (search-cache (cache remembrance-mode) query)))

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

(defun page-last-update (page-doc)
  (alex:when-let ((last-update (montezuma:document-value page-doc "last-update")))
    (local-time:parse-timestring last-update)))

(defun page-keywords (page-doc)
  (safe-document-value page-doc "keywords"))

(defun buffer-content (buffer)
  ;; TODO: Use cl-readability for better results.
  (ps-eval :buffer buffer
    (ps:@ document body |innerText|)))

(defun timestamp->string (timestamp)
  (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+))

(defun out-of-date-p (page mode)
  (< (discard-interval mode)
     (local-time:timestamp-difference (local-time:now)
                                      (page-last-update page))))

(defun out-of-date-pages (mode)
  (sera:filter (rcurry #'out-of-date-p mode) (all-cache-entries (cache mode))))

(defun delete-cached-pages (urls &optional (mode (find-submode 'remembrance-mode)))
  (dolist (url (uiop:ensure-list urls) )
    (montezuma:query-delete (cache mode)
                            (make-term "url" (render-url url))))
  ;; Without optimization (or flush?), the deletion is not persisted to disk
  ;; until next addition.
  (montezuma:optimize (cache mode)))

(defun delete-out-of-date-pages (&optional (mode (find-submode 'remembrance-mode)))
  (delete-cached-pages (out-of-date-pages mode) mode))

(defun buffer->cache (buffer remembrance-mode)
  "BUFFER is indexed by URL.
It's only cached if its `last-update' is older than `update-interval'.

Return cached page.
Return NIL if URL is not cached, for instance if it's on
`nyxt/history-mode:history-mode' `history-blocklist'."
  (let* ((url (sera:lret ((copy (quri:copy-uri (url buffer))))
                ;; We drop the fragment as it does not change the page content.
                (setf (quri:uri-fragment copy) nil)))
         (page (find-url url remembrance-mode)))
    (let ((history-mode (find-submode 'nyxt/history-mode:history-mode)))
      (unless (or (internal-url-p url)
                  (and history-mode
                       (nyxt/history-mode:blocked-p url history-mode)))
        (prog1
            (if (or (not page)
                    (< (update-interval remembrance-mode)
                       (local-time:timestamp-difference (local-time:now) (page-last-update page))))
                (let ((doc (make-instance 'montezuma:document)))
                  (flet ((add-field (field value &rest options)
                           (montezuma:add-field doc
                                                (apply #'montezuma:make-field field value options))))
                    (add-field "url" (render-url url) :index :untokenized)
                    (add-field "title" (title buffer))
                    (add-field "content" (buffer-content buffer))
                    (add-field "last-update"
                               (timestamp->string (local-time:now))
                               :index :untokenized)
                    (add-field "keywords"
                               (format nil "~:{~a~^ ~}" (nyxt::keywords buffer))))
                  (montezuma:add-document-to-index (cache remembrance-mode) doc)
                  doc)
                page)
          (delete-out-of-date-pages remembrance-mode))))))

(define-class remembrance-source (prompter:source)
  ((prompter:name "Pages")
   (prompter:constructor (all-cache-entries (cache (find-submode 'remembrance-mode))))
   (prompter:filter nil)
   (prompter:filter-preprocessor (lambda (suggestions source input)
                                   (if (uiop:emptyp input)
                                       suggestions
                                       (lookup input (find-submode 'remembrance-mode)
                                               :suffix-matching-p (suffix-matching-p source)))))
   (prompter:multi-selection-p t)
   (prompter:active-attributes-keys '("URL" "Title" "Keywords"))
   (suffix-matching-p
    t
    :type boolean
    :documentation "Whether to match suffixes in queries.
For instance 'nana' will match 'banana'.
This induces a performance cost."))
  (:export-class-name-p t))

(define-class remembrance-exact-source (remembrance-source)
  ((prompter:name "Exact page matches")
   (prompter:constructor (all-cache-entries (cache (find-submode 'remembrance-mode))))
   (prompter:filter (lambda (suggestion source input)
                      (declare (ignore source))
                      (when (search input (montezuma:document-value
                                           (prompter:value suggestion) "content")
                                    :test #'string-equal)
                        suggestion)))
   (prompter:filter-preprocessor nil)
   (prompter:multi-selection-p t)
   (prompter:active-attributes-keys '("URL" "Title" "Keywords")))
  (:export-class-name-p t))

(defmethod prompter:object-attributes ((doc montezuma:document) (source remembrance-source))
  (declare (ignore source))
  `(("URL" ,(page-url-string doc))
    ("Title" ,(page-title doc))
    ("Keywords" ,(page-keywords doc))))

(define-internal-page view-cached-page (&key url-string query) ; TODO: `view-remembered-page'?
    (:title "*Cached page*")
  "View textual content of cached page in new buffer."
  (enable-modes 'remembrance-mode (current-buffer))
  (let* ((mode (find-submode 'remembrance-mode))
         (doc (find-url url-string mode))
         (content (page-content doc)))
    (dolist (term (cons query (uiop:split-string query)))
      (setf content (ppcre:regex-replace-all (uiop:strcat "(?i)" term)
                                             content
                                             (spinneret:with-html-string (:b term))
                                             ;; REVIEW: https://github.com/edicl/cl-ppcre/issues/51
                                             ;; Only works for whole-word matches.
                                             :preserve-case t)))
    (spinneret:with-html-string
      (:style (style mode))
      (:h1 (format nil "Cached content of ~a" url-string))
      (:div (:pre (:raw content))))))

(define-command recollect-visited-page
    (&key (return-actions (list (lambda-command buffer-load* (suggestion-values)
                                  "Load first selected cache entry in current buffer and the rest in new buffer(s)."
                                  (mapc (lambda (page-doc) (make-buffer :url (page-url page-doc))) (rest suggestion-values))
                                  (buffer-load (page-url (first suggestion-values))))
                                (lambda-command new-buffer-load (suggestion-values)
                                  "Load cache entries in new buffer(s)."
                                  (mapc (lambda (page-doc) (make-buffer :url (page-url page-doc))) (rest suggestion-values))
                                  (make-buffer-focus :url (page-url (first suggestion-values))))
                                (lambda-command copy-url* (suggestions)
                                  "Copy bookmark URL."
                                  (let ((url (page-url-string (first suggestions))))
                                    (trivial-clipboard:text url)
                                    (echo "Copied to clipboard: ~s" url)))
                                (lambda-command view-cached-content (suggestions)
                                  "View content in new buffer."
                                  (let ((query (prompter:input (current-prompt-buffer))))
                                    (set-current-buffer
                                     (buffer-load (nyxt-url 'view-cached-page
                                                            :url-string (render-url (page-url-string (first suggestions)))
                                                            :query query)
                                                  :buffer (ensure-internal-page-buffer 'buffer-history-tree))))))))
  "Search the local cache for URL, title and content."
  (prompt
   :prompt "Search cache"
   :input (ffi-buffer-copy (current-buffer))
   :sources (list (make-instance 'remembrance-source :return-actions return-actions)
                  (make-instance 'remembrance-exact-source :return-actions return-actions))))

(define-command remember-buffer (&key buffer)
  "Cache the BUFFER URL, title and textual content.
BUFFER can be a list of buffers."
  (let ((buffers (or (alex:ensure-list buffer)
                     (prompt :prompt "Cache content of buffers"
                             :sources (make-instance 'buffer-source
                                                     :return-actions '())))))
    (dolist (buffer buffers)
      (buffer->cache buffer (find-submode 'remembrance-mode)))))

(defun url->cache (url)
  (unless (internal-url-p url)
    (let ((background-buffer (make-instance 'background-buffer))
          (promise (lpara:promise)))
      ;; TODO: Use nhooks helper macro instead.
      (hooks:once-on (buffer-loaded-hook background-buffer) (_)
        (lpara:fulfill promise))
      (buffer-load url :buffer background-buffer)
      (lpara:force promise)

      (enable-modes 'remembrance-mode background-buffer)
      (buffer->cache background-buffer (find-submode 'remembrance-mode background-buffer))
      (nyxt::buffer-delete background-buffer))))

(define-command remember-bookmark (&key bookmark)
  "Cache the BOOKMARK URL, title and textual content.
BOOKMARK can be a list of bookmarks."
  (let ((bookmarks (or (alex:ensure-list bookmark)
                       (prompt
                        :prompt "Cache content of bookmarks"
                        :sources (make-instance 'nyxt/bookmark-mode:bookmark-source
                                                :multi-selection-p t)))))
    (mapc (compose #'url->cache #'url) bookmarks)))

(define-command remember-history-entry ()
  "Cache the queried history entries URL, title and textual content."
  (let ((history-nodes (prompt :prompt "Cache history entries"
                               :sources (make-instance 'nyxt/history-mode:history-all-source
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
