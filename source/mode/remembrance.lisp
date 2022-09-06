;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Find better name for "cached pages".

;; TODO: Compress content using montezuma :stored :compress?  Looks like
;; `montezuma::compress' is a no-op.

;; TODO: Note that field access can be programmatic too:
;; `montezuma::field-name', `montezuma::all-fields', `montezuma:field-data'.
;; Not needed though?

;; TODO: Add support for automatic tags?
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

(define-class cache-entry ()            ; TODO: Do we need this?
  ((url
    (quri:uri "")
    :type quri:uri)
   (title
    ""
    :type string)
   (content
    ""
    :type string)
   (last-update
    (local-time:now)
    :type local-time:timestamp
    :documentation "See `remembrance-mode' `update-interval'."))
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
    :documentation "Discard entry if its `last-update' is older than this.")
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
  (search-cache cache "*"))

(defun find-url (url remembrance-mode)
  "Return entry matching URL"
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (montezuma:add-query query (make-term* "url" (render-url url)) :should-occur)
    (first (search-cache (cache remembrance-mode) query))))

(defun lookup (input remembrance-mode
               &key suffix-matching-p)
  "Return entry matching document."
  ;; TODO: What to return?  montezuma's doc or our own objects?
  ;; For prompt-buffer specialization, our own would be better.
  (let ((query (make-instance 'montezuma:boolean-query)))
    (mapc (lambda (term)
            (dolist (field '("title" "content" "url"))
              (montezuma:add-query query (make-term* field term) :should-occur)))
          (mapcar (sera:op (str:concat (when suffix-matching-p "*")
                                       _ "*"))
                  (mapcar #'string-downcase (sera:words input))))
    (search-cache (cache remembrance-mode) query)))

;; We define functions to protect ourselves from typing the string keys manually
;; (and risking typos).
(defun page-url-string (page-doc)
  (montezuma:document-value page-doc "url"))

(defun page-url (page-doc)
  (quri:uri (page-url-string page-doc)))

(defun page-title (page-doc)
  (montezuma:document-value page-doc "title"))

(defun page-content (page-doc)
  (montezuma:document-value page-doc "content"))

(defun page-last-update (page-doc)
  (local-time:parse-timestring
   (montezuma:document-value page-doc "last-update")))

(defun buffer-content (buffer)
  ;; TODO: Use cl-readability for better results.
  (ps-eval :buffer buffer
    (ps:@ document body |innerText|)))

(defun timestamp->string (timestamp)
  (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+))

(defun buffer->cache (buffer remembrance-mode)
  "BUFFER is indexed by URL.
It's only cached if its `last-update' is older than `update-interval'."
  (let* ((url (sera:lret ((copy (quri:copy-uri (url buffer))))
                ;; We drop the fragment as it does not change the page content.
                (setf (quri:uri-fragment copy) nil)))
         (page (find-url url remembrance-mode)))
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
                       :index :untokenized))
          (montezuma:add-document-to-index (cache remembrance-mode) doc)
          doc)
        page)))

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
   (prompter:active-attributes-keys '("URL" "Title"))
   (suffix-matching-p
    t
    :type boolean
    :documentation "Whether to match suffixes in queries.
For instance 'nana' will match 'banana'.
This induces a performance cost."))
  (:export-class-name-p t))

(defmethod prompter:object-attributes ((doc montezuma:document) (source remembrance-source))
  (declare (ignore source))
  `(("URL" ,(page-url-string doc))
    ("Title" ,(page-title doc))))

(define-internal-page view-cached-page (&key url-string) ; TODO: `view-remembered-page'?
    (:title "*Cached page*")
  "View textual content of cached page in new buffer."
  (enable-modes 'remembrance-mode (current-buffer))
  (let* ((mode (find-submode 'remembrance-mode))
         (doc (find-url url-string mode)))
    (spinneret:with-html-string
      (:style (style mode))
      (:h1 (format nil "Cached content of ~a" url-string))
      (:div (:pre (page-content doc))))))

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
                                  (set-current-buffer
                                   (buffer-load (nyxt-url 'view-cached-page :url-string (render-url (page-url-string (first suggestions))))
                                                :buffer (ensure-internal-page-buffer 'buffer-history-tree)))))))
  "Search the local cache for URL, title and content."
  (prompt
   :prompt "Search cache"
   :sources (make-instance 'remembrance-source
                           :return-actions return-actions)))

(define-command remember-current-buffer ()
  "Cache the current buffer URL, title and textual content."
  ;; TODO: Make this more generic, get it to work on the GHT / buffer list.
  (buffer->cache (current-buffer) (find-submode 'remembrance-mode)))

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
