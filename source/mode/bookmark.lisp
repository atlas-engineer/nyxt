;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/bookmark
  (:documentation "Package for `bookmark-mode', mode to manage bookmarks.
The main object is `bookmark-entry'. The main function to add a bookmark is
`bookmark-add'.

See the `bookmark-mode' for the external user-facing APIs."))
(in-package :nyxt/mode/bookmark)

;;; We don't use CL-prevalence to serialize / deserialize bookmarks for a couple for reasons:
;;; - It's too verbose, e.g. a list is
;;; (:SEQUENCE 3 :CLASS CL:LIST :SIZE 2 :ELEMENTS ( "bar" "baz" ) )
;;;
;;; - We lack control on the line breaks.
;;;
;;; - It needs IDs for every object, which makes it hard for the user to
;;;   hand-edit the file without breaking it.
;;;
;;; - Un-explicitly-set class slots are exported if they have an initform;
;;;   removing the initform forces us to put lots of (slot-boundp ...).

(export-always 'bookmark-mode)
(define-mode bookmark-mode ()
  "Manage bookmarks.
Bookmarks can be persisted to disk, see the `bookmarks-file' mode slot.

See `nyxt/mode/bookmark' package documentation for implementation details and
internal programming APIs."
  ((visible-in-status-p nil)
   (bookmarks-file
    (make-instance 'bookmarks-file)
    :type bookmarks-file
    :documentation "File where bookmarks are saved.")
   (keyscheme-map
    (define-keyscheme-map "bookmarks-mode" ()
      keyscheme:default
      (list
       "C-b" 'list-bookmarks
       "C-m g" 'bookmark-hint)
      keyscheme:cua
      (list
       "C-m o" 'set-url-from-bookmark
       "C-m s" 'bookmark-current-url
       "C-d" 'bookmark-current-url
       "C-m C-s" 'bookmark-buffer-url
       "C-m l" 'bookmark-url
       "C-m k" 'delete-bookmark)
      keyscheme:emacs
      (list
       "C-x r j" 'set-url-from-bookmark
       "C-x r M" 'bookmark-current-url
       "C-x r m" 'bookmark-buffer-url
       "C-x r l" 'bookmark-url
       "C-x r k" 'delete-bookmark)
      keyscheme:vi-normal
      (list
       "m l" 'list-bookmarks
       "m f" 'bookmark-hint
       "m o" 'set-url-from-bookmark
       "m M" 'bookmark-current-url
       "m m" 'bookmark-buffer-url
       "m u" 'bookmark-url
       "m d" 'delete-bookmark)))
   (style (theme:themed-css (theme *browser*)
            `("dl"
              :margin-left "8px")
            ;; Taken from buffer.lisp to save space for big bookmark lists.
            `(button
              :background-color ,theme:secondary
              :color ,theme:on-secondary
              :display "inline-block"
              :text-decoration "none"
              :border-radius "2px"
              :padding "6px"
              :margin-left "2px"
              :margin-right "2px")))))

(define-configuration context-buffer
  ((default-modes (cons 'bookmark-mode %slot-value%))))

(defmethod bookmarks-file ((buffer buffer))
  (bookmarks-file (find-submode 'bookmark-mode buffer)))

(defun group-bookmarks (buffer)
  (let ((bookmarks-table (make-hash-table :test #'equalp))
        (bookmarks (files:content (bookmarks-file buffer))))
    (dolist (bookmark bookmarks)
      (let ((tags (tags bookmark)))
        (if tags
            (dolist (tag tags)
              (push bookmark (gethash tag bookmarks-table nil)))
            (push bookmark (gethash tags bookmarks-table nil)))))
    bookmarks-table))

(define-class bookmarks-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"bookmarks")
   (files:name "bookmarks"))
  (:export-class-name-p t))

(define-class bookmark-entry ()
  ((url (quri:uri ""))
   (title "")
   (annotation "")
   (date (time:now))
   (tags
    '()
    :type (list-of string)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Represents Nyxt bookmark.
`url' is the identity of the `bookmark-entry', used in `equals'.
`title', `annotation', `date', `tags' are useful pieces of metadata."))

(defmethod prompter:object-attributes ((entry bookmark-entry) (source prompter:source))
  (declare (ignore source))
  `(("URL" ,(render-url (url entry)) nil 3)
    ("Title" ,(title entry) nil 2)
    ("Tags" ,(format nil "~{~a ~}" (tags entry)))
    ("Date" ,(local-time:format-timestring nil (date entry) :format local-time:+asctime-format+))))

;; FIXME: Move somewhere else, where Nyxt-global function declarations belong?
(define-generic equals (object1 object2)
  "Nyxt-specific equality function."
  (:method ((e1 bookmark-entry) (e2 bookmark-entry))
    "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
    (url-equal (url e1) (url e2)))
  (:export-generic-name-p t))

(-> bookmark-add
    (quri:uri &key (:title string) (:date (or time:timestamp null)) (:tags t))
    t)
(export-always 'bookmark-add)
(defun bookmark-add (url &key date title tags)
  "Store the bookmark for URL in `bookmarks-file' of the current buffer.
Creates a `bookmark-entry' with DATE, TITLE, and TAGS, when provided.
If there's a bookmarks with the same URL, update the TITLE, TAGS, and DATE
instead."
  (files:with-file-content (bookmarks (bookmarks-file (current-buffer)))
    (unless (or (url-empty-p url)
                (string= "about:blank" (render-url url)))
      (multiple-value-bind (entries bookmarks-without-url)
          (sera:partition (sera:partial #'url-equal url) bookmarks :key #'url)
        (let ((entry (if entries
                         (first entries)
                         (make 'bookmark-entry (url) nil))))
          (unless (str:emptyp title)
            (setf (title entry) title))
          (setf (tags entry)
                (sort (delete-duplicates
                       (remove "" tags :test #'string=)
                       :test #'string=)
                      #'string<))
          (when date
            (setf (date entry) date))
          (push entry bookmarks-without-url)
          (setf bookmarks bookmarks-without-url))))))

(define-class bookmark-source (prompter:source)
  ((prompter:name "Bookmarks")
   (prompter:constructor (files:content (bookmarks-file (current-buffer))))
   (prompter:enable-marks-p t)
   (prompter:active-attributes-keys
    '("URL" "Title" "Tags")
    :accessor nil))
  (:export-class-name-p t)
  (:documentation "Source for bookmark search.
By default, matches URL, title, and tags of the bookmark, but can also match
against date, given `prompter:active-attributes-keys' configuration."))

(defmethod url-sources ((mode bookmark-mode) actions-on-return)
  (make 'bookmark-source (actions-on-return) nil))

(defun tag-suggestions ()
  (let ((bookmarks (files:content (bookmarks-file (current-buffer)))))
    ;; Warning: `sort' is destructive and `append' does not copy the last list,
    ;; so if we used `delete-duplicates' here it would have modified the last
    ;; list.
    (let ((tags (sort (remove-duplicates
                       (mappend #'tags bookmarks)
                       :test #'string-equal)
                      #'string-lessp)))
      tags)))

(define-class tag-source (prompter:source)
  ((prompter:name "Tags")
   (prompter:filter-preprocessor
    (lambda (initial-suggestions-copy source input)
      (prompter:delete-inexact-matches
       initial-suggestions-copy
       source
       (last-word input))))
   (prompter:filter
    (lambda (suggestion source input)
      (prompter:fuzzy-match suggestion source (last-word input))))
   (prompter:enable-marks-p t)
   (prompter:constructor (tag-suggestions))))

(define-panel-command-global bookmarks-panel ()
    (panel-buffer "*Bookmarks panel*")
  "Shows all the bookmarks in a compact panel-buffer layout."
  (let ((bookmarks (group-bookmarks (current-buffer))))
    ;; Simplified version of list-bookmarks
    (spinneret:with-html-string
      (:body
       (:h1 "Bookmarks")
       (if (zerop (hash-table-count bookmarks))
           (:p (format nil "No bookmarks in ~s."
                       (files:expand (files:content (bookmarks-file (current-buffer))))))
           (maphash (lambda (tag bookmarks)
                      (:nsection
                        :title (or tag "Unsorted")
                        :id (or tag "unsorted")
                        :open-p t
                        (dolist (bookmark bookmarks)
                          (:dl (:dt (:a :href (render-url (url bookmark))
                                        :target "_blank"
                                        (title bookmark)))
                               (when (tags bookmark)
                                 (:dd (format nil "Tags: ~{~a~^, ~}" (tags bookmark)))))
                          (:hr))))
                    bookmarks))))))

(export-always 'url-bookmark-tags)
(defun url-bookmark-tags (url)
  "Return the list of tags of the bookmark corresponding to URL."
  (let ((bookmarks (files:content (bookmarks-file (current-buffer)))))
    (alex:when-let ((existing (find url bookmarks :key #'url :test #'url-equal)))
      (tags existing))))

(define-command bookmark-current-url (&optional (buffer (current-buffer)))
  "Bookmark the URL of the current BUFFER."
  (if (url-empty-p (url buffer))
      (echo "Buffer has no URL.")
      (let ((tags (prompt
                   :prompt (format nil "Tag(s) for ~a " (render-url (url buffer)))
                   :sources (list
                             (make-instance 'prompter:word-source
                                            :name "New tags"
                                            ;; On no input, suggest the empty tag which effectively acts as "no tag".
                                            ;; Without it, we would be forced to specify a tag.
                                            :filter-postprocessor
                                            (lambda (suggestions source input)
                                              (declare (ignore source input))
                                              (or suggestions
                                                  (list "")))
                                            :enable-marks-p t)
                             (make 'keyword-source (buffer) nil)
                             (make-instance 'tag-source
                                            :marks (url-bookmark-tags (url buffer)))))))
        (bookmark-add (url buffer)
                      :title (title buffer)
                      :tags tags)
        (echo "Bookmarked ~a." (render-url (url buffer))))))

(define-command bookmark-buffer-url ()
  "Bookmark the page(s) currently opened in the existing buffers."
  (prompt
   :prompt "Bookmark URL from buffer(s)"
   :sources (make-instance 'buffer-source
                           :enable-marks-p t
                           :actions-on-return (lambda-mapped-command bookmark-current-url))))

(define-command bookmark-url
    (&key (url (prompt1
                :prompt "Bookmark URL"
                :sources (list
                          (make-instance 'new-url-or-search-source
                                         :actions-on-return (lambda-mapped-command url))
                          (make-instance 'buffer-source
                                         :actions-on-return (lambda-mapped-command url))
                          (make-instance 'global-history-source
                                         :actions-on-return (lambda-mapped-command url))
                          (make-instance 'bookmark-source
                                         :actions-on-return (lambda-mapped-command url))))))
  "Prompt for a URL to bookmark."
  (if (not (valid-url-p url))
      (echo "Invalid URL '~a'" url)
      (let* ((url (quri:uri url))
             (title (fetch-url-title url))
             (tags (prompt
                    :prompt "Tag(s)"
                    :sources (list
                              (make-instance 'prompter:word-source
                                             :name "New tags"
                                             :enable-marks-p t)
                              (make-instance 'tag-source
                                             :marks (url-bookmark-tags url))))))
        (bookmark-add url :tags tags :title title))))

(define-command delete-bookmark (&optional urls-or-bookmark-entries)
  "Delete bookmark(s) matching the chosen URLS-OR-BOOKMARK-ENTRIES.
URLS-OR-BOOKMARK-ENTRIES could be a list or a single URL/`bookmark-entry'."
  (if urls-or-bookmark-entries
      (files:with-file-content (bookmarks (bookmarks-file (current-buffer)))
        (setf bookmarks
              (set-difference
               bookmarks
               (mapcar (lambda (url)
                         (if (bookmark-entry-p url)
                             url
                             (make-instance 'bookmark-entry :url (quri:uri url))))
                       (uiop:ensure-list urls-or-bookmark-entries))
               :test #'equals)))
      (let ((entries (prompt
                      :prompt "Delete bookmark(s)"
                      :sources (make-instance 'bookmark-source
                                              :enable-marks-p t))))
        (delete-bookmark entries))))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark.
With marks, open the first bookmark in the current buffer, and the rest in other
buffers in the background."
  (prompt
   :prompt "Open bookmark(s)"
   :sources (make-instance
             'bookmark-source
             :actions-on-return
             (list #'buffer-load*
                   (lambda-command new-buffer-load (suggestion-values)
                     "Load bookmark(s) in new buffer(s)."
                     (mapc (lambda (url) (make-buffer :url (url url))) (rest suggestion-values))
                     (make-buffer-focus :url (url (first suggestion-values))))
                   (lambda-command copy-url* (suggestions)
                     "Copy bookmark URL."
                     (trivial-clipboard:text (render-url (url (first suggestions)))))
                   'delete-bookmark))))

(export-always 'list-bookmarks)
(define-internal-page-command-global list-bookmarks ()
    (bookmarks-buffer "*Bookmarks*")
  "List all bookmarks in a new buffer.
Splits bookmarks into groups by tags."
  (let ((bookmarks (group-bookmarks bookmarks-buffer)))
    (spinneret:with-html-string
      (:nstyle (style (find-submode 'bookmark-mode (current-buffer)))) ; TODO: Make sure this is the right buffer
      (:h1 "Bookmarks")
      (cond
        ((zerop (hash-table-count bookmarks))
         (:p (format nil "No bookmarks in ~s." (files:expand (bookmarks-file bookmarks-buffer)))))
        (t (maphash
            (lambda (tag bookmarks)
              (:nsection
                :title (or tag "Unsorted")
                :id (or tag "unsorted")
                :open-p nil
                (dolist (bookmark bookmarks)
                  (let ((url-href (render-url (url bookmark))))
                    (lisp-url-flet bookmarks-buffer
                        ((delbkm (&key href)
                           (delete-bookmark href)))
                      (:div :class "bookmark-entry"
                            (:dl
                             (:dt
                              (:button :onclick
                                       (ps:ps
                                         (let ((section (ps:chain (nyxt/ps:active-element document)
                                                                  (closest ".bookmark-entry"))))
                                           (ps:chain section parent-node (remove-child section)))
                                         (nyxt/ps:lisp-call delbkm
                                                            :buffer bookmarks-buffer
                                                            :args (:href url-href)))
                                       "✕")
                              (:a :href url-href (title bookmark)))
                             (when (tags bookmark)
                               (:dd (format nil "Tags: ~{~a~^, ~}" (tags bookmark)))))
                            (:hr)))))))
            bookmarks))))))

(defmethod serialize-object ((entry bookmark-entry) stream)
  (unless (url-empty-p (url entry))
    (flet ((write-slot (slot)
             (let ((entry-slot (funcall slot entry)))
               (unless (str:emptyp entry-slot)
                 (format t " :~a ~s"
                         (str:downcase slot)
                         entry-slot)))))
      (let ((*standard-output* stream))
        (write-string "(:url ")
        (format t "~s" (render-url (url entry)))
        (write-slot 'title)
        (write-slot 'annotation)
        (when (date entry)
          (write-string " :date ")
          ;; If we don't force the timezone, the timestamp could be serialized
          ;; differently depending on the local timezone, e.g.
          ;;     2020-12-10T11:46:02.500515+01:00
          ;; instead of
          ;;     2020-12-10T10:46:02.500515Z
          (format t "~s" (time:format-timestring nil (date entry)
                                                       :timezone time:+utc-zone+)))
        (when (tags entry)
          (write-string " :tags (")
          (format t "~s" (first (tags entry)))
          (dolist (tag (rest (tags entry)))
            (write-string " ")
            (write tag))
          (write-string ")"))
        (write-string ")")))))

(defmethod files:serialize ((profile nyxt-profile) (file bookmarks-file) stream &key)
  (let ((content
         ;; Sort the entries to make serialization reproducible.
         ;; Particularly useful when bookmarks are under version control.
         ;;
         ;; Need non-destructive sort here or else the cached version
         ;; of file content may become corrupted. For example, with
         ;; destructive SORT almost all bookmarks are removed when the
         ;; user tries to remove just few.
         (sera:sort-new (files:content file)
                        #'url< :key #'url)))
    (write-string "(" stream)
    (sera:do-each (entry content)
      (write-string +newline+ stream)
      (serialize-object entry stream))
    (format stream "~%)~%")
    (echo "Saved ~a bookmarks to ~s."
          (length content)
          (files:expand file))))

(defmethod files:deserialize ((profile nyxt-profile) (path bookmarks-file) raw-content &key)
  (let ((*package* (find-package :nyxt))
        (entries (safe-read raw-content)))
    (mapcar (lambda (entry)
              (when (getf entry :url)
                (setf (getf entry :url)
                      (quri:uri (getf entry :url))))
              (when (getf entry :date)
                (setf (getf entry :date)
                      (time:parse-timestring (getf entry :date))))
              (apply #'make-instance 'bookmark-entry
                     entry))
            entries)))

(define-command import-bookmarks-from-html
    (&key (html-file (prompt1
                       ;; TODO: Is there a more intuitive directory for bookmarks?
                       :input (uiop:native-namestring (uiop:getcwd))
                       :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                       :sources (make-instance
                                 'nyxt/mode/file-manager:file-source
                                 :extensions '("html")))))
  "Import bookmarks from an HTML-FILE with bookmarks from other browsers."
  (if (and (uiop:file-exists-p html-file)
           (equal (pathname-type html-file) "html"))
      (with-open-file (in-html html-file :external-format :utf-8)
        (let ((a-tags (plump:get-elements-by-tag-name (plump:parse in-html) "a")))
          (dolist (a-tag a-tags)
            (let* ((url (plump:attribute a-tag "href"))
                   (title (plump:render-text a-tag))
                   (date (plump:attribute a-tag "add_date"))
                   (tags (plump:attribute a-tag "tags"))
                   (url-uri (quri:uri url)))
              (when (str:starts-with? "http" (quri:uri-scheme url-uri))
                (bookmark-add url-uri
                              :title title
                              :date (ignore-errors (time:unix-to-timestamp (parse-integer date)))
                              :tags (when tags
                                      (str:split "," tags))))))))
      (echo "The file doesn't exist or is not an HTML file.")))

(define-command bookmark-hint ()
  "Prompt for element hints and bookmark them."
  (nyxt/mode/hint:query-hints
   "Bookmark hint"
   (lambda (result)
     (dolist (url (mapcar #'url result))
       (let ((tags (prompt
                    :prompt "Tag(s)"
                    :sources (list
                              (make-instance 'prompter:word-source
                                             :name "New tags"
                                             :enable-marks-p t)
                              (make-instance 'tag-source
                                             :marks (nyxt/mode/bookmark:url-bookmark-tags url))))))
         (nyxt/mode/bookmark:bookmark-add url :tags tags :title (fetch-url-title url)))))
   :selector "a"))
