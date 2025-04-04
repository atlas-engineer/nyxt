;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/bookmark
  (:documentation "Package for `bookmark-mode', mode to manage bookmarks.
The main object is `bookmark-entry'. The main function to add a bookmark is
`persist-bookmark'.

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
       "C-m s" 'add-bookmark
       "C-d" 'add-bookmark
       "C-m k" 'delete-bookmark)
      keyscheme:emacs
      (list
       "C-x r j" 'set-url-from-bookmark
       "C-x r m" 'add-bookmark
       "C-x r l" 'add-bookmark
       "C-x r k" 'delete-bookmark)
      keyscheme:vi-normal
      (list
       "m l" 'list-bookmarks
       "m f" 'bookmark-hint
       "m o" 'set-url-from-bookmark
       "m M" 'add-bookmark
       "m m" 'add-bookmark
       "m d" 'delete-bookmark)))
   (style (theme:themed-css (theme *browser*)
            '("dl"
              :margin-left "8px")
            ;; Taken from buffer.lisp to save space for big bookmark lists.
            `(button
              :color ,theme:on-secondary-color
              :display "inline-block"
              :text-decoration "none"
              :margin-right "8px")))))

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
  `(("Title" ,(title entry) (:width 3))
    ("URL" ,(render-url (url entry)) (:width 2))
    ("Tags" ,(format nil "~{~a ~}" (tags entry)) (:width 1))
    ("Date" ,(local-time:format-timestring nil
                                           (date entry)
                                           :format local-time:+asctime-format+)
            (:width 1))))

(export-always 'equals)
(defmethod equals ((e1 bookmark-entry) (e2 bookmark-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (url-equal (url e1) (url e2)))

(-> persist-bookmark
    (quri:uri &key (:title string) (:date (or time:timestamp null)) (:tags t))
    t)
(export-always 'persist-bookmark)
(defun persist-bookmark (url &key date title tags)
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
                         (make-instance 'bookmark-entry
                                        :url url))))
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
          (setf bookmarks bookmarks-without-url)))
      (echo "Saved bookmark ~a" url))))

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
  (make-instance 'bookmark-source :actions-on-return actions-on-return))

(defun tag-suggestions ()
  (let ((bookmarks (files:content (bookmarks-file (current-buffer)))))
    ;; Warning: `sort' is destructive and `append' does not copy the last list,
    ;; so if we used `delete-duplicates' here it would have modified the last
    ;; list.
    (sort (remove-duplicates (mappend #'tags bookmarks)
                             :test #'string-equal)
          #'string-lessp)))

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

(export-always 'url-bookmark-tags)
(defun url-bookmark-tags (url)
  "Return the list of tags of the bookmark corresponding to URL."
  (let ((bookmarks (files:content (bookmarks-file (current-buffer)))))
    (when-let ((existing (find url bookmarks :key #'url :test #'url-equal)))
      (tags existing))))

(define-class new-tag-source (prompter:word-source)
  ((prompter:name "New tags")
   (prompter:filter-postprocessor
    ;; On no input, suggest the empty tag
    ;; which effectively acts as "no tag".
    ;; Without it, we would be forced to
    (lambda (suggestions source input)
      (declare (ignore source input))
      (or suggestions
          (list ""))))
   (prompter:enable-marks-p t)))

(export-always 'bookmark)
(defmethod bookmark ((url simple-array))
  (bookmark (quri:uri url)))

(defmethod bookmark ((url quri:uri))
  (let ((title (prompt1
                :prompt (format nil  "Title for ~a" (render-url url))
                :input (fetch-url-title (render-url url))
                :sources (make-instance 'prompter:raw-source
                                        :name "Title")))
        (tags (prompt
               :prompt (format nil "Tag(s) for ~a" (render-url url))
               :sources (list
                         (make-instance 'new-tag-source)
                         (make-instance 'tag-source
                                        :marks (url-bookmark-tags url))))))
    (persist-bookmark url :tags tags :title title)))

(defmethod bookmark ((buffer buffer))
  (let* ((url (url buffer))
         (title (prompt1
                 :prompt (format nil  "Title for ~a" (render-url url))
                 :input (title buffer)
                 :sources (make-instance 'prompter:raw-source
                                         :name "Title")))
         (tags (prompt
                :prompt (format nil "Tag(s) for ~a " (render-url url))
                :sources (list
                          (make-instance 'new-tag-source)
                          (make-instance 'keyword-source
                                         :buffer buffer)
                          (make-instance 'tag-source
                                         :marks (url-bookmark-tags
                                                 (url buffer)))))))
    (persist-bookmark url :title title :tags tags)))

(defmethod bookmark ((history-entry history-entry))
  (let* ((url (url history-entry))
         (title (prompt1
                 :prompt (format nil  "Title for ~a" (render-url url))
                 :input (title history-entry)
                 :sources (make-instance 'prompter:raw-source
                                         :name "Title")))
         (tags (prompt
                :prompt (format nil  "Tag(s) for ~a" (render-url url))
                :sources (list
                          (make-instance 'new-tag-source)
                          (make-instance 'tag-source
                                         :marks (url-bookmark-tags url))))))
    (persist-bookmark url :tags tags :title title)))

(define-command add-bookmark ()
  "Prompt for objects to bookmark."
  (prompt
   :prompt "Add Bookmark(s)"
   :input (render-url (url (current-buffer)))
   :sources (list
             (make-instance 'prompter:raw-source
                            :actions-on-return
                            (lambda-mapped-command bookmark))
             (make-instance 'buffer-source
                            :actions-on-return
                            (lambda-mapped-command bookmark))
             (make-instance 'global-history-source
                            :actions-on-return
                            (lambda-mapped-command bookmark)))))

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

(define-command edit-bookmark ()
  "Edit bookmark(s)."
  (let ((bookmarks (prompt
                    :prompt "Edit bookmark(s)"
                    :sources (make-instance 'bookmark-source
                                            :enable-marks-p t))))
    (loop for bookmark in bookmarks do
      (let* ((url (url bookmark))
             (title (prompt1
                     :prompt (format nil  "Title for ~a" (render-url url))
                     :input (title bookmark)
                     :sources (make-instance 'prompter:raw-source
                                             :name "Title")))
             (tags (prompt
                    :prompt (format nil  "Tag(s) for ~a" (render-url url))
                    :sources (list
                              (make-instance 'new-tag-source)
                              (make-instance 'tag-source
                                             :marks (url-bookmark-tags url))))))
        (persist-bookmark url :title title :tags tags)))))

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
      (render-menu 'bookmark-mode bookmarks-buffer)
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
                  (let ((url (render-url (url bookmark)))
                        (title (title bookmark))
                        (tags  (tags bookmark)))
                    (:div
                     :class "bookmark-entry"
                     (:dl
                      (:dt
                       (:button
                        :onclick
                        (ps:ps
                          (let ((section (ps:chain (nyxt/ps:active-element document)
                                                   (closest ".bookmark-entry"))))
                            (ps:chain section parent-node (remove-child section)))
                          (nyxt/ps:lisp-eval (:title "Delete"
                                              :buffer bookmarks-buffer)
                                             (delete-bookmark url)))
                        "×")
                       (:a :href url title))
                      (when tags
                        (:dd (:pre (format nil "Tags: ~{~a~^, ~}" tags))))))))))
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
                (persist-bookmark url-uri
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
       (bookmark (quri:uri url))))
   :selector "a"))
