;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;;; We don't use CL-prevalence to serialize / deserialize bookmarks for a couple for reasons:
;;; - It's too verbose, e.g. a list is
;;; (:SEQUENCE 3 :CLASS CL:LIST :SIZE 2 :ELEMENTS ( "bar" "baz" ) )
;;;
;;; - We lack control on the linebreaks.
;;;
;;; - It needs IDs for every object, which makes it hard for the user to
;;;   hand-edit the file without breaking it.
;;;
;;; - Un-explicitly-set class slots are exported if they have an initform;
;;;   removing the initform forces us to put lots of (slot-boundp ...).

(define-class bookmarks-file (nfiles:data-file nyxt-lisp-file)
  ((nfiles:base-path #p"bookmarks")
   (nfiles:name "bookmarks"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class bookmark-entry ()
  ((url (quri:uri ""))
   (title "")
   (annotation "")
   (date (local-time:now))
   (tags
    '()
    :type list-of-strings))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((entry bookmark-entry))
  `(("URL" ,(render-url (url entry)))
    ("Title" ,(title entry))
    ("Tags" ,(format nil "~{~a ~}" (tags entry)))
    ("Date" ,(princ-to-string (date entry)))))

(export-always 'equals)
(defmethod equals ((e1 bookmark-entry) (e2 bookmark-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (url-equal (url e1) (url e2)))

(-> bookmark-add
    (quri:uri &key (:title string) (:date (or local-time:timestamp null)) (:tags t))
    t)
(export-always 'bookmark-add)
(defun bookmark-add (url &key date title tags)
  (nfiles:with-file-content (bookmarks (bookmarks-file (current-buffer)))
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
          (setf tags (delete "" tags :test #'string=))
          (setf tags (delete-duplicates tags :test #'string=))
          (setf (tags entry) (sort tags #'string<))
          (when date
            (setf (date entry) date))
          (push entry bookmarks-without-url)
          (setf bookmarks bookmarks-without-url))))))

(define-class bookmark-source (prompter:source)
  ((prompter:name "Bookmarks")
   (prompter:constructor (nfiles:content (bookmarks-file (current-buffer))))
   (prompter:multi-selection-p t)
   (prompter:active-attributes-keys '("URL" "Title" "Tags"))))

(defun tag-suggestions ()
  (let ((bookmarks (nfiles:content (bookmarks-file (current-buffer)))))
    ;; Warning: `sort' is destructive and `append' does not copy the last list,
    ;; so if we used `delete-duplicates' here it would have modified the last
    ;; list.
    (let ((tags (sort (remove-duplicates
                       (apply #'append
                              (mapcar #'tags bookmarks))
                       :test #'string-equal)
                      #'string-lessp)))
      tags)))

(defun last-word (s)
  (if (uiop:emptyp s)
      ""
      (alex:last-elt (sera:words s))))

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
   (prompter:multi-selection-p t)
   (prompter:constructor (tag-suggestions)))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class keyword-source (prompter:source)
  ((prompter:name "Keywords")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (mapcar #'car (keywords (buffer source))))))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-panel-global bookmarks ()
    (panel-buffer "*Bookmarks panel*")
  "Shows all the bookmarks in a compact panel-buffer layout."
  (spinneret:with-html-string
    (:style (cl-css:css
             '((p
                :font-size "12px"
                :margin "0"
                :white-space "nowrap"
                :overflow-x "hidden"
                :text-overflow "ellipsis")
               (div
                :padding-bottom "10px"))))
    (:body
     (:h1 "Bookmarks")
     (or (let ((bookmarks (nfiles:content (bookmarks-file (current-buffer)))))
           (loop for bookmark in bookmarks
                 collect
                 (let ((url-href (render-url (url bookmark))))
                   (:div
                    (:p (title bookmark))
                    (:p (:a :href url-href url-href))))))
         (format nil "No bookmarks in ~s." (nfiles:expand (nfiles:content (bookmarks-file (current-buffer)))))))))

(export-always 'url-bookmark-tags)
(defun url-bookmark-tags (url)
  "Return the list of tags of the bookmark corresponding to URL."
  (let ((bookmarks (nfiles:content (bookmarks-file (current-buffer)))))
    (alex:when-let ((existing (find url bookmarks :key #'url :test #'url-equal)))
      (tags existing))))

(define-command bookmark-current-url (&optional (buffer (current-buffer)))
  "Bookmark the URL of BUFFER."
  (if (url-empty-p (url buffer))
      (echo "Buffer has no URL.")
      (let ((tags (prompt
                   :prompt (format nil "Tag(s) for ~a " (render-url (url buffer)))
                   :sources (list
                             (make-instance 'prompter:word-source
                                            :name "New tags"
                                            ;; On no input, suggest the empty tag which effectively acts as "no tag".
                                            ;; Without it, we would be force to specify a tag.
                                            :filter-postprocessor
                                            (lambda (suggestions source input)
                                              (declare (ignore source input))
                                              (or suggestions
                                                  (list "")))
                                            :multi-selection-p t)
                             (make-instance 'keyword-source
                                            :buffer buffer)
                             (make-instance 'tag-source
                                            :marks (url-bookmark-tags (url buffer)))))))
        (bookmark-add (url buffer)
                      :title (title buffer)
                      :tags tags)
        (echo "Bookmarked ~a." (render-url (url buffer))))))

(define-command bookmark-buffer-url ()
  "Bookmark the currently opened page(s) in the active buffer."
  (prompt
   :prompt "Bookmark URL from buffer(s)"
   :sources (make-instance 'user-buffer-source
                           :multi-selection-p t
                           :actions (list (make-mapped-command bookmark-current-url)))))

(define-command bookmark-url (&key url)
  "Allow the user to bookmark a URL via minibuffer input."
  (let ((url (or url
                 (ignore-errors
                  (quri:uri
                   (first
                    (prompt
                     :prompt "Bookmark URL"
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "New URL")))))))))
    (if (not (valid-url-p url))
        (echo "Invalid URL '~a'" url)
        (let* ((url (quri:uri url))
               (title (fetch-url-title url))
               (tags (prompt
                      :prompt "Tag(s)"
                      :sources (list
                                (make-instance 'prompter:word-source
                                               :name "New tags"
                                               :multi-selection-p t)
                                (make-instance 'tag-source
                                               :marks (url-bookmark-tags url))))))
          (bookmark-add url :tags tags :title title)))))

(define-command delete-bookmark (&optional urls-or-bookmark-entries)
  "Delete bookmark(s) matching URLS-OR-BOOKMARK-ENTRIES.
URLS is either a list or a single element."
  (if urls-or-bookmark-entries
      (nfiles:with-file-content (bookmarks (bookmarks-file (current-buffer)))
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
                      ;; :default-modes '(minibuffer-tag-mode minibuffer-mode)
                      :sources (make-instance 'bookmark-source
                                              :multi-selection-p t))))
        (delete-bookmark entries))))

(define-command set-url-from-bookmark
    (&key (actions (list (make-command buffer-load* (suggestion-values)
                           "Load first selected bookmark in current buffer and the rest in new buffer(s)."
                           (mapc (lambda (url) (make-buffer :url (url url))) (rest suggestion-values))
                           (buffer-load (url (first suggestion-values))))
                         (make-command new-buffer-load (suggestion-values)
                           "Load bookmark(s) in new buffer(s)."
                           (mapc (lambda (url) (make-buffer :url (url url))) (rest suggestion-values))
                           (make-buffer-focus :url (url (first suggestion-values)))))))
  "Set the URL for the current buffer from a bookmark.
With multiple selections, open the first bookmark in the current buffer, the
rest in background buffers."
  (prompt
   :prompt "Open bookmark(s)"
   :sources (make-instance 'bookmark-source
                           :actions actions)))

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
          (format t "~s" (local-time:format-timestring nil (date entry)
                                                       :timezone local-time:+utc-zone+)))
        (when (tags entry)
          (write-string " :tags (")
          (format t "~s" (first (tags entry)))
          (dolist (tag (rest (tags entry)))
            (write-string " ")
            (write tag))
          (write-string ")"))
        (write-string ")")))))

(defmethod nfiles:serialize ((profile nyxt-profile) (file bookmarks-file) stream &key)
  (let ((content
          ;; Sort the entries to make serialization reproducible.
          ;; Particularly useful when bookmarks are under version control.
          (sort (nfiles:content file)
                #'url< :key #'url)))
    (write-string "(" stream)
    (dolist (entry content)
      (write-string +newline+ stream)
      (serialize-object entry stream))
    (format stream "~%)~%")
    (echo "Saved ~a bookmarks to ~s."
          (length content)
          (nfiles:expand file))))

(defmethod nfiles:deserialize ((profile nyxt-profile) (path bookmarks-file) raw-content &key)
  (let ((entries (read raw-content)))
    (mapcar (lambda (entry)
              (when (getf entry :url)
                (setf (getf entry :url)
                      (quri:uri (getf entry :url))))
              (when (getf entry :date)
                (setf (getf entry :date)
                      (local-time:parse-timestring (getf entry :date))))
              (apply #'make-instance 'bookmark-entry
                     entry))
            entries)))

(define-command import-bookmarks-from-html (&key (html-file nil))
  "Import bookmarks from an HTML file."
  (let ((html-file (or html-file
                       (prompt1
                         ;; TODO: Is there a more intuitive directory for bookmarks?
                         :input (uiop:native-namestring (uiop:getcwd))
                         :extra-modes '(nyxt/file-manager-mode:file-manager-mode)
                         :sources (make-instance
                                   'nyxt/file-manager-mode:user-file-source
                                   :extensions '("html"))))))
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
                                :date (ignore-errors (local-time:unix-to-timestamp (parse-integer date)))
                                :tags (when tags
                                        (str:split "," tags))))))))
        (echo "The file doesn't exist or is not an HTML file."))))
