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

(define-class bookmark-entry ()
  ((url (quri:uri ""))
   (title "")
   (annotation "")
   (date (local-time:now))
   (tags '()
         :type list-of-strings)
   (shortcut ""
             :documentation "
This allows the following URL queries from the minibuffer:

- SHORTCUT: Open the associated bookmark.
- SHORTCUT TERM: Use SEARCH-URL to search TERM.  If SEARCH-URL is empty, fallback on other search engines.")
   (search-url ""
               :documentation "
The URL to use when SHORTCUT is the first word in the input.
The search term is placed at '~a' in the SEARCH-URL if any, or at the end otherwise.
SEARCH-URL maybe either be a full URL or a path.  If the latter, the path is
appended to the URL."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(defmethod object-string ((entry bookmark-entry))
  (object-string (url entry)))

(defmethod object-display ((entry bookmark-entry))
  (format nil "~a~a  ~a~a"
          (if (str:emptyp (shortcut entry))
              ""
              (str:concat "[" (shortcut entry) "] "))
          (object-display (url entry))
          (if (str:emptyp (title entry))
              ""
              (title entry))
          (if (tags entry)
              (format nil " (~{~a~^, ~})" (tags entry))
              "")))

(declaim (ftype (function (quri:uri quri:uri) boolean) equal-url))
(defun equal-url (url1 url2)
  "URLs are equal if the URIs are equal, scheme excluded.
Empty paths are also excluded from the comparison.
For instance, these are equal:
- http://example.org
- https://example.org/"
  (if (and (quri:uri-http-p url1)
           (quri:uri-http-p url2))
      (schemeless-uri= url1 url2)
      (the (values boolean &optional) (quri:uri= url1 url2))))

(defmethod equals ((e1 bookmark-entry) (e2 bookmark-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (equal-url (url e1) (url e2)))

(defstruct tag
  name
  description)

(defmethod object-string ((tag tag))
  (tag-name tag))

(defmethod object-display ((tag tag))
  (if (tag-description tag)
      (format nil "~a (~a)"
              (tag-name tag)
              (tag-description tag))
      (object-string tag)))

(declaim (ftype (function (quri:uri &key (:title string) (:tags t)) t) bookmark-add))
(export-always 'bookmark-add)
(defun bookmark-add (url &key title tags)
  (with-data-access bookmarks (bookmarks-path (current-buffer))
    (unless (or (url-empty-p url)
                (string= "about:blank" (object-string url)))
      (let* ((entry nil)
             (bookmarks-without-url (remove-if (lambda (b)
                                                 (when (equal-url (url b) url)
                                                   (setf entry b)))
                                               bookmarks))
             (tags (if (stringp tags) (str:split " " tags :omit-nulls t) tags)))
        (unless entry
          (setf entry (make-instance 'bookmark-entry
                                     :url url)))
        (unless (str:emptyp title)
          (setf (title entry) title))
        (setf tags (delete-duplicates tags :test #'string=))
        (setf (tags entry) (sort tags #'string<))
        (push entry bookmarks-without-url)
        (setf bookmarks bookmarks-without-url)))))

(export-always 'match-bookmarks)
(defun match-bookmarks (specification &optional (as-url-list-p t))
  (let* ((input-specs (multiple-value-list
                       (parse-tag-specification
                        specification)))
         (tag-specs (first input-specs))
         (non-tags (str:downcase (str:join " " (second input-specs))))
         (validator (ignore-errors (tag-specification-validator tag-specs)))
         (bookmarks (get-data (bookmarks-path (current-buffer)))))
    (when validator
      (setf bookmarks (remove-if (lambda (bookmark)
                                   (not (funcall validator
                                                 (tags bookmark))))
                                 bookmarks)))
    (if as-url-list-p
        (mapcar #'url (fuzzy-match non-tags bookmarks))
        (fuzzy-match non-tags bookmarks))))

(defun bookmark-suggestion-filter ()
  (lambda (minibuffer)
    (match-bookmarks (input-buffer minibuffer) nil)))

(export-always 'tag-suggestion-filter)
(declaim (ftype (function (&key (:with-empty-tag boolean)
                                (:extra-tags list-of-tags)))
                tag-suggestion-filter))
(defun tag-suggestion-filter (&key with-empty-tag extra-tags)
  "When with-empty-tag is non-nil, insert the empty string as the first tag.
This can be useful to let the user select no tag when returning directly."
  (let ((tags (sort (append extra-tags
                            (mapcar (lambda (name) (make-tag :name name))
                                    (delete-duplicates
                                     (apply #'append
                                            (mapcar #'tags (get-data
                                                            (bookmarks-path (current-buffer)))))
                                     :test #'string-equal)))
                    #'string-lessp
                    :key #'tag-name)))
    (when with-empty-tag
      (push "" tags))
    (lambda (minibuffer)
      (fuzzy-match (text-buffer::word-at-cursor (input-cursor minibuffer)) tags))))

(define-command insert-tag (&optional (minibuffer (current-minibuffer)))
  "Replace current word with selected tag."
  (let ((selection (get-suggestion minibuffer)))
    (unless (uiop:emptyp selection)
      (text-buffer::replace-word-at-cursor (input-cursor minibuffer) (str:concat selection " "))
      (update-display minibuffer))))

(define-mode set-tag-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the tag of a bookmark."
  ((keymap-scheme
    (define-scheme "set-tag"
      scheme:cua
      (list "tab" 'insert-tag
            "return" 'nyxt/minibuffer-mode:return-immediate)))))

(define-command list-bookmarks ()
  "List all bookmarks in a new buffer."
  (let* ((bookmarks-buffer (make-internal-buffer :title "*Bookmarks*"))
         (bookmark-contents
           (markup:markup
            (:style (style bookmarks-buffer))
            (:h1 "Bookmarks")
            (:body
             (loop for bookmark in (get-data (bookmarks-path (current-buffer)))
                   collect
                      (let ((url-display (object-display (url bookmark)))
                            (url-href (object-string (url bookmark))))
                        (markup:markup (:div
                                        (:p (:b "Title: ") (title bookmark))
                                        (:p (:b "URL: ") (:a :href url-href
                                                             url-display))
                                        (:p (:b "Tags: ")
                                            (when (tags bookmark)
                                              (format nil " (~{~a~^, ~})" (tags bookmark))))
                                        (:p (:a :class "button"
                                                :href (lisp-url `(nyxt::delete-bookmark ,url-href)) "Delete"))
                                        (:hr ""))))))))
         (insert-contents (ps:ps (setf (ps:@ document Body |innerHTML|)
                                       (ps:lisp bookmark-contents)))))
    (ffi-buffer-evaluate-javascript bookmarks-buffer insert-contents)
    (set-current-buffer bookmarks-buffer)
    bookmarks-buffer))

(declaim (ftype (function (quri:uri) string) url-bookmark-tags))
(export-always 'url-bookmark-tags)
(defun url-bookmark-tags (url)
  "Return the space-separated string of tags of the bookmark corresponding to
URL."
  (the (values string &optional)
       (let ((existing-bm (find url
                                (get-data (bookmarks-path (current-buffer)))
                                :key #'url
                                :test #'equal-url)))
         (if existing-bm
             (str:join " " (tags existing-bm))
             ""))))

(define-command bookmark-current-page (&optional (buffer (current-buffer)))
  "Bookmark the URL of BUFFER."
  (flet ((extract-keywords (html limit)
           (sera:take limit (delete "" (mapcar #'first
                                               (text-analysis:document-keywords
                                                (make-instance
                                                 'text-analysis:document
                                                 :string-contents (plump:text (plump:parse html)))))
                                    :test #'string=)))
         (make-tags (name-list)
           (mapcar (lambda (name) (make-tag :name name :description "suggestion"))
                   name-list)))
    (if (url-empty-p (url buffer))
        (echo "Buffer has no URL.")
        (with-result* ((body (with-current-buffer buffer
                               (document-get-body)))
                       (tags (read-from-minibuffer
                              (make-minibuffer
                               :input-prompt "Space-separated tag(s)"
                               :default-modes '(set-tag-mode minibuffer-mode)
                               :input-buffer (url-bookmark-tags (url buffer))
                               :suggestion-function (tag-suggestion-filter
                                                     :extra-tags (make-tags (extract-keywords body 5)))))))
          (bookmark-add (url buffer)
                        :title (title buffer)
                        :tags tags)
          (echo "Bookmarked ~a." (object-display (url buffer)))))))

(define-command bookmark-page ()
  "Bookmark the currently opened page(s) in the active buffer."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Bookmark URL from buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (mapcar #'bookmark-current-page buffers)))

(define-command bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt "Bookmark URL")))
    (if (not (valid-url-p url))
        (echo "Invalid URL")
        (let ((url (quri:uri url)))
          (with-result (tags (read-from-minibuffer
                              (make-minibuffer
                               :input-prompt "Space-separated tag(s)"
                               :default-modes '(set-tag-mode minibuffer-mode)
                               :input-buffer (url-bookmark-tags url)
                               :suggestion-function (tag-suggestion-filter))))
            (bookmark-add url :tags tags))))))

(define-command bookmark-delete ()
  "Delete bookmark(s)."
  (with-data-access bookmarks (bookmarks-path (current-buffer))
    (with-result (entries (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Delete bookmark(s)"
                            :multi-selection-p t
                            :default-modes '(minibuffer-tag-mode minibuffer-mode)
                            :suggestion-function (bookmark-suggestion-filter))))
      (setf bookmarks
            (set-difference bookmarks
                            entries :test #'equals)))))

(defun delete-bookmark (url)
  "Delete a bookmark by the URL. This function depends on equals only
comparing URLs."
  (with-data-access bookmarks (bookmarks-path (current-buffer))
    (setf bookmarks
          (set-difference
           bookmarks
           (list (make-instance 'bookmark-entry :url (quri:uri url)))
           :test #'equals))))

(define-command insert-suggestion-or-tag (&optional (minibuffer (current-minibuffer)))
  "Paste selected suggestion or tag in input.
If character before cursor is '+' or '-' complete against tag."
  (let* ((current-word (text-buffer::word-at-cursor (input-cursor minibuffer)))
         (operand? (unless (str:emptyp current-word) (subseq current-word 0 1))))
    (if (or (equal "-" operand?)
            (equal "+" operand?))
        (with-result (tag (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Tag"
                            :input-buffer (subseq current-word 1)
                            :suggestion-function (tag-suggestion-filter))))
          (when tag
            (text-buffer::replace-word-at-cursor
             (input-cursor minibuffer)
             (str:concat operand? (tag-name tag)))))
        (nyxt/minibuffer-mode:insert-suggestion minibuffer))))

(define-mode minibuffer-tag-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the bookmark and their tags."
  ((keymap-scheme
    (define-scheme "minibuffer-tag"
      scheme:cua
      (list
       "tab" 'insert-suggestion-or-tag)))))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark.
With multiple selections, open the first bookmark in the current buffer, the
rest in background buffers."
  (with-result (entries (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Open bookmark(s)"
                          :default-modes '(minibuffer-tag-mode minibuffer-mode)
                          :suggestion-function (bookmark-suggestion-filter)
                          :multi-selection-p t)))
    (dolist (entry (rest entries))
      (make-buffer :url (object-string (url entry))))
    (buffer-load (url (first entries)))))

(define-command set-url-from-bookmark-new-buffer ()
  "Open selected bookmarks in new buffers."
  (with-result (entries (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Open bookmarks in new buffers"
                          :default-modes '(minibuffer-tag-mode minibuffer-mode)
                          :suggestion-function (bookmark-suggestion-filter)
                          :multi-selection-p t)))
    (dolist (entry (rest entries))
      (make-buffer :url (object-string (url entry))))
    (buffer-load (url (first entries))
                 :buffer (make-buffer-focus))))

(defmethod serialize-object ((entry bookmark-entry) stream)
  (unless (url-empty-p (url entry))
    (flet ((write-slot (slot)
             (unless (str:emptyp (funcall slot entry))
               (format t " :~a ~s"
                       (str:downcase slot)
                       (funcall slot entry)))))
      (let ((*standard-output* stream))
        (write-string "(:url ")
        (format t "~s" (object-string (url entry)))
        (write-slot 'title)
        (write-slot 'annotation)
        (when (date entry)
          (write-string " :date ")
          (format t "~s" (local-time:format-timestring nil (date entry))))
        (when (tags entry)
          (write-string " :tags (")
          (format t "~s" (first (tags entry)))
          (dolist (tag (rest (tags entry)))
            (write-string " ")
            (write tag))
          (write-string ")"))
        (write-slot 'shortcut)
        (write-slot 'search-url)
        (write-string ")")))))

(defmethod deserialize-bookmarks (stream)
  (handler-case
      (let ((*standard-input* stream))
        (let ((entries (read stream)))
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
    (error (c)
      (log:error "During bookmark deserialization: ~a" c)
      nil)))

(defmethod store ((profile data-profile) (path bookmarks-data-path))
  "Store the bookmarks to the buffer `bookmarks-path'."
  (with-data-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; TODO: Make sorting customizable?  Note that `store-sexp-bookmarks' is
    ;; already a customizable function.
    (setf (get-data path)
          (sort (get-data path)
                #'uri< :key #'url))
    (write-string "(" file)
    (dolist (entry (get-data path))
      (write-char #\newline file)
      (serialize-object entry file))
    (format file "~%)~%")
    (echo "Saved ~a bookmarks to ~s."
          (length (get-data path))
          (expand-path path)))
  t)

(defmethod restore ((profile data-profile) (path bookmarks-data-path))
  "Restore the bookmarks from the buffer `bookmarks-path'."
  (handler-case
      (let ((data (with-data-file (file path
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file
                      (deserialize-bookmarks file)))))
        (when data
          (echo "Loading ~a bookmarks from ~s." (length data) (expand-path path))
          (setf (get-data path) data)))
    (error (c)
      (echo-warning "Failed to load bookmarks from ~s: ~a"
                    (expand-path path) c))))
