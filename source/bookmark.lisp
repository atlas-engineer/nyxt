(in-package :nyxt)

;; We don't use CL-prevalence to serialize / deserialize bookmarks for a couple for reasons:
;; - It's too verbose, e.g. a list is
;; (:SEQUENCE 3 :CLASS CL:LIST :SIZE 2 :ELEMENTS ( "bar" "baz" ) )
;;
;; - We lack control on the linebreaks.
;;
;; - It needs IDs for every object, which makes it hard for the user to
;;   hand-edit the file without breaking it.
;;
;; - Un-explicitly-set class slots are exported if they have an initform;
;;   removing the initform forces us to put lots of (slot-boundp ...).

(defclass-export bookmark-entry ()
  ((url :initarg :url
        :accessor url
        :type string
        :initform "")
   (title :initarg :title
          :accessor title
          :type string
          :initform "")
   (annotation :initarg :annotation
               :accessor annotation
               :type string
               :initform "")
   (date :initarg :date
         :accessor date
         :type local-time:timestamp
         :initform (local-time:now))
   (tags :initarg :tags
         :accessor tags
         :type list-of-strings
         :initform nil
         :documentation "A list of strings.")
   (shortcut :initarg :shortcut
             :accessor shortcut
             :type string
             :initform ""
             :documentation "
This allows the following URL queries from the minibuffer:

- SHORTCUT: Open the associated bookmark.
- SHORTCUT TERM: Use SEARCH-URL to search TERM.  If SEARCH-URL is empty, fallback on other search engines.")
   (search-url :initarg :search-url
               :accessor search-url
               :type string
               :initform ""
               :documentation "
The URL to use when SHORTCUT is the first word in the input.
The search term is placed at '~a' in the SEARCH-URL if any, or at the end otherwise.
SEARCH-URL maybe either be a full URL or a path.  If the latter, the path is
appended to the URL.")))

(defmethod object-string ((entry bookmark-entry))
  (url entry))

(defmethod object-display ((entry bookmark-entry))
  (format nil "~a~a  ~a~a"
          (if (str:emptyp (shortcut entry))
              ""
              (str:concat "[" (shortcut entry) "] "))
          (url entry)
          (if (str:emptyp (title entry))
              ""
              (title entry))
          (if (tags entry)
              (format nil " (~{~a~^, ~})" (tags entry))
              "")))

(defun url-sans-scheme (url)
  (apply #'str:concat
         (mapcar (alex:curry #'format nil "~a")
                 (delete nil
                         (rest (multiple-value-list (quri:parse-uri url)))))))

(defun equal-url (url1 url2)
  "URLs are equal if the URIs are equal, scheme excluded.
Empty paths are also excluded from the comparison.
For instance, these are equal:
- http://example.org
- https://example.org/"
  (flet ((normalize-path (uri)
           (if (nth-value 4 (quri:parse-uri uri))
               uri
               (str:concat uri "/"))))
    (string= (url-sans-scheme (normalize-path url1))
             (url-sans-scheme (normalize-path url2)))))

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

(export-always 'bookmark-add)
(defun bookmark-add (url &key title tags)
  (unless (or (str:emptyp url)
              (string= "about:blank" url))
    (let* ((entry nil)
           (bookmarks-without-url (remove-if (lambda (b)
                                               (when (equal-url (url b) url)
                                                 (setf entry b)))
                                             (bookmarks-data *browser*)))
           (tags (if (stringp tags) (str:split " " tags :omit-nulls t) tags)))
      (unless entry
        (setf entry (make-instance 'bookmark-entry
                                   :url url)))
      (unless (str:emptyp title)
        (setf (title entry) title))
      (setf tags (delete-duplicates tags :test #'string=))
      (setf (tags entry) (sort tags #'string<))
      (push entry bookmarks-without-url)
      ;; Warning: Make sure to set bookmarks-data only once here since it is
      ;; persisted each time.
      (setf (bookmarks-data *browser*) bookmarks-without-url))))

(defun bookmark-completion-filter ()
  (lambda (minibuffer)
    (let* ((input-specs (multiple-value-list
                         (parse-tag-specification
                          (input minibuffer))))
           (tag-specs (first input-specs))
           (non-tags (str:downcase (str:join " " (second input-specs))))
           (validator (ignore-errors (tag-specification-validator tag-specs)))
           (bookmarks (bookmarks-data *browser*)))
      (when validator
        (setf bookmarks (remove-if (lambda (bookmark)
                                     (not (funcall validator
                                                   (tags bookmark))))
                                   bookmarks)))
      (fuzzy-match non-tags bookmarks))))

(export-always 'tag-completion-filter)
(declaim (ftype (function (&key (:with-empty-tag boolean)
                                (:extra-tags list-of-tags)))
                tag-completion-filter))
(defun tag-completion-filter (&key with-empty-tag extra-tags)
  "When with-empty-tag is non-nil, insert the empty string as the first tag.
This can be useful to let the user select no tag when returning directly."
  (let ((tags (sort (append extra-tags
                            (mapcar (lambda (name) (make-tag :name name))
                                    (delete-duplicates
                                     (apply #'append
                                            (mapcar (lambda (b) (tags b))
                                                    (bookmarks-data *browser*)))
                                     :test #'string-equal)))
                    #'string-lessp
                    :key #'tag-name)))
    (when with-empty-tag
      (push "" tags))
    (lambda (minibuffer)
      (fuzzy-match (word-at-cursor minibuffer) tags))))

(define-command insert-tag (&optional (minibuffer (current-minibuffer)))
  "Replace current work with selected tag."
  (let ((selection (get-candidate minibuffer)))
    (unless (uiop:emptyp selection)
      (with-accessors ((cursor input-cursor-position) (buffer input)) minibuffer
        (let ((before (subseq buffer 0 (word-start buffer cursor)))
              (after (subseq buffer (word-end buffer cursor))))
          (nyxt/minibuffer-mode:kill-whole-line minibuffer)
          (insert (str:concat before selection after)))))))

(define-mode set-tag-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the tag of a bookmark."
  ((keymap-scheme
    :initform
    (define-scheme "set-tag"
      scheme:cua
      (list "tab" 'insert-tag
            "return" 'nyxt/minibuffer-mode:return-immediate)))))

(define-command show-bookmarks ()
  "Show all bookmarks in a new buffer."
  (let* ((bookmarks-buffer (make-buffer :title "*Bookmarks*"))
         (bookmark-contents
           (markup:markup
            (:h1 "Bookmarks")
            (:body
             (loop for bookmark in (bookmarks-data *browser*)
                   collect (markup:markup (:p (:a :href (url bookmark) (url bookmark))
                                              " "
                                              ;; The :a tag must be on the URL because a bookmark may have no title.
                                              (:b (title bookmark))
                                              (when (tags bookmark)
                                                (format nil " (~{~a~^, ~})" (tags bookmark)))))))))
         (insert-contents (ps:ps (setf (ps:@ document Body |innerHTML|)
                                       (ps:lisp bookmark-contents)))))
    (ffi-buffer-evaluate-javascript bookmarks-buffer insert-contents)
    (set-current-buffer bookmarks-buffer)
    bookmarks-buffer))

(export-always 'url-bookmark-tags)
(defun url-bookmark-tags (url)
  "Return the space-separated string of tags of the bookmark corresponding to
URL."
  (let ((existing-bm (find url
                           (bookmarks-data *browser*)
                           :key #'url
                           :test #'equal-url)))
    (if existing-bm
        (str:join " " (tags existing-bm))
        "")))

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
    (if (uiop:emptyp (url buffer))
        (echo "Buffer has no URL.")
        (with-result* ((body (with-current-buffer buffer
                               (document-get-body)))
                       (tags (read-from-minibuffer
                              (make-minibuffer
                               :input-prompt "Space-separated tag(s)"
                               :default-modes '(set-tag-mode minibuffer-mode)
                               :input-buffer (url-bookmark-tags (url buffer))
                               :completion-function (tag-completion-filter
                                                     :extra-tags (make-tags (extract-keywords body 5)))))))
          (bookmark-add (url buffer)
                        :title (title buffer)
                        :tags tags)
          (echo "Bookmarked ~a." (url-display (url buffer)))))))

(define-command bookmark-page ()
  "Bookmark the currently opened page(s) in the active buffer."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Bookmark URL from buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (mapcar #'bookmark-current-page buffers)))

(define-command bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result* ((url (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Bookmark URL")))
                 (tags (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Space-separated tag(s)"
                         :default-modes '(set-tag-mode minibuffer-mode)
                         :input-buffer (url-bookmark-tags url)
                         :completion-function (tag-completion-filter)))))

    (bookmark-add url :tags tags)))

(define-command bookmark-delete ()
  "Delete bookmark(s)."
  (with-result (entries (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete bookmark(s)"
                          :multi-selection-p t
                          :default-modes '(minibuffer-tag-mode minibuffer-mode)
                          :completion-function (bookmark-completion-filter))))
    (setf (bookmarks-data *browser*)
          (set-difference (bookmarks-data *browser*) entries :test #'equals))))

(define-command insert-candidate-or-tag (&optional (minibuffer (current-minibuffer)))
  "Paste selected candidate or tag in input.
If character before cursor is '+' or '-' complete against tag."
  (cond
    ;; Complete a tag.
    ((and (not (str:emptyp (input minibuffer)))
          (let ((current-word (word-at-cursor minibuffer)))
            (or (str:starts-with? "-" current-word)
                (str:starts-with? "+" current-word))))
     (with-result (tag (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Tag"
                         :input-buffer (subseq (word-at-cursor minibuffer) 1)
                         :completion-function (tag-completion-filter))))
       (when tag
         (let ((before (subseq (input-buffer minibuffer)
                               0
                               (word-start (input-buffer minibuffer)
                                           (input-cursor-position minibuffer))))
               (after (subseq (input-buffer minibuffer)
                              (word-end (input-buffer minibuffer)
                                        (input-cursor-position minibuffer))))
               (prefix (elt (word-at-cursor minibuffer) 0)))
           (log:info (input minibuffer) before after)
           (nyxt/minibuffer-mode:kill-whole-line minibuffer)
           (insert (str:concat
                    before
                    (string prefix) (tag-name tag) " "
                    after)
                   minibuffer)))))
    (t
     (nyxt/minibuffer-mode:insert-candidate minibuffer))))

(define-mode minibuffer-tag-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the bookmark and their tags."
  ((keymap-scheme
    :initform
    (define-scheme "minibuffer-tag"
      scheme:cua
      (list
       "tab" 'insert-candidate-or-tag)))))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark."
  (with-result (entry (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Open bookmark"
                        :default-modes '(minibuffer-tag-mode minibuffer-mode)
                        :completion-function (bookmark-completion-filter))))
    ;; TODO: Add support for multiple bookmarks?
    (set-url* (url entry) :buffer (current-buffer) :raw-url-p t)))

(define-command set-url-from-bookmark-new-buffer ()
  "Open selected bookmark in a new buffer."
  (with-result (entry (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Open bookmark in a new buffer"
                        :default-modes '(minibuffer-tag-mode minibuffer-mode)
                        :completion-function (bookmark-completion-filter))))
    (set-url* (url entry) :buffer (make-buffer-focus :url nil) :raw-url-p t)))

(defmethod serialize-object ((entry bookmark-entry) stream)
  (unless (str:emptyp (url entry))
    (flet ((write-slot (slot)
             (unless (str:emptyp (funcall slot entry))
               (format t " :~a ~s"
                       (str:downcase slot)
                       (funcall slot entry)))))
      (let ((*standard-output* stream))
        (write-string "(:url ")
        (format t "~s" (url entry))
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
                    (when (getf entry :date)
                      (setf (getf entry :date)
                            (local-time:parse-timestring (getf entry :date))))
                    (apply #'make-instance 'bookmark-entry
                           entry))
                  entries)))
    (error (c)
      (log:error "During bookmark deserialization: ~a" c)
      nil)))

(defun store-sexp-bookmarks ()
  "Store the bookmarks to the browser `bookmarks-path'."
  (with-data-file (file (bookmarks-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; TODO: Make sorting customizable?  Note that `store-sexp-bookmarks' is
    ;; already a customizable function.
    (setf (slot-value *browser* 'bookmarks-data)
          (sort (slot-value *browser* 'bookmarks-data)
                (lambda (e1 e2)
                  (string< (url-sans-scheme (url e1))
                           (url-sans-scheme (url e2))))))
    (write-string "(" file)
    (dolist (entry (slot-value *browser* 'bookmarks-data))
      (write-char #\newline file)
      (serialize-object entry file))
    (format file "~%)~%")
    (echo "Saved ~a bookmarks to ~s."
          (length (slot-value *browser* 'bookmarks-data))
          (expand-path (bookmarks-path *browser*))))
  t)

(defun restore-sexp-bookmarks ()
  "Restore the bookmarks from the browser `bookmarks-path'."
  (handler-case
      (let ((data (with-data-file (file (bookmarks-path *browser*)
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file
                      (deserialize-bookmarks file)))))
        (when data
          (echo "Loading ~a bookmarks from ~s."
                (length data)
                (expand-path (bookmarks-path *browser*)))
          (setf (slot-value *browser* 'bookmarks-data) data)))
    (error (c)
      (echo-warning "Failed to load bookmarks from ~s: ~a" (expand-path (bookmarks-path *browser*)) c))))
