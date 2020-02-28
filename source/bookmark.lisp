;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

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

(defclass bookmark-entry ()
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

(defun url-sans-protocol (url)
  (let ((uri (quri:uri url)))
    (str:concat (quri:uri-host uri)
                (quri:uri-path uri))))

(defun equal-url (url1 url2)
  "URLs are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (string= (url-sans-protocol url1) (url-sans-protocol url2)))

(defmethod equals ((e1 bookmark-entry) (e2 bookmark-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (equal-url (url e1) (url e2)))

(defun bookmark-add (url &key title tags)
  (unless (or (str:emptyp url)
              (string= "about:blank" url))
    (let ((entry nil))
      (setf (bookmarks-data *browser*)
            (delete-if (lambda (b)
                         (when (equal-url (url b) url)
                           (setf entry b)))
                       (bookmarks-data *browser*)))
      (unless entry
        (setf entry (make-instance 'bookmark-entry
                                   :url url)))
      (unless (str:emptyp title)
        (setf (title entry) title))
      (setf tags (delete-duplicates (append (tags entry) tags)
                                    :test #'string=))
      (setf (tags entry) (sort tags #'string<))
      (push entry (bookmarks-data *browser*)))))

(defun bookmark-completion-filter ()
  (lambda (input)
    (let* ((input-specs (multiple-value-list (parse-tag-specification
                                              (str:replace-all " " " " input))))
           (tag-specs (nth 0 input-specs))
           (non-tags (str:downcase (str:join " " (nth 1 input-specs))))
           (validator (ignore-errors (tag-specification-validator tag-specs)))
           (bookmarks (bookmarks-data *browser*)))
      (when validator
        (setf bookmarks (remove-if (lambda (bookmark)
                                     (not (funcall validator
                                                   (tags bookmark))))
                                   bookmarks)))
      (fuzzy-match non-tags bookmarks))))

(defun tag-completion-filter (&key with-empty-tag)
  "When with-empty-tag is non-nil, insert the empty string as the first tag.
This can be useful to let the user select no tag when returning directly."
  (let ((tags (sort (delete-duplicates
                     (apply #'append
                            (mapcar (lambda (b) (tags b))
                                    (bookmarks-data *browser*)))
                     :test #'string-equal)
                    #'string-lessp)))
    (when with-empty-tag
      (push "" tags))
    (lambda (input)
      (fuzzy-match input tags))))

(define-command bookmark-current-page (&optional (buffer (current-buffer)))
  "Bookmark the URL of BUFFER."
  (if (url buffer)
      (with-result (tags (read-from-minibuffer
                          (make-minibuffer
                           :input-prompt "Space-separated tag(s)"
                           :multi-selection-p t
                           :completion-function (tag-completion-filter :with-empty-tag t)
                           :empty-complete-immediate t)))
        (when tags
          ;; Turn tags with spaces into multiple tags.
          (setf tags (alexandria:flatten
                      (mapcar (lambda (tag)
                                (str:split " " tag :omit-nulls t))
                              tags))))
        (bookmark-add (url buffer)
                       :title (title buffer)
                       :tags tags)
        (echo "Bookmarked ~a." (url buffer)))
      (echo "Buffer has no URL.")))

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
  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt "Bookmark URL")))
    (bookmark-add url)))

(define-command bookmark-delete ()
  "Delete bookmark(s)."
  (with-result (entries (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete bookmark(s)"
                          :multi-selection-p t
                          :completion-function (bookmark-completion-filter))))
    (setf (bookmarks-data *browser*)
          (set-difference (bookmarks-data *browser*) entries :test #'equals))))

(define-command bookmark-hint ()
  "Show link hints on screen, and allow the user to bookmark one"
  (with-result* ((links-json (add-element-hints))
                 (selected-hint (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt "Bookmark hint"
                                  :cleanup-function #'remove-element-hints))))
    (let* ((link-hints (cl-json:decode-json-from-string links-json))
           (selected-link (cadr (assoc selected-hint link-hints :test #'equalp))))
      (when selected-link
        (bookmark-add selected-link)))))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark."
  (with-result (entry (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Open bookmark"
                        :completion-function (bookmark-completion-filter))))
    ;; TODO: Add support for multiple bookmarks?
    (set-url (url entry) :buffer (current-buffer) :raw-url-p t)))



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
      (log:error "Error during bookmark deserialization: ~a" c)
      nil)))

(defun store-sexp-bookmarks ()
  "Store the bookmarks to the browser `bookmarks-path'."
  (with-open-file (file (bookmarks-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; TODO: Make sorting customizable?  Note that `store-sexp-bookmarks' is
    ;; already a customizable function.
    (setf (slot-value *browser* 'bookmarks-data)
          (sort (slot-value *browser* 'bookmarks-data)
                (lambda (e1 e2)
                  (string< (url-sans-protocol (url e1))
                           (url-sans-protocol (url e2))))))
    (write-string "(" file)
    (dolist (entry (slot-value *browser* 'bookmarks-data))
      (write-char #\newline file)
      (serialize-object entry file))
    (write-char #\newline file)
    (write-string ")" file)
    (echo "Saved ~a bookmarks to ~a."
          (length (slot-value *browser* 'bookmarks-data))
          (bookmarks-path *browser*)))
  t)

(defun restore-sexp-bookmarks ()
  "Restore the bookmarks from the browser `bookmarks-path'."
  (let ((path (bookmarks-path *browser*)))
    (if (not (uiop:file-exists-p path))
        ;; TODO: Stop importing the SQLite bookmarks after 1.3.3?
        (dolist (url (import-sqlite-bookmarks))
          (unless (str:emptyp url)
            (let ((entry (make-instance 'bookmark-entry
                                        :url url)))
              ;; Calling (bookmarks-data *browser*) calls the restore function if
              ;; empty, so we need to use SLOT-VALUE here.
              (pushnew entry (slot-value *browser* 'bookmarks-data) :test #'equals)))))
    (handler-case
        (let ((data (with-open-file (file path
                                          :direction :input
                                          :if-does-not-exist nil)
                      (when file
                        (deserialize-bookmarks file)))))
          (when data
            (echo "Loading ~a bookmarks from ~a."
                  (length data)
                  (bookmarks-path *browser*))
            (setf (slot-value *browser* 'bookmarks-data) data)))
      (error (c)
        (echo-warning "Failed to load bookmarks from ~a: ~a" path c)))))


;; SQLite importer.
(defun import-sqlite-bookmarks ()
  (let ((database-path (cl-ppcre:regex-replace "\\.lisp$"
                                               (namestring (bookmarks-path *browser*))
                                               ".db")))
    (unless (uiop:file-exists-p database-path)
      ;; Next 1.3.2 default database was named "bookmark.db".
      (setf database-path (cl-ppcre:regex-replace "s\\.lisp$"
                                                  (namestring (bookmarks-path *browser*))
                                                  ".db")))
    (when (uiop:file-exists-p database-path)
      (log:info "Importing bookmarks from ~a." database-path)
      (let* ((db (sqlite:connect database-path))
             (urls
               (sqlite:execute-to-list
                db "select url from bookmarks")))
        (sqlite:disconnect db)
        (apply #'append urls)))))
