;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

;; TODO: Output tags on the same line.

;; Warning: We don't set most slots' initforms so that they are not serialized
;; if unset.
(defclass bookmark-entry ()
  ((url :initarg :url
        :accessor url
        :type string
        :initform "")
   (title :initarg :title
          :accessor title
          :type string)
   (annotation :initarg :annotation
               :accessor annotation
               :type string)
   (date :initarg :date
         :accessor date
         :type string
         :initform (local-time:format-timestring nil (local-time:now))
         ;; We don't store the local-time:timestamp because it's too verbose
         ;; when serialized.
         :documentation "The date must be parsable by `local-time:parse-timestring'.")
   (tags :initarg :tags
         :accessor tags
         :type list
         :documentation "A list of strings.")
   (shortcut :initarg :shortcut ; TODO: Add support for short-cuts in `parse-url'.
             :accessor shortcut
             :type string
             :documentation "
This allows the following URL queries from the minibuffer:

- SHORTCUT: Open the associated bookmark.
- SHORTCUT TERM: Use SEARCH-URL to search TERM.  If SEARCH-URL is empty, fallback on other search engines.")
   (search-url :initarg :search-url
               :accessor search-url
               :type string)))

(defmethod object-string ((entry bookmark-entry))
  (format nil "~a~a  ~a~a"
          (if (str:emptyp (shortcut entry))
              ""
              (str:concat "[" (shortcut entry) "] "))
          (url entry)
          (title entry)
          (if (tags entry)
              (format nil " (~{~a~^, ~})" (tags entry))
              "")))

(defun equal-url (url1 url2)
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (let ((uri1 (quri:uri url1))
        (uri2 (quri:uri url2)))
    (and (string= (quri:uri-host uri1) (quri:uri-host uri2))
         (string= (quri:uri-path uri1) (quri:uri-path uri2)))))

(defmethod equals ((e1 bookmark-entry) (e2 bookmark-entry))
  "Entries are equal if the hosts and the paths are equal.
In particular, we ignore the protocol (e.g. HTTP or HTTPS does not matter)."
  (equal-url (quri:uri (url e1)) (quri:uri (url e2))))

(defun bookmark-add (url &key title tags)
  (unless (or (str:emptyp url)
              (string= "about:blank" url))
    (let ((entry nil))
      (setf (bookmarks-data *interface*)
            (delete-if (lambda (b)
                         (when (equal-url (url b) url)
                           (setf entry b)))
                       (bookmarks-data *interface*)))
      (unless entry
        (make-instance 'bookmark-entry
               :url url))
      (unless (str:emptyp title)
        (setf (title entry) title))
      (when (slot-boundp entry 'tags)
        (setf tags (delete-duplicates (append (tags entry) tags)
                                      :test #'string=)))
      (setf (tags entry) (sort tags #'string<))
      (push entry (bookmarks-data *interface*)))))

(defun bookmark-completion-filter (input)
  ;; TODO: Filter by tags.
  (fuzzy-match input (bookmarks-data *interface*)))

(defun tag-completion-filter ()
  (let ((tags (delete-if #'null
                         (apply #'append
                                (mapcar (lambda (b) (when (slot-boundp b 'tags) (tags b)))
                                        (bookmarks-data *interface*))))))
    (lambda (input)
      (fuzzy-match input tags))))

(define-command bookmark-current-page (&optional (buffer (current-buffer)))
  "Bookmark the URL of BUFFER."
  (if (url buffer)
      (with-result (tags (read-from-minibuffer
                          (make-instance 'minibuffer
                                         :input-prompt "Tag(s):"
                                         :multi-selection-p t
                                         :completion-function (tag-completion-filter)
                                         ;; TODO: Can we query both new and existings tags?
                                         :empty-complete-immediate t)))
        (bookmark-add (url buffer)
                       :title (title buffer)
                       :tags tags)
        (echo "Bookmarked ~a." (url buffer)))
      (echo "Buffer has no URL.")))

(define-command bookmark-page ()
  "Bookmark the currently opened page(s) in the active buffer."
  (with-result (buffers (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Bookmark URL from buffer(s):"
                                        :multi-selection-p t
                                        :completion-function (buffer-completion-filter))))
    (mapcar #'bookmark-current-page buffers)))

(define-command bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result (url (read-from-minibuffer
                     (make-instance 'minibuffer
                                    :input-prompt "Bookmark URL:")))
    (bookmark-add url)))

(define-command bookmark-delete ()
  "Delete bookmark(s)."
  (with-result (entries (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Delete bookmark(s):"
                                        :multi-selection-p t
                                        :completion-function #'bookmark-completion-filter)))
    (setf (bookmarks-data *interface*)
          (set-difference (bookmarks-data *interface*) entries :test #'equals))))

(define-command bookmark-hint ()
  "Show link hints on screen, and allow the user to bookmark one"
  (with-result* ((links-json (add-link-hints))
                 (selected-hint (read-from-minibuffer
                                 (make-instance 'minibuffer
                                                :input-prompt "Bookmark hint:"
                                                :cleanup-function #'remove-link-hints))))
    (let* ((link-hints (cl-json:decode-json-from-string links-json))
           (selected-link (cadr (assoc selected-hint link-hints :test #'equalp))))
      (when selected-link
        (bookmark-add selected-link)))))

(define-deprecated-command bookmark-anchor ()
  "Deprecated by `bookmark-hint'."
  (bookmark-hint))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark."
  (with-result (entry (read-from-minibuffer
                       (make-instance 'minibuffer
                                      :input-prompt "Open bookmark:"
                                      :completion-function #'bookmark-completion-filter)))
    ;; TODO: Add support for multiple bookmarks?
    (set-url (url entry) :buffer (current-buffer) :raw-url-p t)))

(defun store-sexp-bookmarks ()         ; TODO: Factor with `store-sexp-session'.
  "Store the bookmarks to the interface `bookmarks-path'."
  (with-open-file (file (bookmarks-path *interface*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; TODO: Sort by customizable function, ignore protocol by default.
    (let ((*package* *package*)
          ;; (*print-case* :downcase) ; TODO: Wait until prevalence fully supports it.  https://github.com/40ants/cl-prevalence/issues/4
          (s-serialization::*local-package* (find-package 'next))
          (s-serialization::*one-element-per-line* t))
      ;; We need to make sure current package is :next so that
      ;; symbols a printed with consistent namespaces.
      (in-package :next)
      (s-serialization:serialize-sexp (bookmarks-data *interface*) file))))

(defun restore-sexp-bookmarks ()
  "Restore the bookmarks from the interface `bookmarks-path'."
  (let ((path (bookmarks-path *interface*)))
    (if (not (uiop:file-exists-p path))
        ;; TODO: Stop importing the SQLite bookmarks after 1.3.3?
        (dolist (url (import-sqlite-bookmarks))
          (unless (str:emptyp url)
            (let ((entry (make-instance 'bookmark-entry
                                        :url url)))
              ;; Calling (bookmarks-data *interface*) calls the restore function if
              ;; empty, so we need to use SLOT-VALUE here.
              (pushnew entry (slot-value *interface* 'bookmarks-data) :test #'equals)))))
    (handler-case
        (let ((data (with-open-file (file path
                                          :direction :input
                                          :if-does-not-exist nil)
                      (when file
                        ;; We need to make sure current package is :next so that
                        ;; symbols a printed with consistent namespaces.
                        (let (;; (*print-case* :downcase) ; TODO: Wait until prevalence fully supports it.
                              (*package* *package*))
                          (in-package :next)
                          (s-serialization:deserialize-sexp file))))))
          (when data
            (echo "Loading ~a bookmarks." (length data))
            (setf (slot-value *interface* 'bookmarks-data) data)))
      (error (c)
        (echo-warning "Failed to load bookmarks from ~a: ~a" path c)))))


;; SQLite importer.
(defun import-sqlite-bookmarks ()
  (let ((database-path (cl-ppcre:regex-replace "\\.lisp$"
                                               (namestring (bookmarks-path *interface*))
                                               ".db")))
    (unless (uiop:file-exists-p database-path)
      ;; Next 1.3.2 default database was named "bookmark.db".
      (setf database-path (cl-ppcre:regex-replace "s\\.lisp$"
                                                  (namestring (bookmarks-path *interface*))
                                                  ".db")))
    (when (uiop:file-exists-p database-path)
      (log:info "Importing bookmarks from ~a." database-path)
      (let* ((db (sqlite:connect database-path))
             (urls
               (sqlite:execute-to-list
                db "select url from bookmarks")))
        (sqlite:disconnect db)
        (apply #'append urls)))))
