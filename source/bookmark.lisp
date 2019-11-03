;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

(defun %initialize-bookmark-db (path)
  "Create a database file if necessary and make a table for bookmarks"
  (close (open (ensure-parent-exists path) :direction :probe :if-does-not-exist :create))
  (let ((db (sqlite:connect
             (truename (probe-file path)))))
    (sqlite:execute-non-query
     db "create table bookmarks (id integer primary key, url text not null)")
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" "about:blank")
    (sqlite:disconnect db)))

(defun bookmark-complete (input)
  (let* ((db (sqlite:connect
              (ensure-file-exists (bookmark-db-path *interface*)
                                  #'%initialize-bookmark-db)))
         (candidates
           (sqlite:execute-to-list
            db "select url from bookmarks where url like ?"
            (format nil "%~a%" (str:replace-all " " "%" input)))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))

(defun %bookmark-url (url)
  (let ((db (sqlite:connect
             (ensure-file-exists (bookmark-db-path *interface*)
                                 #'%initialize-bookmark-db))))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" url)
    (sqlite:disconnect db)))

(define-command bookmark-current-page ()
  "Bookmark the currently opened page in the active buffer."
  (with-result (url (buffer-get-url))
    (let ((db (sqlite:connect
               (ensure-file-exists (bookmark-db-path *interface*)
                                   #'%initialize-bookmark-db))))
      (sqlite:execute-non-query
       db "insert into bookmarks (url) values (?)" url)
      (sqlite:disconnect db)))
  (echo "Current page bookmarked."))

(define-command bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result (url (read-from-minibuffer (make-instance 'minibuffer
                                                         :input-prompt "Bookmark URL:")))
    (%bookmark-url url)))

(define-command bookmark-delete ()
  "Delete a bookmark from the bookmark database."
  (with-result (bookmark (read-from-minibuffer
                          (make-instance 'minibuffer
                                         :input-prompt "Delete bookmark:"
                                         :completion-function 'bookmark-complete)))
    (let ((db (sqlite:connect
               (ensure-file-exists (bookmark-db-path *interface*)
                                   #'%initialize-bookmark-db))))
      (sqlite:execute-non-query
       db "delete from bookmarks where url = ?" bookmark)
      (sqlite:disconnect db))))

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
        (%bookmark-url selected-link)))))

(define-deprecated-command bookmark-anchor ()
  "Deprecated by `bookmark-hint'."
  (bookmark-hint))

(define-command set-url-from-bookmark ()
  "Set the URL for the current buffer from a bookmark."
  (with-result (url (read-from-minibuffer
                     (make-instance 'minibuffer
                                    :input-prompt "Open bookmark:"
                                    :completion-function 'bookmark-complete)))
    (set-url url :buffer (active-buffer *interface*) :raw-url-p t)))
