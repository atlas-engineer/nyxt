;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

(defun initialize-bookmark-db ()
  "Create a database file if necessary and make a table for bookmarks"
  (unless (probe-file *bookmark-db-path*)
    (close (open *bookmark-db-path* :direction :probe :if-does-not-exist :create))
    (let ((db (sqlite:connect
	       (truename (probe-file *bookmark-db-path*)))))
      (sqlite:execute-non-query
       db"create table bookmarks (id integer primary key, url text not null)")
      (sqlite:execute-non-query
       db "insert into bookmarks (url) values (?)" "about:blank")
      (sqlite:disconnect db))))

(defun bookmark-complete (input)
  (let* ((db (sqlite:connect
	      (truename (probe-file *bookmark-db-path*))))
	 (candidates
	  (sqlite:execute-to-list
	   db "select url from bookmarks where url like ?"
	   (format nil "%~a%" input))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))

(defun %bookmark-url (input)
  (let ((db (sqlite:connect
	     (truename (probe-file *bookmark-db-path*)))))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" input)
    (sqlite:disconnect db)))

(define-command bookmark-current-page ()
  "Bookmark the currently opened page in the active buffer."
  (let ((db (sqlite:connect
	     (truename (probe-file *bookmark-db-path*))))
	(url (name *active-buffer*)))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" url)
    (sqlite:disconnect db)))

(define-command bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result (url (read-from-minibuffer *minibuffer*))
    (%bookmark-url url)))

(define-command bookmark-url-from-buffer ()
  "Allow the user to bookmark a URL from the current active buffer."
  (let ((url (web-view-get-url *interface* (view *active-buffer*))))
    (%bookmark-url url)))

(define-command bookmark-delete ()
  "Delete a bookmark from the bookmark database."
  (with-result (bookmark (read-from-minibuffer
                          *minibuffer*
                          :completion-function 'bookmark-complete))
    (let ((db (sqlite:connect
               (truename (probe-file *bookmark-db-path*)))))
      (sqlite:execute-non-query
       db "delete from bookmarks where url = ?" bookmark)
      (sqlite:disconnect db))))

(define-command bookmark-anchor ()
  "Show link hints on screen, and allow the user to bookmark one"
  (with-result (selected-anchor (read-from-minibuffer
                                 *minibuffer*
                                 :setup-function 'setup-anchor
                                 :cleanup-function 'remove-link-hints))
    (loop for hint in (link-hints (mode *active-buffer*))
          do (when (equalp (nth 0 hint) selected-anchor)
               (%bookmark-url (nth 1 hint))))))
