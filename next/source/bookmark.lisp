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

(defun bookmark-current-page ()
  (let ((db (sqlite:connect
	     (truename (probe-file *bookmark-db-path*))))
	(url (name *active-buffer*)))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" url)
    (sqlite:disconnect db)))

(defcommand bookmark-url ()
  "Allow the user to bookmark a URL via minibuffer input."
  (with-result (url (read-from-minibuffer (mode *minibuffer*)))
    (%bookmark-url url)))

(defun %bookmark-url (input)
  (let ((db (sqlite:connect
	     (truename (probe-file *bookmark-db-path*)))))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" input)
    (sqlite:disconnect db)))

(defun bookmark-anchor (input)
  (loop for hint in (link-hints (mode *active-buffer*))
     do (when (equalp (nth 0 hint) input)
	  (%bookmark-url (nth 1 hint)))))

(defun bookmark-delete (input)
    (let ((db (sqlite:connect
	       (truename (probe-file *bookmark-db-path*)))))
      (sqlite:execute-non-query
       db "delete from bookmarks where url = ?" input)
      (sqlite:disconnect db)))

(defun bookmark-complete (input)
  (let* ((db (sqlite:connect
	      (truename (probe-file *bookmark-db-path*))))
	 (candidates
	  (sqlite:execute-to-list
	   db "select url from bookmarks where url like ?"
	   (format nil "%~a%" input))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))
