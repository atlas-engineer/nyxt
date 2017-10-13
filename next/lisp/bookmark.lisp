;;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

(defun initialize-bookmark-db ()
  "Create a database file if necessary and make a table for bookmarks"
  (unless (probe-file "~/.next.d/bookmark.db")
    (close (open "~/.next.d/bookmark.db" :direction :probe :if-does-not-exist :create))
    (let ((db (sqlite:connect
	       (truename (probe-file "~/.next.d/bookmark.db")))))
      (sqlite:execute-non-query
       db"create table bookmarks (id integer primary key, url text not null)")
      (sqlite:execute-non-query
       db "insert into bookmarks (url) values (?)" "about:blank")
      (sqlite:disconnect db))))

(defun bookmark-current-page ()
  (let ((db (sqlite:connect
	     (truename (probe-file "~/.next.d/bookmark.db"))))
	(url (buffer-name *active-buffer*)))
    (sqlite:execute-non-query
     db "insert into bookmarks (url) values (?)" url)
    (sqlite:disconnect db)))

(defun bookmark-delete (input)
    (let ((db (sqlite:connect
	       (truename (probe-file "~/.next.d/bookmark.db")))))
      (sqlite:execute-non-query
       db "delete from bookmarks where url = ?" input)
      (sqlite:disconnect db)))

(defun bookmark-complete (input)
  (let* ((db
	  (sqlite:connect (truename (probe-file "~/.next.d/bookmark.db"))))
	 (candidates
	  (sqlite:execute-to-list
	   db "select url from bookmarks where url like ?"
	   (format nil "%~a%" input))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))
