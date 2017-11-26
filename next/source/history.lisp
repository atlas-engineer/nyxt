;;;; history.lisp --- manage and create bookmarks

(in-package :next)

(defun initialize-history-db ()
  "Create a database file if necessary and make a table for bookmarks"
  (unless (probe-file "~/.next.d/history.db")
    (close (open "~/.next.d/history.db" :direction :probe :if-does-not-exist :create))
    (let ((db (sqlite:connect
	       (truename (probe-file "~/.next.d/history.db")))))
      (sqlite:execute-non-query
       db"create table history (id integer primary key, url text not null)")
      (sqlite:execute-non-query
       db"create table typed (id integer primary key, url text not null, unique (url) on conflict replace)")
      (sqlite:execute-non-query
       db "insert into history (url) values (?)" "about:blank")
      (sqlite:execute-non-query
       db "insert into typed (url) values (?)" "about:blank")
      (sqlite:disconnect db))))

(defun history-add (url)
  (let ((db (sqlite:connect
	     (truename (probe-file "~/.next.d/history.db")))))
    (sqlite:execute-non-query
     db "insert into history (url) values (?)" url)
    (sqlite:disconnect db)))

(defun history-typed-add (url)
  (let ((db (sqlite:connect
	     (truename (probe-file "~/.next.d/history.db")))))
    (sqlite:execute-non-query
     db "insert into typed (url) values (?)" url)
    (sqlite:disconnect db)))
