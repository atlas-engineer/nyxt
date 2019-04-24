;;; history.lisp --- manage and create bookmarks

(in-package :next)

(defun initialize-history-db ()
  "Create a database file if necessary and make a table for bookmarks"
  (unless (probe-file *history-db-path*)
    (close (open (ensure-parent-exists *history-db-path*)
                 :direction :probe :if-does-not-exist :create))
    (let ((db (sqlite:connect
               (truename (probe-file *history-db-path*)))))
      (sqlite:execute-non-query
       db "create table history (id integer primary key, url text not null)")
      (sqlite:execute-non-query
       db "create table typed (id integer primary key, url text not null, unique (url) on conflict replace)")
      (sqlite:execute-non-query
       db "insert into history (url) values (?)" "about:blank")
      (sqlite:execute-non-query
       db "insert into typed (url) values (?)" "about:blank")
      (sqlite:disconnect db))))

(defun history-add (url)
  (let ((db (sqlite:connect
             (truename (probe-file *history-db-path*)))))
    (sqlite:execute-non-query
     db "insert into history (url) values (?)" url)
    (sqlite:disconnect db)))

(defun history-typed-add (url)
  (let ((db (sqlite:connect
             (truename (probe-file *history-db-path*)))))
    (sqlite:execute-non-query
     db "insert into typed (url) values (?)" url)
    (sqlite:disconnect db)))

(defun history-typed-complete (input)
  (let* ((db (sqlite:connect
              (truename (probe-file *history-db-path*))))
         (candidates
          (sqlite:execute-to-list
           db "select url from typed where url like ?"
           (format nil "%~a%" input))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))
