;;; history.lisp --- manage and create bookmarks

(in-package :next)

(defun %initialize-history-db (path)
  "Create a database file if necessary and make a table for bookmarks."
  (close (open (ensure-parent-exists path)
               :direction :probe :if-does-not-exist :create))
  (let ((db (sqlite:connect
             (truename (probe-file path)))))
    (sqlite:execute-non-query
     db "create table history (id integer primary key, url text not null)")
    (sqlite:execute-non-query
     db "create table typed (id integer primary key, url text not null, visits integer default 1, unique (url) on conflict replace)")
    (sqlite:execute-non-query
     db "insert into history (url) values (?)" "about:blank")
    (sqlite:execute-non-query
     db "insert into typed (url) values (?)" "about:blank")
    (sqlite:disconnect db)))

(defun ensure-history-db ()
  "Return the pathname of the history database."
  (let* ((path (if (rpc-window-active *interface*)
                   (history-db-path (rpc-window-active *interface*))
                   ;; This additional window fallback should not be necessary
                   ;; anymore now that `rpc-window-make' sets `last-active-window'
                   ;; so that `rpc-window-active' always returns a result.
                   (some (lambda (window)
                           (history-db-path window))
                         (alexandria:hash-table-values (windows *interface*))))))
    (if (probe-file path)
        path
        (ensure-file-exists path #'%initialize-history-db))))

(defun history-typed-add (url)
  "Add this url to the history, increment its number of visits."
  (let* ((db (sqlite:connect (ensure-history-db)))
         ;; Would be better done in one query,
         ;; but without "insert ... on conflict" which was added in sqlite 3.24.
         (visits (or (sqlite:execute-single db "select visits from typed where url like ? limit 1;" url)
                     0))
         (visits (incf visits)))
    (sqlite:execute-non-query
     db "insert or replace into typed (url, visits) values (?, ?)" url visits)
    (sqlite:disconnect db)))

(defun history-typed-complete (input)
  (let* ((db (sqlite:connect (ensure-history-db)))
         (candidates
          (sqlite:execute-to-list
           db "select url from typed where url like ? order by visits desc"
           (format nil "%~a%" (cl-strings:replace-all input " " "%")))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))
