;;;; bookmark.lisp --- manage and create bookmarks

(in-package :next)

(defun initialize-bookmark-db ()
  "Create a databas file if necessary and make a table for bookmarks"
  (unless (probe-file "~/.next.d/bookmark.db")
    (with-open-file (p "~/.next.d/bookmark.db" :if-does-not-exist :create))
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

(defun bookmark-complete (input)
  (let* ((db
	  (sqlite:connect (truename (probe-file "~/.next.d/bookmark.db"))))
	 (candidates
	  (sqlite:execute-to-list
	   db "select url from bookmarks where url like ?"
	   (format nil "%~a%" input))))
    (sqlite:disconnect db)
    (reduce #'append candidates :from-end t)))

(define-key document-mode-map (kbd "C-s")
  (:input-complete set-url bookmark-complete))
(define-key document-mode-map (kbd "S-s") #'bookmark-current-page)
(initialize-bookmark-db)
