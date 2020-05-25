;;; urls.lisp --- utility classes and functions for handling urls.

(in-package :next)

(export-always 'host)
(declaim (ftype (function (string) (or string null)) host))
(defun host (url)
  "Return URL's host.
Return NIL on error."
  (ignore-errors (quri:uri-host (quri:uri url))))

(export-always 'domain)
(declaim (ftype (function (string) (or string null)) domain))
(defun domain (url)
  "Return URL's domain.
Return NIL on error."
  (ignore-errors (the (values string &optional) (quri:uri-domain (quri:uri url)))))

(defun url-display (url)
  "Return decoded URL.
If the URL contains hexadecimal-encoded characters, return their unicode counterpart.
On errors, return URL."
  (or (ignore-errors (quri:url-decode url)) url))

(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           ;; We need to encode the search string to escape special characters.
           ;; Besides, we separate search patterns by a "+".
           (cl-ppcre:regex-replace-all "(%20)+" (quri:url-encode search-string) "+"))
         (url (format nil search-url encoded-search-string)))
    url))

(defun bookmark-search-engines (&optional (bookmarks (bookmarks-data *browser*)))
  (mapcar (lambda (b)
            (make-instance 'search-engine
                           :shortcut (shortcut b)
                           :search-url (if (quri:uri-scheme (quri:uri (search-url b)))
                                           (search-url b)
                                           (str:concat (url b) (search-url b)))
                           :fallback-url (url b)))
          (remove-if (lambda (b) (or (str:emptyp (search-url b))
                                     (str:emptyp (shortcut b))))
                     bookmarks)))

(defun valid-url-p (url)
  (let ((uri (ignore-errors (quri:uri url))))
    (and uri
         (quri:uri-p uri)
         ;; E.g. "http://foo" has an empty domain, so it's probably
         ;; not a URI query.
         (quri:uri-domain uri)
         ;; E.g. "http://algo" have the same tld and domain, which is
         ;; probably not a URI query.
         (not (string= (quri:uri-domain uri)
                       (quri:uri-tld uri))))))

(defun all-search-engines ()
  "Return the `search-engines' from the `browser' instance plus those in
bookmarks."
  (append (search-engines *browser*)
          (bookmark-search-engines)))

(defun default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the search engine with the 'default' shortcut, or the first one if
there is none."
  (or (find "default"
            search-engines :test #'string= :key #'shortcut)
      (first search-engines)))

(defun parse-url (input-url)
  "From user input, return the full url to visit.

If the first word references a search engine, generate a search query.
If the input starts with an uri scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid uri, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((search-engines (all-search-engines))
         (terms (str:split " " input-url :omit-nulls t))
         (engine (find (first terms)
                       search-engines :test #'string= :key #'shortcut)))
    (if engine
        (let ((new-input (str:join " " (rest terms))))
          (if (and (not (str:emptyp (fallback-url engine)))
                   (str:emptyp new-input))
              (fallback-url engine)
              (generate-search-query new-input (search-url engine))))
        (let ((recognized-scheme (ignore-errors (quri:uri-scheme (quri:uri input-url)))))
          (cond
            ((str:starts-with? "magnet:" input-url)
             (log:debug "Open magnet link with external application.")
             (ignore-errors
              (uiop:launch-program (list "xdg-open" input-url))
              (cancel-input)))
            ((and recognized-scheme
                  (not (string= "file" recognized-scheme)))
             input-url)
            ((or (string= "file" recognized-scheme)
                 (uiop:file-exists-p input-url))
             (if (string= "file" recognized-scheme)
                 input-url
                 (format nil "file://~a"
                         (uiop:ensure-absolute-pathname input-url *default-pathname-defaults*))))
            ((valid-url-p (str:concat "https://" input-url))
             (str:concat "https://" input-url))
            (t (match (default-search-engine search-engines)
                 (nil input-url)
                 (default (generate-search-query input-url (search-url default))))))))))
