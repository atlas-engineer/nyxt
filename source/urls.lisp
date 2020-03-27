;;; urls.lisp --- utility classes and functions for hanlding urls.

(in-package :next)

(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           ;; We need to encode the search string to escape special characters.
           ;; Besides, we separate search patterns by a "+".
           (cl-ppcre:regex-replace-all "(%20)+" (quri:url-encode search-string) "+"))
         (url (format nil search-url encoded-search-string)))
    url))

(defun bookmark-search-engines (&optional (bookmarks (bookmarks-data *browser*)))
  (mapcar (lambda (b)
            (list
             (shortcut b)
             (if (quri:uri-scheme (quri:uri (search-url b)))
                 (search-url b)
                 (str:concat (url b) (search-url b)))
             (url b)))
          (remove-if (lambda (b) (or (str:emptyp (search-url b))
                                     (str:emptyp (shortcut b))))
                     bookmarks)))

(defun parse-url (input-url)
  "From user input, return the full url to visit.

If the first word references a search engine, generate a search query.
If the input starts with an uri scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid uri, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((search-engines (append (search-engines *browser*)
                                 (bookmark-search-engines)))
         (terms (str:split " " input-url :omit-nulls t))
         (engine (assoc (first terms)
                        search-engines :test #'string=))
         (default (assoc "default"
                         search-engines :test #'string=)))
    (if engine
        (let ((new-input (str:join " " (rest terms))))
          (if (and (listp (rest engine))
                   (str:emptyp new-input))
              (third engine)
              (generate-search-query new-input
                                     (if (atom (rest engine))
                                         (rest engine)
                                         (second engine)))))
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
            ((let ((uri (ignore-errors
                         (quri:uri (str:concat "https://" input-url)))))
               (and uri
                    (quri:uri-p uri)
                    ;; E.g. "http://foo" has an empty domain, so it's probably
                    ;; not a URI query.
                    (quri:uri-domain uri)
                    ;; E.g. "http://algo" have the same tld and domain, which is
                    ;; probably not a URI query.
                    (not (string= (quri:uri-domain uri)
                                  (quri:uri-tld uri)))))
             (str:concat "https://" input-url))
            (t (generate-search-query input-url
                                      (if (atom (rest default))
                                          (rest default)
                                          (second default)))))))))
