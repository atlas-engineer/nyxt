;;; urls.lisp --- utility classes and functions for hanlding urls.

(in-package :next)
(annot:enable-annot-syntax)

(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           ;; We need to encode the search string to escape special characters.
           ;; Besides, we separate search patterns by a "+".
           (cl-ppcre:regex-replace-all "(%20)+" (quri:url-encode search-string) "+"))
         (url (format nil search-url encoded-search-string)))
    url))

(defun parse-url (input-url)
  "From user input, return the full url to visit.

If the first word references a search engine, generate a search query.
If the input starts with an uri scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid uri, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((engine (assoc (first (str:split " " input-url))
                        (search-engines *interface*) :test #'string=))
         (default (assoc "default"
                         (search-engines *interface*) :test #'string=)))
    (if engine
        (generate-search-query
         (subseq input-url
                 (length (first (str:split " " input-url))))
         (rest engine))
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
            (t (generate-search-query input-url (rest default))))))))
