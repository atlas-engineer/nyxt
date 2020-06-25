(in-package :nyxt)

(declaim (ftype (function (string) (values (or quri:uri null) t &optional)) parse-url))
(defun parse-url (input-url)
  "From user input, return the full URL to visit.

If the first word references a search engine, generate a search query.
If the input starts with an URI scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid URI, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((search-engines (all-search-engines))
         (terms (str:split " " input-url :omit-nulls t))
         (engine (find (first terms)
                       search-engines :test #'string= :key #'shortcut)))
    (if engine
        (let ((new-input (str:join " " (rest terms))))
          (if (and (not (str:emptyp (fallback-url engine)))
                   (str:emptyp new-input))
              (quri:uri (fallback-url engine))
              (quri:uri (generate-search-query new-input (search-url engine)))))
        (let ((recognized-scheme (ignore-errors (quri:uri-scheme (quri:uri input-url)))))
          (cond
            ((str:starts-with? "magnet:" input-url)
             (log:debug "Open magnet link with external application.")
             (ignore-errors
              (uiop:launch-program (list "xdg-open" input-url))
              (nyxt/minibuffer-mode:cancel-input)))
            ((and recognized-scheme
                  (not (string= "file" recognized-scheme)))
             (quri:uri input-url))
            ((or (string= "file" recognized-scheme)
                 (uiop:file-exists-p input-url))
             (quri:uri (if (string= "file" recognized-scheme)
                           input-url
                           (format nil "file://~a"
                                   (uiop:ensure-absolute-pathname
                                    input-url *default-pathname-defaults*)))))
            ((valid-url-p (str:concat "https://" input-url))
             (quri:uri (str:concat "https://" input-url)))
            (t (match (default-search-engine search-engines)
                 (nil (quri:uri input-url))
                 (default (quri:uri (generate-search-query input-url (search-url default)))))))))))
