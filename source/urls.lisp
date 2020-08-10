(in-package :nyxt)

(export-always 'object-string)
(defmethod object-string ((uri quri:uri))
  (the (values string &optional) (quri:render-uri uri)))

(export-always 'object-display)
(defmethod object-display ((uri quri:uri))
    "Return decoded URL.
If the URL contains hexadecimal-encoded characters, return their unicode counterpart.
On errors, return URL."
    (the (values (or string null) &optional)
         (or (ignore-errors (quri:url-decode (quri:render-uri uri))) (quri:render-uri uri))))

(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           ;; We need to encode the search string to escape special characters.
           ;; Besides, we separate search patterns by a "+".
           (cl-ppcre:regex-replace-all "(%20)+" (quri:url-encode search-string) "+"))
         (url (format nil search-url encoded-search-string)))
    url))

(export-always 'valid-url-p)
(defun valid-url-p (url)
  (let ((uri (ignore-errors (quri:uri url))))
    (and uri
         (quri:uri-p uri)
         (not (uiop:emptyp (quri:uri-scheme uri)))
         ;; "http://" does not have a host.
         ;; A valid URL may have an empty domain, e.g. http://192.168.1.1.
         (quri:uri-host uri)
         ;; E.g. "http://algo" or "http://foo" have the same tld and host, which
         ;; is probably not a URI query.
         (not (string= (quri:uri-host uri)
                       (quri:uri-tld uri))))))

(declaim (ftype (function (t) quri:uri) ensure-url))
(defun ensure-url (thing)
  "Return `quri:uri' derived from THING.
If it cannot be derived, return an empty `quri:uri'."
  (the (values quri:uri &optional)
       (if (quri:uri-p thing)
           thing
           (or (ignore-errors (quri:uri thing))
               (quri:uri "")))))

(declaim (ftype (function ((or quri:uri string null)) boolean) url-empty-p))
(export-always 'url-empty-p)
(defun url-empty-p (url)
  "Small convenience function to check whether the given URL is empty."
  (the (values boolean &optional)
       (when url
         (uiop:emptyp (object-string url)))))

(declaim (ftype (function (quri:uri) boolean)
                empty-path-url-p host-only-url-p))
(export-always 'empty-path-url-p)
(defun empty-path-url-p (url)
  (or (string= (quri:uri-path url) "/")
      (null (quri:uri-path url))))

(export-always 'host-only-url-p)
(defun host-only-url-p (url)
  (every #'null
         (list (quri:uri-query url)
               (quri:uri-fragment url)
               (quri:uri-userinfo url))))

(declaim (ftype (function (quri:uri quri:uri) boolean) schemeless-uri=))
(defun schemeless-uri= (uri1 uri2)
  "Like `quri:uri=' but ignore scheme in comparison.
Authority is compared case-insensitively (RFC 3986)."
 (and (equal  (or (quri:uri-path uri1) "/") (or (quri:uri-path uri2) "/"))
      (equal  (quri:uri-query uri1)     (quri:uri-query uri2))
      (equal  (quri:uri-fragment uri1)  (quri:uri-fragment uri2))
      (equalp (quri:uri-authority uri1) (quri:uri-authority uri2))))

(declaim (ftype (function (quri:uri) string) schemeless-url))
(defun schemeless-url (uri)             ; Inspired by `quri:render-uri'.
  "Return URL without its scheme (e.g. it removes 'https://')."
  ;; Warning: We can't just set `quri:uri-scheme' to nil because that would
  ;; change the port (e.g. HTTP defaults to 80, HTTPS to 443).
  (format nil
          "~@[~(~A~)~]~@[~A~]~@[?~A~]~@[#~A~]"
          (quri:uri-authority uri)
          (or (quri:uri-path uri) "/")
          (quri:uri-query uri)
          (quri:uri-fragment uri)))

(declaim (ftype (function (quri:uri quri:uri) boolean) url<))
(defun uri< (uri1 uri2)
  "Like `string<' but ignore the URI scheme.
This way, HTTPS and HTTP is ignored when comparing URIs."
  (string< (schemeless-url uri1)
           (schemeless-url uri2)))

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

(defun default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the search engine with the 'default' shortcut, or the first one if
there is none."
  (or (find "default"
            search-engines :test #'string= :key #'shortcut)
      (first search-engines)))

(defun lisp-url (data)
  "Generate a Lisp URL from a string. This is useful for encoding
functionality into internal-buffers."
  (str:concat "lisp://" (quri:url-encode data)))

