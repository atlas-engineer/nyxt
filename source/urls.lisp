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

(defun bookmark-search-engines (&optional (bookmarks (bookmarks-data *browser*)))
  (mapcar (lambda (b)
            (make-instance 'search-engine
                           :shortcut (shortcut b)
                           :search-url (if (quri:uri-scheme (quri:uri (search-url b)))
                                           (search-url b)
                                           (str:concat (object-display (url b)) (search-url b)))
                           :fallback-url (object-string (url b))))
          (remove-if (lambda (b) (or (str:emptyp (search-url b))
                                     (str:emptyp (shortcut b))))
                     bookmarks)))

(export-always 'valid-url-p)
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

(defun all-search-engines ()
  "Return the `search-engines' from the `browser' instance plus those in
bookmarks."
  (when *browser*
    (append (search-engines *browser*)
            (bookmark-search-engines))))

(defun default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the search engine with the 'default' shortcut, or the first one if
there is none."
  (or (find "default"
            search-engines :test #'string= :key #'shortcut)
      (first search-engines)))
