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

(declaim (ftype (function (quri:uri) boolean) empty-url-p))
(export-always 'url-empty-p)
(defun url-empty-p (url)
  "Small convenience function to check whether the given URL is empty."
  (uiop:emptyp (object-string url)))

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
