;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'url)
(defmethod url ((url quri:uri))
  url)

(defmethod url ((url-string string))
  (quri:uri url-string))

(defun string->url (url-string)
  "Convert URL-STRING to its corresponding `quri:uri' object.
If URL-STRING is a path to an existing file, return a `quri:uri-file' object.
If the conversion fails, a `quri:uri' object is always returned."
  (or (ignore-errors
       (sera:and-let* ((path (uiop:merge-pathnames* url-string (uiop:getcwd)))
                       (exist-p (uiop:file-exists-p path)))
         (quri.uri.file:make-uri-file :path path)))
      (quri:uri url-string)
      (quri:uri "")))

(defun strings->urls (url-strings)
  "Convert URL-STRINGS to a list of its corresponding `quri:uri' objects.
Members of URL-STRINGS corresponding to the empty URL are discarded."
  ;; how to define the empty URL?
  (remove-if #'url-empty-p (mapcar #'string->url url-strings)))

(defun has-url-method-p (object)
  "Return non-nil if OBJECT has `url' specialization."
  (has-method-p object #'url))

(deftype url-designator ()
  "Type for anything URL-like.
Means that `url' can be applied to it to get `quri:uri'."
  `(satisfies has-url-method-p))

(export-always 'render-url)
(-> render-url (url-designator) t)
(defun render-url (url)
  "Return the aesthetic decoded URL.

If the URL contains hexadecimal-encoded characters, return their Unicode
counterpart, unless there are unprintable characters.

If URL contains non-ascii chars in domain part (IDN), return two values:
- The aesthetic decoded URL, and
- The safe punicode-encoded one."
  (let* ((initial-url (url url))        ; Ensure `quri:uri'.
         (url (quri:render-uri
               (quri:copy-uri initial-url :query (ignore-errors (quri:url-decode (quri:uri-query initial-url))))))
         (displayed (or (ignore-errors (ffi-display-url *browser* url))
                        url)))
    (cond
      ((every (alex:conjoin #'graphic-char-p #'sera:ascii-char-p) displayed)
       displayed)
      ((and (every #'sera:ascii-char-p displayed)
            (notevery #'graphic-char-p displayed))
       (quri:render-uri initial-url))
      (t
       (values displayed (let ((uri (quri:uri displayed)))
                           (quri:render-uri (quri:make-uri :host (if (quri:uri-host uri)
                                                                     (idna:to-ascii (quri:uri-host uri))
                                                                     "")
                                                           :defaults uri))))))))

(export-always 'fetch-url-title)
(defun fetch-url-title (url)
  "Return URL's title.
The URL is fetched, which explains possible bottlenecks."
  (ignore-errors (plump:text (aref (clss:select "title" (plump:parse (dex:get url))) 0))))

(export-always 'error-help)
(defun error-help (&optional (title "Unknown error") (text ""))
  "A helper to print error messages as displayable HTML."
  (values
   (spinneret:with-html-string
     (:head
      (:title title)
      (:style (:raw (style (current-buffer)))))
     (:body
      (:h1 title)
      (:pre text)))
   "text/html;charset=utf8"))

(export-always 'renderer-scheme)
(defclass renderer-scheme ()
  ()
  (:metaclass interface-class)
  (:documentation "Renderer-specific representation of the custom scheme.
Should be redefined by the renderer."))

(define-class scheme (renderer-scheme)
  ((name
    (alex:required-argument 'name)
    :documentation "The custom scheme name to handle.
HTTPS or FILE are examples of schemes.")
   (callback
    nil
    :type (or null function)
    :documentation "A function called on URL load that returns the page contents.

It takes the URL as an argument and returns up to 5 values:
- The data for page contents (either as string or as a unsigned byte array)
- The MIME type for the contents
- The status code for the request
- An alist of headers for the request
- A status reason phrase")
   (error-callback
    nil
    :type (or null function)
    :documentation "Callback to use when a condition is signaled.

Accepts only one argument: the signaled condition."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Representation of Nyxt-specific internal schemes.
Has `name' it can be accessed with. When accessed, runs `callback' to return
content. In case something goes wrong, runs `error-callback'.")
  (:metaclass user-class))

(defmethod print-object ((scheme scheme) stream)
  (print-unreadable-object (scheme stream :type t :identity t)
    (format stream "~a" (name scheme))))

(defvar *schemes* (sera:dict)
  "A table of internal schemes registered in Nyxt.
It maps scheme names as strings to `scheme' objects.")

(export-always 'define-internal-scheme)
(defun define-internal-scheme (scheme-name callback &optional error-callback)
  "Define handler CALLBACK for SCHEME-NAME `scheme'.

See the `callback' and `error-callback' slot documentation for their type
signatures."
  (setf (gethash scheme-name *schemes*)
        (list callback error-callback)))

(defmemo lookup-hostname (name)
  "Resolve hostname NAME and memoize the result."
  ;; `sb-bsd-sockets:get-host-by-name' may signal a `ns-try-again-condition' which is
  ;; not an error, so we can't use `ignore-errors' here.
  (handler-case
      #+sbcl
    (sb-bsd-sockets:get-host-by-name name)
    #-sbcl
    (iolib/sockets:lookup-hostname name)
    (t () nil)))

(export-always 'valid-tld-p)
(defun valid-tld-p (hostname)
  "Return NIL if HOSTNAME does not include a valid TLD as determined by the
Public Suffix list, T otherwise."
  (sera:true (cl:ignore-errors (cl-tld:get-tld hostname))))

(export-always 'browser-schemes)
(defgeneric browser-schemes (browser)
  (:method-combination append)
  (:documentation "Return a list of schemes supported by a browser"))

;; Set specifier to T because *BROWSER* can be bound to NIL
(defmethod browser-schemes append ((browser t))
  (let ((nyxt-schemes (append '("blob" "javascript") (alex:hash-table-keys *schemes*)))
        ;; List of URI schemes: https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
        ;; Last updated 2020-08-26.
        (iana-schemes
         '("aaa" "aaas" "about" "acap" "acct" "cap" "cid" "coap" "coap+tcp" "coap+ws"
           "coaps" "coaps+tcp" "coaps+ws" "crid" "data" "dav" "dict" "dns" "example" "file"
           "ftp" "geo" "go" "gopher" "h323" "http" "https" "iax" "icap" "im" "imap" "info"
           "ipp" "ipps" "iris" "iris.beep" "iris.lwz" "iris.xpc" "iris.xpcs" "jabber"
           "ldap" "leaptofrogans" "mailto" "mid" "msrp" "msrps" "mtqp" "mupdate" "news"
           "nfs" "ni" "nih" "nntp" "opaquelocktoken" "pkcs11" "pop" "pres" "reload" "rtsp"
           "rtsps" "rtspu" "service" "session" "shttp" "sieve" "sip" "sips" "sms" "snmp"
           "soap.beep" "soap.beeps" "stun" "stuns" "tag" "tel" "telnet" "tftp"
           "thismessage" "tip" "tn3270" "turn" "turns" "tv" "urn" "vemmi" "vnc" "ws" "wss"
           "xcon" "xcon-userid" "xmlrpc.beep" "xmlrpc.beeps" "xmpp" "z39.50r" "z39.50s"))
        ;; https://www.iana.org/assignments/special-use-domain-names/special-use-domain-names.xml
        ;; TODO: Remove when https://github.com/lu4nx/cl-tld/issues/2 is fixed.
        (special-use-schemes
         '("example" "invalid" "local" "localhost" "onion" "test")))
    (append nyxt-schemes iana-schemes special-use-schemes)))

(export-always 'valid-scheme-p)
(defun valid-scheme-p (scheme)
  "Whether the scheme is supported by Nyxt.
Usually means that either:
- SCHEME is IANA-approved,
- or SCHEME is a Nyxt-specific `scheme'."
  (sera:true (find scheme (browser-schemes *browser*) :test #'string=)))

(export-always 'valid-url-p)
(defun valid-url-p (url &key (check-dns-p t))
  "Return non-nil when URL is a valid URL.
The domain name existence is verified only if CHECK-DNS-P is T. Domain name
validation may take significant time since it looks up the DNS."
  (let ((%url (ignore-errors (quri:uri url))))
    (and %url
         (valid-scheme-p (quri:uri-scheme %url))
         ;; `new-url-query' automatically falls back to HTTPS if it makes for
         ;; a valid URL:
         (or (not (quri:uri-http-p %url))
             (and
              ;; "http:/https://www.iana.org/assignments/special-use-domain-names/special-use-domain-names.xml/" does not have a host.
              ;; A valid URL may have an empty domain, e.g. http://192.168.1.1.
              (quri:uri-host %url)
              (or
               (not check-dns-p)
               (valid-tld-p (quri:uri-host %url))
               ;; "http://algo" has the "algo" hostname but it's probably invalid
               ;; unless it's found on the local network.  We also need to
               ;; support "localhost" and the current system hostname.
               (or (quri:ip-addr-p (quri:uri-host %url))
                   (lookup-hostname (quri:uri-host %url)))))))))

(-> ensure-url (t) quri:uri)
(export-always 'ensure-url)
(defun ensure-url (thing)
  "Return `quri:uri' derived from THING.
If it cannot be derived, return an empty `quri:uri'."
  (the (values quri:uri &optional)
       (if (quri:uri-p thing)
           thing
           (or (ignore-errors (quri:uri thing))
               (quri:uri "")))))

(-> url-empty-p ((or quri:uri string null)) boolean)
(export-always 'url-empty-p)
(defun url-empty-p (url)
  "Check whether the given URL is empty (renders to empty string)."
  (the (values boolean &optional)
       (uiop:emptyp (if (quri:uri-p url) (quri:render-uri url) url))))

(-> empty-path-url-p (quri:uri) boolean)
(export-always 'empty-path-url-p)
(defun empty-path-url-p (url)
  "Whether the URL is a root one, having no path or an empty path."
  (or (string= (quri:uri-path url) "/")
      (null (quri:uri-path url))))

(-> host-only-url-p (quri:uri) boolean)
(export-always 'host-only-url-p)
(defun host-only-url-p (url)
  "Check whether the URL only has a host, and none other URL elements."
  (every #'null
         ;; FIXME: Check path too? `empty-path-url-p'?
         (list (quri:uri-query url)
               (quri:uri-fragment url)
               (quri:uri-userinfo url))))

(-> schemeless-url (quri:uri) string)
(defun schemeless-url (url)             ; Inspired by `quri:render-uri'.
  "Return URL without its scheme (e.g. it removes 'https://')."
  ;; Warning: We can't just set `quri:uri-scheme' to nil because that would
  ;; change the port (e.g. HTTP defaults to 80, HTTPS to 443).
  (format nil
          "~@[~A~]~@[~A~]~@[?~A~]~@[#~A~]"
          (quri:uri-authority url)
          (or (quri:uri-path url) "/")
          (quri:uri-query url)
          (quri:uri-fragment url)))

(export-always 'url<)
(-> url< (quri:uri quri:uri) (or null fixnum))
(defun url< (url1 url2)
  "Like `string<' but ignore the URL scheme.
This way, HTTPS and HTTP is ignored when comparing URLs."
  (string< (schemeless-url url1)
           (schemeless-url url2)))

(export-always 'url-equal)
(-> url-equal (quri:uri quri:uri) boolean)
(defun url-equal (url1 url2)
  "Like `quri:uri=' but ignoring the scheme.
URLs are equal up to `scheme='.
Authority is compared case-insensitively (RFC 3986)."
  (the (values boolean &optional)
       (url-eqs url1
                url2
                (list #'scheme=
                      (lambda (url1 url2) (equal (or (quri:uri-path url1) "/")
                                                 (or (quri:uri-path url2) "/")))
                      (lambda (url1 url2) (equal (quri:uri-query url1)
                                                 (quri:uri-query url2)))
                      (lambda (url1 url2) (equal (quri:uri-fragment url1)
                                                 (quri:uri-fragment url2)))
                      (lambda (url1 url2) (equalp (quri:uri-authority url1)
                                                  (quri:uri-authority url2)))))))

(-> symbol->param-name (symbol) string)
(defun symbol->param-name (symbol)
  "Turn the provided SYMBOL into a reasonable query URL parameter name."
  (let ((*package* (find-package :nyxt))
        (*print-case* :downcase))
    (if (keywordp symbol)
        (format nil "~(~a~)" symbol)
        (format nil "~s" symbol))))

(-> value->param-value (t) (values string &optional))
(defun value->param-value (value)
  "Turn VALUE into a representation readable by `query-params->arglist'."
  (if (stringp value)
      value
      ;; As to notify query-params->arglist.
      (str:concat +escape+ (let ((*package* (find-package :nyxt)))
                             (prin1-to-string value)))))

(export-always 'nyxt-url)
(-> nyxt-url (t &rest t &key &allow-other-keys) string)
(defun nyxt-url (fn &rest args &key &allow-other-keys)
  "Return a nyxt scheme URL encoding a call to FN with ARGS.
It is useful to request `internal-page's.

ARGS is an arbitrary keyword arguments list that is translated to a URL query."
  (let* ((query (quri:url-encode-params (mapcar (lambda (pair)
                                                  (cons (symbol->param-name (first pair))
                                                        (value->param-value (rest pair))))
                                                (alexandria:plist-alist args))
                                        :space-to-plus t))
         (url (quri:render-uri
               (quri:make-uri
                :scheme "nyxt"
                :path (symbol->param-name fn)
                :query (unless (uiop:emptyp query) query)))))
    (if (internal-page-symbol-p fn)
        url
        (error "URL ~a is undefined." url))))

(export-always 'internal-page-name)
(-> internal-page-name ((or string quri:uri)) t)
(defun internal-page-name (url)
  (alex:when-let* ((%url (quri:uri url))
                   (_ (string= "nyxt" (quri:uri-scheme %url))))
    ;; As to account for nyxt:foo and nyxt://foo.
    (uiop:safe-read-from-string (str:upcase (or (quri:uri-path %url)
                                                (quri:uri-host %url)))
                                :package :nyxt)))

(export-always 'internal-url-p)
(defun internal-url-p (url)
  "Whether the URL is the `internal-page' one."
  ;; FIXME: Too simple. Maybe check for command presence too?
  (or (string= "nyxt" (quri:uri-scheme (url url)))
      (string= "editor" (quri:uri-scheme (url url)))))

(-> query-params->arglist ((types:association-list string string)) (values list &optional))
(defun query-params->arglist (params)
  "Process the PARAMS (an alist of strings, as returned by QURI) to a regular Lisp argument plist."
  (mappend (lambda (pair)
             (let* ((key (intern (str:upcase (first pair)) :keyword))
                    (value (if (str:starts-with-p +escape+ (rest pair))
                               (uiop:safe-read-from-string (subseq (rest pair) 1)
                                                           :package (find-package :nyxt))
                               (rest pair))))
               (list key value)))
           params))

(define-internal-scheme "nyxt"
    (lambda (url buffer)
      (with-protect ("Error while processing the \"nyxt:\" URL: ~a" :condition)
        (let ((url (quri:uri (str:replace-first "://" ":" url))))
          (log:debug "Internal page ~a requested." url)
          (multiple-value-bind (internal-page-name args)
              (parse-nyxt-url url)
            (when (and internal-page-name)
              (alex:when-let ((internal-page (gethash internal-page-name *nyxt-url-commands*)))
                (setf (title buffer) (apply #'dynamic-title internal-page args))
                ;; FIXME: This allows `find-internal-page-buffer' to find the
                ;; buffer and `form' to have this buffer as the buffer-var.
                (setf (url buffer) (quri:uri url))
                (apply (form internal-page) args))))))))

(define-internal-scheme "nyxt-resource"
    (lambda (url buffer)
      (declare (ignore buffer))
      (nth-value 0 (gethash (quri:uri-path (url url)) *static-data*)))
  :secure-p t)

(-> lisp-url (&rest t &key (:id string)
              (:buffer t)
              (:callback (or function symbol))
              (:title (maybe string)))
    (values quri:uri &optional))
(defun lisp-url (&key (id (princ-to-string (nyxt:new-id)))
                   (buffer (alex:required-argument 'buffer))
                   (callback (alex:required-argument 'callback))
                   title)
  (sera:synchronized ((nyxt::lisp-url-callbacks buffer))
    (log:debug "Registering callback ~a in buffer ~a" id buffer)
    (setf (gethash id (nyxt::lisp-url-callbacks buffer)) callback))
  (quri:make-uri :scheme "lisp"
                 :path id
                 :query `(("title" . ,title) ("buffer" . ,(id buffer)))))

(export-always 'nyxt/ps::lisp-eval :nyxt/ps)
(ps:defpsmacro nyxt/ps::lisp-eval ((&key (buffer '(nyxt:current-buffer)) title) &body body)
  "Request a URL that evaluates BODY in BUFFER.
TITLE is purely informative."
  `(let ((promise
           (fetch (ps:lisp
                   (quri:render-uri
                    (lisp-url :buffer ,buffer
                              :title ,title
                              :callback ,(if (and (sera:single body)
                                                  (member (first (first body)) '(lambda function)))
                                             (first body)
                                             `(lambda () ,@body)))))
                  (ps:create :mode "no-cors"))))))

(define-internal-scheme "lisp"
    (lambda (url buffer)
      (declare (ignore buffer))
      (alex:when-let* ((%url (quri:uri url))
                       (request-id (quri:uri-path %url))
                       (query (quri:uri-query-params %url))
                       (title (alex:assoc-value query "title" :test 'equal))
                       (buffer-id (alex:assoc-value query "buffer" :test 'equal))
                       (buffer (find (read-from-string buffer-id) (internal-buffers) :key 'id)))
        (log:debug "Evaluate Lisp callback ~a in buffer ~a: ~a" request-id buffer title)
        (values
         (alex:if-let ((callback (sera:synchronized ((lisp-url-callbacks buffer))
                                   (gethash request-id (lisp-url-callbacks buffer)))))
           (let ((callback-output (with-current-buffer buffer (run callback))))
             ;; Objects and other complex structures make cl-json choke.
             ;; TODO: Maybe encode it to the format that `cl-json' supports,
             ;; then we can override the encoding and decoding methods and allow
             ;; arbitrary objects (like buffers) in the nyxt:// URL arguments..
             (when (or (scalar-p callback-output)
                       (and (sequence-p callback-output)
                            (every #'scalar-p callback-output)))
               (j:encode callback-output)))
           (log:warn "Request ~a isn't bound to a callback in buffer ~a" %url buffer))
         "application/json")))
  :cors-enabled-p t
  :error-callback (lambda (c) (log:debug "Error when evaluating lisp URL: ~a" c)))

(-> path= (quri:uri quri:uri) boolean)
(defun path= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same path."
  ;; See https://github.com/fukamachi/quri/issues/48.
  (equalp (string-right-trim "/" (or (quri:uri-path url1) ""))
          (string-right-trim "/" (or (quri:uri-path url2) ""))))

(-> scheme= (quri:uri quri:uri) boolean)
(defun scheme= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same scheme.
HTTP and HTTPS belong to the same equivalence class."
  (or (equalp (quri:uri-scheme url1) (quri:uri-scheme url2))
      (and (quri:uri-http-p url1) (quri:uri-http-p url2))))

(-> domain= (quri:uri quri:uri) boolean)
(defun domain= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same domain."
  (equalp (quri:uri-domain url1) (quri:uri-domain url2)))

(-> host= (quri:uri quri:uri) boolean)
(defun host= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same host.
This is a more restrictive requirement than `domain='."
  (equalp (quri:uri-host url1) (quri:uri-host url2)))

(-> url-eqs (quri:uri quri:uri list) boolean)
(defun url-eqs (url1 url2 eq-fn-list)
  "Return non-nil when URL1 and URL2 are \"equal\" as dictated by EQ-FN-LIST.

EQ-FN-LIST is a list of functions that take URL1 and URL2 as arguments and
return a boolean.  It defines an equivalence relation induced by EQ-FN-LIST.
`quri:uri=' and `url-equal' are examples of equivalence relations."
  ;; (and (fn1 url1 url2) (fn2 url1 url2) ...) stops as soon as any fn returns
  ;; nil, unlike the solution below.
  (every #'identity (mapcar (lambda (fn) (funcall fn url1 url2)) eq-fn-list)))

(-> match-scheme (string &rest string) (function (quri:uri) boolean))
(export-always 'match-scheme)
(defun match-scheme (scheme &rest other-schemes)
  "Return a predicate for URL designators matching one of SCHEME or OTHER-SCHEMES."
  #'(lambda (url-designator)
      (when url-designator
        (some (curry #'string= (quri:uri-scheme (url url-designator)))
              (cons scheme other-schemes)))))

(-> match-host (string &rest string) (function (quri:uri) boolean))
(export-always 'match-host)
(defun match-host (host &rest other-hosts)
  "Return a predicate for URL designators matching one of HOST or OTHER-HOSTS."
  #'(lambda (url-designator)
      (when url-designator
        (some (curry #'string= (quri:uri-host (url url-designator)))
              (cons host other-hosts)))))

(-> match-domain (string &rest string) (function (quri:uri) boolean))
(export-always 'match-domain)
(defun match-domain (domain &rest other-domains)
  "Return a predicate for URL designators matching one of DOMAIN or OTHER-DOMAINS."
  #'(lambda (url-designator)
      (when url-designator
        (some (curry #'string= (quri:uri-domain (url url-designator)))
              (cons domain other-domains)))))

(-> match-port (integer &rest integer) (function (quri:uri) boolean))
(export-always 'match-port)
(defun match-port (port &rest other-ports)
  "Return a predicate for URL designators matching one of PORT or OTHER-PORTS."
  #'(lambda (url-designator)
      (when url-designator
        (some (curry #'eq (quri:uri-port (url url-designator)))
              (cons port other-ports)))))

(-> match-file-extension (string &rest string) (function (quri:uri) boolean))
(export-always 'match-file-extension)
(defun match-file-extension (extension &rest other-extensions)
  "Return a predicate for URL designators matching one of EXTENSION or OTHER-EXTENSIONS."
  #'(lambda (url-designator)
      (when url-designator
        (some (curry #'string= (pathname-type (or (quri:uri-path (url url-designator)) "")))
              (cons extension other-extensions)))))

(-> match-regex (string &rest string) (function (quri:uri) boolean))
(export-always 'match-regex)
(defun match-regex (regex &rest other-regex)
  "Return a predicate for URL designators matching one of REGEX or OTHER-REGEX."
  #'(lambda (url-designator)
      (when url-designator
        (some (rcurry #'cl-ppcre:scan (render-url (url url-designator)))
              (cons regex other-regex)))))

(-> match-url (string &rest string) (function (quri:uri) boolean))
(export-always 'match-url)
(defun match-url (one-url &rest other-urls)
  "Return a predicate for URLs exactly matching ONE-URL or OTHER-URLS."
  #'(lambda (url-designator)
      (when url-designator
        (some (rcurry #'string= (render-url (url url-designator)))
              (mapcar (lambda (u) (quri:url-decode u :lenient t))
                      (cons one-url other-urls))))))
