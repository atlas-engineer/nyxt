;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; A URI is defined by the following components:
;; URI = scheme ":" ["//" authority] path ["?" query] ["#" fragment]
;; authority = [userinfo "@"] host [":" port]

(defparameter +extensions-without-links+
  '(".mp3" ".png" ".jpg" ".jpeg" ".pdf")
  "TODO")

(export-always 'url)
(defmethod url ((url quri:uri))
  url)

(defmethod url ((url-string string))
  (quri:uri url-string))

(defun string->url (url-string)
  "Return the `quri:uri' object corresponding to URL-STRING.
If URL-STRING is a path pointing to an existing file, return a `quri:uri' object
 with the `file' scheme.
If URL-STRING cannot be converted to a `quri:uri' object, return an empty `quri:uri'."
  (or (ignore-errors
       (if (uiop:file-exists-p url-string)
           (quri:uri (str:concat "file://" url-string))
           (quri:uri url-string)))
      (quri:uri "")))

(defun strings->urls (url-strings)
  "Return the list of `quri:uri's corresponding to URL-STRINGS.
If a URL string cannot be converted to a `quri:uri', it is discarded from the result."
  (remove-if #'url-empty-p (mapcar #'string->url url-strings)))

(defun has-method-p (object generic-function)
  "Return non-nil if OBJECT has GENERIC-FUNCTION specialization."
  (some (lambda (method)
          (subtypep (type-of object) (class-name
                                      (first (closer-mop:method-specializers method)))))
        (closer-mop:generic-function-methods generic-function)))

(defun has-url-method-p (object)
  "Return non-nil if OBJECT has `url' specialization."
  (has-method-p object #'url))

(deftype url-designator ()
  `(satisfies has-url-method-p))

(export-always 'render-url)
(-> render-url ((or quri:uri string)) string)
(defun render-url (url)
  "Return decoded URL.
If the URL contains hexadecimal-encoded characters, return their unicode counterpart."
  (let ((url (if (stringp url)
                 url
                 (quri:render-uri url))))
    (the (values (or string null) &optional)
         (or (ignore-errors (ffi-display-url *browser* url))
             url))))

(export-always 'render-host-and-scheme)
(defun render-host-and-scheme (url)
  "Return decoded URL without path, if existent."
  (format nil "~a://~a" (quri:uri-scheme url)
          (quri:uri-host url)))

(export-always 'fetch-url-title)
(defun fetch-url-title (url)
  "Return URL's title.
The URL is fetched, which explains possible bottlenecks."
  (plump:text (aref (clss:select "title" (plump:parse (dex:get url))) 0)))

(export-always 'error-help)
(defun error-help (&optional (title "Unknown error") (text ""))
  "A helper to print error messages as displayable HTML."
  (values
   (spinneret:with-html-string
     (:head
      (:title title)
      (:style (style (current-buffer))))
     (:body
      (:h1 title)
      (:pre text)))
   "text/html;charset=utf8"))

(export-always 'renderer-scheme)
(defclass renderer-scheme ()
  ()
  (:metaclass mixin-class))

(define-class scheme (renderer-scheme)
  ((name (error "Scheme must have a name/scheme")
         :documentation "Scheme/name of the internal scheme.
For instance, \"gopher\", \"irc\".")
   (callback
    nil
    :type (or null (function (url-designator buffer) t))
    :documentation "Callback to get the page contents when accessing resource with this scheme.

Takes two arguments: the URL with scheme and the buffer it was requested in.

Optionally returns two values:
- The data for page contents (either as string or as a unsigned byte array).
- The MIME type for the contents.")
   (error-callback
    nil
    :type (or null (function (condition)))
    :documentation "Callback to use when a condition is signalled.")
   (local-p
    nil
    :documentation "Local schemes are not accessible to the pages of other schemes.")
   (no-access-p
    nil
    :documentation "No-access schemes cannot access pages with any other scheme.")
   (secure-p
    nil
    :documentation "Secure schemes can access the Web, Web can access them too.

Requires encryption or other means of security.")
   (cors-enabled-p
    nil
    :documentation "Whether other pages can do requests to the resources with this scheme."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Representation of Nyxt-specific internal schemes.")
  (:metaclass user-class))

(defmethod print-object ((scheme scheme) stream)
  (print-unreadable-object (scheme stream :type t :identity t)
    (format stream "~a" (name scheme))))

(defvar *schemes* (sera:dict)
  "A table of internal schemes registered in Nyxt.
Keys are scheme strings, values are `scheme' objects.")

(export-always 'define-internal-scheme)
(defun define-internal-scheme (scheme-name callback
                      &rest keys
                      &key local-p
                        no-access-p
                        secure-p
                        cors-enabled-p
                      &allow-other-keys)
  "Define a handler (running CALBACK) for SCHEME-NAME scheme.

CALLBACK is called with two arguments:
- the URL that was requested with this scheme, and
- buffer that it was requested in.

For keyword arguments' meaning, see `scheme' slot documentation."
  (declare (ignorable local-p no-access-p secure-p cors-enabled-p))
  (setf (gethash scheme-name *schemes*)
        (apply #'make-instance 'scheme
               :name scheme-name
               :callback callback
               keys)))

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

(export-always 'valid-scheme-p)
(defun valid-scheme-p (scheme)
  (let* ((nyxt-schemes (append '("blob" "javascript") (alex:hash-table-keys *schemes*)))
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
           '("example" "invalid" "local" "localhost" "onion" "test"))
         (valid-schemes (append nyxt-schemes iana-schemes special-use-schemes)))
    (sera:true (find scheme valid-schemes :test #'string=))))

(export-always 'valid-url-p)
(defun valid-url-p (url &key (check-dns-p t))
  "Return non-nil when URL is a valid URL.
The domain name existence is verified only if CHECK-DNS-P is T. Domain
name validation may take significant time since it looks up the DNS."
  (let ((url (ignore-errors (quri:uri url))))
    (and url
         (quri:uri-p url)
         (valid-scheme-p (quri:uri-scheme url))
         ;; `new-url-query' automatically falls back to HTTPS if it makes for
         ;; a valid URL:
         (or (not (quri:uri-http-p url))
             (and
              ;; "http:/https://www.iana.org/assignments/special-use-domain-names/special-use-domain-names.xml/" does not have a host.
              ;; A valid URL may have an empty domain, e.g. http://192.168.1.1.
              (quri:uri-host url)
              (or
               (not check-dns-p)
               (valid-tld-p (quri:uri-host url))
               ;; "http://algo" has the "algo" hostname but it's probably invalid
               ;; unless it's found on the local network.  We also need to
               ;; support "localhost" and the current system hostname.
               (or (quri:ip-addr-p (quri:uri-host url))
                   (lookup-hostname (quri:uri-host url)))))))))

(-> ensure-url (t) quri:uri)
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
  "Small convenience function to check whether the given URL is empty."
  (the (values boolean &optional)
       (uiop:emptyp (if (quri:uri-p url) (quri:render-uri url) url))))

(-> empty-path-url-p (quri:uri) boolean)
(export-always 'empty-path-url-p)
(defun empty-path-url-p (url)
  (or (string= (quri:uri-path url) "/")
      (null (quri:uri-path url))))

(-> host-only-url-p (quri:uri) boolean)
(export-always 'host-only-url-p)
(defun host-only-url-p (url)
  (every #'null
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

(export-always 'nyxt-url)
(-> nyxt-url (t &rest t &key &allow-other-keys) string)
(defun nyxt-url (function-name &rest args &key &allow-other-keys)
  "Generate a nyxt: URL from the given FUNCTION-NAME applied to ARGS.
 This is useful to generate internal pages.

ARGS is an arbitrary keyword (!) arguments list that will be translated to
URL parameters.

The resulting URL should be perfectly parseable back to the initial form with
`parse-nyxt-url'.

Example:
\(nyxt-url 'nyxt:describe-command :value 'nyxt:describe-value)
=> \"nyxt:describe-command?value=NYXT%3ADESCRIBE-VALUE\"

\(parse-nyxt-url (nyxt-url 'nyxt:describe-value :id \"1000\"))
=> NYXT:DESCRIBE-VALUE
=> (:ID \"1000\")"
  (let ((*print-case* :downcase))
    (flet ((param-name (symbol)
             (let ((*package* (find-package :nyxt)))
               (if (keywordp symbol)
                   (format nil "~(~a~)" symbol)
                   (format nil "~s" symbol)))))
      (if (gethash function-name *nyxt-url-commands*)
          (let ((params (quri:url-encode-params
                         (mapcar (lambda (pair)
                                   (cons (param-name (first pair))
                                         ;; This is to safely parse the args afterwards
                                         (if (stringp (rest pair))
                                             (rest pair)
                                             (str:concat +escape+ (prin1-to-string (rest pair))))))
                                 (alexandria:plist-alist args)))))
            (the (values string &optional)
                 (format nil "nyxt:~a~@[~*?~a~]"
                         (param-name function-name)
                         (not (uiop:emptyp params))
                         params)))
          (error "There's no nyxt:~a page defined" (param-name function-name))))))

(export-always 'javascript-url)
(defun javascript-url (javascript-string)
  "Return a string that's a suitable value for an HTML href."
  (str:concat "javascript:" (quri:url-encode javascript-string)))

(export-always 'internal-url-p)
(defun internal-url-p (url)
  (string= "nyxt" (quri:uri-scheme (url url))))

(export-always 'parse-nyxt-url)
(-> parse-nyxt-url ((or string quri:uri)) (values symbol list &optional))
(defun parse-nyxt-url (url)
  "Return two values parsed from the nyxt: URL:
- the name of the `internal-page',
- the arguments to it.

Error out if some of the params are not constants. Thanks to this,
`parse-nyxt-url' can be repeatedly called on the same nyxt: URL, with the
guarantee of the same result."
  (let* ((url (url url))
         (symbol (quri:uri-path url))
         (params (quri:uri-query-params url))
         (internal-page-name (let ((*package* (find-package :nyxt)))
                               (read-from-string (str:upcase symbol)))))
    (if (gethash internal-page-name *nyxt-url-commands*)
        (values internal-page-name
                (alex:mappend (lambda (pair)
                                (let ((key (intern (str:upcase (first pair)) :keyword))
                                      (value (if (str:starts-with-p +escape+ (rest pair))
                                                 (read-from-string (subseq (rest pair) 1))
                                                 (rest pair))))
                                  ;; Symbols are safe (are they?)
                                  (if (or (symbolp value)
                                          (constantp value))
                                      (list key value)
                                      (error "A non-constant value passed in URL params: ~a" value))))
                              params))
        (error "There's no nyxt:~a internal-page defined" symbol))))

(define-internal-scheme "nyxt"
    (lambda (url buffer)
      (with-protect ("Error while processing the \"nyxt:\" URL: ~a" :condition)
        (let ((url (quri:uri url)))
          (log:debug "Internal page ~a requested." url)
          (multiple-value-bind (internal-page-name args) (parse-nyxt-url url)
            (when (and internal-page-name)
              (alex:when-let ((internal-page (gethash internal-page-name *nyxt-url-commands*)))
                (enable-modes (page-mode internal-page) buffer)
                (setf (title buffer) (apply #'dynamic-title internal-page args))
                (multiple-value-bind (content encoding)
                    (apply (form internal-page) args)
                  (cond
                    ((and (arrayp content)
                          (stringp encoding))
                     (values content encoding))
                    ((arrayp content)
                     content)
                    (t (error "Cannot display evaluation result"))))))))))
  :local-p t)

(ps:defpsmacro nyxt/ps::lisp-eval ((&key (buffer '(nyxt:current-buffer)) title callback) &body form)
  "Request the lisp: URL and invoke CALLBACK when there's a successful result.
BUFFER must be a `document-buffer'.
TITLE is purely informative."
  ;; We define it here and not in parenscript-macro because we
  `(let ((url (ps:lisp (let ((request-id (string (gensym ""))))
                             (setf (gethash request-id (nyxt::lisp-url-callbacks ,buffer))
                                   (lambda () ,@form))
                             (let ((url-string (format nil "lisp://~a" request-id)))
                               (when ,title
                                 (setf url-string (str:concat url-string "/" ,title)))
                               url-string)))))
     (let ((request (fetch url
                           (ps:create :mode "no-cors"))))
       (when ,callback
         (chain request
                (then (lambda (response)
                        (when (@ response ok)
                          (chain response (json)))))
                (then ,callback))))))
(export-always 'nyxt/ps::lisp-eval :nyxt/ps)

(define-internal-scheme "lisp"
    (lambda (url buffer)
      (let ((url (quri:uri url)))
        ;; TODO: Replace this condition with `(not (network-buffer-p buffer))`?
        (if (or (status-buffer-p buffer)
                (panel-buffer-p buffer)
                (prompt-buffer-p buffer)
                (internal-url-p (url buffer)))
            (let* ((request-id (quri:uri-host url))
                   (title (when (and url (quri:uri-path url)) (sera:drop-prefix "/" (quri:uri-path url)))))
              (log:debug "Evaluate Lisp code from internal page: ~a" (or title "UNTITLED"))
              (values (let ((result (run (gethash request-id (lisp-url-callbacks buffer)))))
                        ;; Objects and other complex structures make cl-json choke.
                        ;; TODO: Maybe encode it to the format that `cl-json'
                        ;; supports, then we can override the encoding and
                        ;; decoding methods and allow arbitrary objects (like
                        ;; buffers) in the nyxt:// URL arguments..
                        (cl-json:encode-json-to-string
                         (when (or (scalar-p result)
                                   (and (sequence-p result)
                                        (every #'scalar-p result)))
                           result)))
                      "application/json"))
            (values "undefined" "application/json;charset=utf8"))))
  :cors-enabled-p t
  :error-callback (lambda (c) (log:debug "Error when evaluating lisp URL: ~a" c)))

;; useless?
(-> path= (quri:uri quri:uri) boolean)
(defun path= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same path."
  (flet ((normalize-path (path)
           "TODO"
           (if (or (null path) (equal path ""))
               "/"
               path)))
    (equal (normalize-path (quri:uri-path url1))
           (normalize-path (quri:uri-path url2)))))

(-> distinct-document-p (quri:uri quri:uri) boolean)
(defun distinct-document-p (url1 url2)
  "Return non-nil when URL1 and URL2 refer to distinct documents."
  (let ((path-match-p (path= url1 url2))
        (query-p (or (quri:uri-query url1) (quri:uri-query url2))))
    (cond ((and path-match-p query-p)
           t)
          ((and path-match-p (null query-p))
           nil)
          (t t))))

(-> links-p (quri:uri) boolean)
(defun links-p (url)
  "Return non-nil when URL has links."
  (loop for extension in +extensions-without-links+
        ;; wrapped in null so that it returns a boolean
        always (null (search extension (quri:uri-path url) :from-end t))))

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
  (equal (quri:uri-host url1) (quri:uri-host url2)))

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
        (some (alex:curry #'string= (quri:uri-scheme (url url-designator)))
              (cons scheme other-schemes)))))

(-> match-host (string &rest string) (function (quri:uri) boolean))
(export-always 'match-host)
(defun match-host (host &rest other-hosts)
  "Return a predicate for URL designators matching one of HOST or OTHER-HOSTS."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:curry #'string= (quri:uri-host (url url-designator)))
              (cons host other-hosts)))))

(-> match-domain (string &rest string) (function (quri:uri) boolean))
(export-always 'match-domain)
(defun match-domain (domain &rest other-domains)
  "Return a predicate for URL designators matching one of DOMAIN or OTHER-DOMAINS."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:curry #'string= (quri:uri-domain (url url-designator)))
              (cons domain other-domains)))))

(-> match-port (integer &rest integer) (function (quri:uri) boolean))
(export-always 'match-port)
(defun match-port (port &rest other-ports)
  "Return a predicate for URL designators matching one of PORT or OTHER-PORTS."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:curry #'eq (quri:uri-port (url url-designator)))
              (cons port other-ports)))))

(-> match-file-extension (string &rest string) (function (quri:uri) boolean))
(export-always 'match-file-extension)
(defun match-file-extension (extension &rest other-extensions)
  "Return a predicate for URL designators matching one of EXTENSION or OTHER-EXTENSIONS."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:curry #'string= (pathname-type (or (quri:uri-path (url url-designator)) "")))
              (cons extension other-extensions)))))

(-> match-regex (string &rest string) (function (quri:uri) boolean))
(export-always 'match-regex)
(defun match-regex (regex &rest other-regex)
  "Return a predicate for URL designators matching one of REGEX or OTHER-REGEX."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:rcurry #'cl-ppcre:scan (render-url (url url-designator)))
              (cons regex other-regex)))))

(-> match-url (string &rest string) (function (quri:uri) boolean))
(export-always 'match-url)
(defun match-url (one-url &rest other-urls)
  "Return a predicate for URLs exactly matching ONE-URL or OTHER-URLS."
  #'(lambda (url-designator)
      (when url-designator
        (some (alex:rcurry #'string= (render-url (url url-designator)))
              (mapcar (lambda (u) (quri:url-decode u :lenient t))
                      (cons one-url other-urls))))))

(-> eq-uri-p (quri:uri quri:uri list) boolean)
(defun eq-uri-p (url1 url2 eq-fn-list)
  "Return non-nil when URL1 and URL2 are \"equal\" as dictated by EQ-FN-LIST.

EQ-FN-LIST is a list of functions that take URL1 and URL2 as arguments and
return a boolean.  It defines an equivalence relation induced by EQ-FN-LIST.
`quri:uri=', `host=' and `scheme=' are examples of equivalence relations."
  ;; (and (fn1 url1 url2) (fn2 url1 url2) ...) returns without evaluating all
  ;; forms, unlike the solution below.
  (every (lambda (fn) (funcall fn url1 url2)) eq-fn-list))

;; this doesn't check for repeated links, but I think it's correct!
(defun renderer-get-links (&optional (buffer (current-buffer))
                              (filtering-rules (list #'scheme=
                                                     #'host=
                                                     #'distinct-document-p)))
  "Return a list of links from BUFFER.
FILTERING-RULES is a list of functions that take two URLs and return a boolean.
A link is collected when, for all elements of FILTERING-RULES, the return value
is non-nil."
  (with-current-buffer buffer
    ;; using below doesn't work, why?
    (loop for i from 0 to (1- (peval (ps:@ document links length)))
          with url = (url buffer) ; computed once
          for link = (quri:uri (peval (ps:chain document links (item (ps:lisp i)) href)))
          when (and (links-p link)
                    (eq-uri-p url link filtering-rules))
            collect link)))

;; TODO How to pass the optional arg to fetch-links?
;; shouldn't this be together with fetch-links at renderer-script.lisp?
;; I think this is useless...
;; (-> renderer-fetch-links (quri:uri) list)
(defun renderer-fetch-links (url)
  "Return a list of links from URL.
If there is a buffer corresponding to URL, then fetch the links from it.
Otherwise, create a new buffer to fetch the links."
  (alex:if-let ((bufferp (find url (buffer-list) :test #'quri:uri= :key #'url)))
    (renderer-get-links bufferp)
    ;; Use background buffers
    ;; Buffers should be made as light as possible (no images, CSS, etc).
    (let ((channel (make-channel)))
      (once-on (buffer-loaded-hook (make-nosave-buffer :url url)) buffer
        (calispel:! channel (renderer-get-links buffer)))
      (calispel:? channel))))

;; FIXME Manage urls that download stuff or can't be reached
;; (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
;;   (dex:get url))
;; SEE https://github.com/fukamachi/dexador/issues/127
(defun client-get-links (url)
  (let ((all-links))
    (dolist (elem (plump:get-elements-by-tag-name
                   (plump:parse (handler-bind
                                    ((dex:http-request-failed #'dex:ignore-and-continue))
                                  (dex:get url)))
                   "a")
                  all-links)
      (when (plump:attribute elem "href")
        (push (quri:merge-uris (quri:uri (plump:attribute elem "href")) url)
              all-links)))))

;; Shouldn't the responsibility of deleting duplicated links be delegated to
;; recursive-links alone?
;; (-> http-fetch-links (&optional quri:uri function) list)
(defun http-fetch-links (&optional (url (url (current-buffer)))
                                   (equiv-relation #'uri-equivalence-for-fetch-links))
  "Return a list of links from URL."
  (loop for link in (client-get-links url)
        ;; links-p should be passed optionally
        when (and (links-p link)
                  (funcall equiv-relation url link)
                  (not (find link all-links :test #'uri-link-equivalence)))
          collect link into all-links
        finally (return all-links)))

(-> uri-equivalence-for-fetch-links (quri:uri quri:uri) boolean)
(defun uri-equivalence-for-fetch-links (uri1 uri2)
  (flet ((%path (path) "Normalizes PATH." (if (or (null path)
                                                  (equal path ""))
                                              "/"
                                              path)))
    (and ;; guarantees that http and https belong to the same equivalence class
         (quri:uri-http-p uri1)
         (quri:uri-http-p uri2)
         (equal (quri:uri-host uri1)
                (quri:uri-host uri2))
         (not (equal (%path (quri:uri-path uri1))
                     (%path (quri:uri-path uri2))))
         (or (and (null (quri:uri-query uri1))
                  (null (quri:uri-query uri2)))
             (not (equal (quri:uri-query uri1)
                         (quri:uri-query uri2)))))))

(-> uri-link-equivalence (quri:uri quri:uri) boolean)
(defun uri-link-equivalence (uri1 uri2)
  (flet ((%path (path) (string-right-trim "/" (or path ""))))
    (and (or (eq (type-of uri1) (type-of uri2))
             (and (quri:uri-http-p uri1)
                  (quri:uri-http-p uri2)))
         (equal (quri:uri-host uri1)
                (quri:uri-host uri2))
         (equal (%path (quri:uri-path uri1))
                (%path (quri:uri-path uri2)))
         (equal (quri:uri-query uri1)
                (quri:uri-query uri2)))))

(-> recursive-links (integer
                     list
                     &optional (or compiled-function symbol)
                     (or compiled-function symbol))
    list)
(defun recursive-links (depth url-list
                        &optional
                          (fetch-links-fn #'http-fetch-links)
                          (filter-links-fn #'identity))
  "Return a list of length DEPTH.

Each element is a list of URLs at distance 0..DEPTH from URL-LIST.

DEPTH is the minimum distance between 2 URLs, i.e. how many URLs need to be
visited to get from one to the other via their links.

URL-LIST is list of URLs of type QURI.URI.

FILTER-LINKS-FN is a function that takes a list of URLs and returns another.

FETCH-LINKS-FN is a function that takes a URL and returns its corresponding
links.

Example: Take URL-LIST to be a list of one element, url U, and DEPTH to be 2.
The links of U are collected in a list, (list (\"links-of\" U)).  With that list
as the new starting point, the same method is called with depth 1, and finally
with depth 0.  Find below the stack of calls.

(recursive-links 2 (list U))
(recursive-links 1 (\"links-of\" U))
(recursive-links 0 (\"links-of\" (\"links-of\" U)))

-> ((list U)                         ; DEPTH 0
    (\"links-of\" U)                 ; DEPTH 1
    (\"links-of\" (\"links-of\" U))) ; DEPTH 2"
  (labels ((nth-power-function (n arg fn)
             "Apply FN on ARG at most N times, as long as ARG is non-nil."
             (if (or (zerop n) (null arg))
                 arg
                 (nth-power-function (decf n) (funcall fn arg) fn)))
           (url-in-list-p (url list)
             "Return non-nil when URL is a member of LIST."
             ;; position, find or member?
             (find url list :test #'uri-link-equivalence)))
    ;; why do I need (when url-list) and not just (list url-list)?
    (let ((url-list-all-depth (when url-list (list url-list)))
          ;; Unless `filter-links-fn' is the identity function, VISITED-URL-LIST
          ;; doesn't replace URL-LIST-ALL-DEPTH.
          (visited-url-list (when url-list url-list)))
      (nth-power-function
       depth
       url-list
       (lambda (url-list)
         ;; Guarantee that the links of any URL are fetched once and only once;
         ;; and that LINK-LIST is a set, i.e. a list without duplicated
         ;; elements.
         (let ((link-list))
           (dolist (url url-list)
             (loop for link in (funcall fetch-links-fn url)
                   unless (or (url-in-list-p link link-list)
                              (url-in-list-p link visited-url-list))
                     do (progn (push link link-list)
                               (push link visited-url-list))))
           (push (funcall filter-links-fn link-list) url-list-all-depth)
           link-list)))
      (nreverse url-list-all-depth))))

(defun interactive-recursive-links (depth url-list
                                    &optional (fetch-links-fn #'http-fetch-links))
  "Return the same as `recursive-links'.
At each DEPTH, a subset of URLs is selected with `prompt-buffer'."
  ;; TODO How to exit a prompt without choosing any URL?
  (recursive-links
   depth
   url-list
   fetch-links-fn
   (lambda (url-list) (prompt :prompt "Choose URLs"
                         :sources (make-instance 'prompter:source
                                                 :constructor url-list
                                                 :name "URL"
                                                 :multi-selection-p t)))))
