;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

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
         (or (ignore-errors (ffi-display-url url))
             url))))

(defmacro defmemo (name params &body body) ; TODO: Replace with https://github.com/AccelerationNet/function-cache?
  (alex:with-gensyms (memo-table args result result?)
    `(let ((,memo-table (make-hash-table :test 'equal)))
       (defun ,name (&rest ,args)
         (multiple-value-bind (,result ,result?)
             (gethash ,args ,memo-table)
           (if ,result?
               ,result
               (setf (gethash ,args ,memo-table)
                     (apply (lambda ,params
                              ,@body)
                            ,args))))))))

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

(export-always 'valid-url-p)
(defun valid-url-p (url &key (check-dns-p t))
  "Return non-nil when URL is a valid URL.
The domain name existence is verified only if CHECK-DNS-P is T. Domain
name validation may take significant time since it looks up the DNS."
  ;; List of URI schemes: https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
  ;; Last updated 2020-08-26.
  (let* ((nyxt-schemes '("nyxt" "lisp" "web-extension" "javascript"))
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
         (valid-schemes (append nyxt-schemes iana-schemes))
         (url (ignore-errors (quri:uri url))))
    (flet ((valid-scheme-p (scheme)
             (find scheme valid-schemes :test #'string=))
           (http-p (scheme)
             (find scheme '("http" "https") :test #'string=)))
      (and url
           (quri:uri-p url)
           (valid-scheme-p (quri:uri-scheme url))
           ;; `new-url-query' automatically falls back to HTTPS if it makes for
           ;; a valid URL:
           (or (not (http-p (quri:uri-scheme url)))
               (and
                ;; "http://" does not have a host.
                ;; A valid URL may have an empty domain, e.g. http://192.168.1.1.
                (quri:uri-host url)
                (or
                 (not check-dns-p)
                 ;; Onion links are not resolved via DNS, just accept them.
                 (string= (quri:uri-tld url) "onion")
                 ;; "http://algo" has the "algo" hostname but it's probably invalid
                 ;; unless it's found on the local network.  We also need to
                 ;; support "localhost" and the current system hostname.
                 (or (quri:ip-addr-p (quri:uri-host url))
                     (lookup-hostname (quri:uri-host url))))))))))

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

(-> url< (quri:uri quri:uri) (or null fixnum))
(defun url< (url1 url2)
  "Like `string<' but ignore the URL scheme.
This way, HTTPS and HTTP is ignored when comparing URIs."
  (string< (schemeless-url url1)
           (schemeless-url url2)))

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

(defvar *nyxt-url-commands* (make-hash-table)
  "A map from allowed nyxt: URLs symbols to the functions that generate code of
  the pages related to these commands.")

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

\(parse-nyxt-url (nyxt-url 'nyxt:describe-value :value ''nyxt:*browser*))
=> NYXT:DESCRIBE-VALUE
=> (:VALUE 'NYXT:*BROWSER*)"
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
                                         (prin1-to-string (rest pair))))
                                 (alexandria:plist-alist args)))))
            (the (values string &optional)
                 (format nil "nyxt:~a~@[~*?~a~]"
                         (param-name function-name)
                         (not (uiop:emptyp params))
                         params)))
          (error "There's no nyxt:~a page defined" (param-name function-name))))))

(defun internal-url-p (url)
  (string= "nyxt" (quri:uri-scheme (url url))))

(export-always 'lisp-url)
(-> lisp-url (t &rest t) string)
(defun lisp-url (lisp-form &rest more-lisp-forms)
  "Generate a lisp:// URL from the given Lisp FUNCTION-NAME and ARGS. This is useful for encoding
functionality into internal-buffers."
  (the (values string &optional)
       (apply #'str:concat "lisp://"
              (mapcar (alex:compose #'quri:url-encode #'write-to-string)
                      (cons lisp-form more-lisp-forms)))))

(export-always 'parse-nyxt-url)
(-> parse-nyxt-url ((or string quri:uri)) (values symbol list &optional))
(defun parse-nyxt-url (url)
  "Return (1) the name of the function and (2) the arguments to it, parsed from nyxt: URL.

Error out if some of the params are not constants. Thanks to this,
`parse-nyxt-url' can be repeatedly called on the same nyxt: URL, with the
guarantee of the same result."
  (let* ((url (url url))
         (symbol (quri:uri-path url))
         (params (quri:uri-query-params url))
         (function (read-from-string (str:upcase symbol))))
    (if (gethash function *nyxt-url-commands*)
        (values function
                (alex:mappend (lambda (pair)
                                (let ((key (intern (str:upcase (first pair)) :keyword))
                                      (value (read-from-string (rest pair))))
                                  ;; Symbols are safe (are they?)
                                  (if (or (symbolp value)
                                          (constantp value))
                                      (list key value)
                                      (error "A non-constant value passed in URL params: ~a" value))))
                              params))
        (error "There's no nyxt:~a page defined" symbol))))

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
