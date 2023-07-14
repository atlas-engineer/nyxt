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

(export-always 'render-host-and-scheme)
(defun render-host-and-scheme (url)
  "Return decoded URL without path, if existent."
  (format nil "~a://~a" (quri:uri-scheme url)
          (quri:uri-host url)))

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
  ((name (error "Scheme must have a name/scheme")
         :documentation "Scheme/name of the internal scheme.
For instance, \"gopher\", \"irc\".")
   (callback
    nil
    :type (or null function)
    :documentation "Callback to get the page contents when accessing resource with this scheme.

Takes two arguments: the URL with scheme and the buffer it was requested in.

Returns up to five values:
- The data for page contents (either as string or as a unsigned byte array).
- The MIME type for the contents.
- The status code for the request.
- An alist of headers for the request.
- A status reason phrase.")
   (error-callback
    nil
    :type (or null function)
    :documentation "Callback to use when a condition is signaled.

Accepts only one argument: the signaled condition.")
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
    :documentation "Whether other pages can do requests to the resources with this scheme.")
   (display-isolated-p
    nil
    :documentation "Display-isolated schemes cannot be displayed (in iframes, for example) by other schemes.")
   (empty-document-p
    nil
    :documentation "Empty document schemes can be loaded synchronously by websites referring to them."))
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
Keys are scheme strings, values are `scheme' objects.")

(export-always 'define-internal-scheme)
(defun define-internal-scheme (scheme-name callback
                               &rest keys
                               &key local-p
                                 no-access-p
                                 secure-p
                                 cors-enabled-p
                               &allow-other-keys)
  "Define a handler (running CALLBACK) for SCHEME-NAME `scheme'.

CALLBACK is called with two arguments:
- the URL that was requested with this scheme, and
- buffer that it was requested in.

For keyword arguments' meaning, see the corresponding `scheme' slot
documentation."
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
  (let* ((*package* (find-package :nyxt))
         (*print-case* :downcase))
    (if (keywordp symbol)
        (format nil "~(~a~)" symbol)
        (format nil "~s" symbol))))

(-> value->param-value (t) (values string &optional))
(defun value->param-value (value)
  "Turns the VALUE into a `query-params->arglist'-readable representation.
- If VALUE is a string, return it as-is.
- Otherwise, print it so string and prepend `+escape+' in front of it to notify
  the `query-params->arglist' that it should be READ by CL reader."
  (if (stringp value)
      value
      ;; This is to safely parse the args afterwards
      (str:concat +escape+ (let ((*package* (find-package :nyxt)))
                             (prin1-to-string value)))))

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
\(nyxt-url 'nyxt:describe-function :value 'nyxt:describe-value)
=> \"nyxt:describe-function?value=NYXT%3ADESCRIBE-VALUE\"

\(parse-nyxt-url (nyxt-url 'nyxt:describe-value :id \"1000\"))
=> NYXT:DESCRIBE-VALUE
=> (:ID \"1000\")"
  (let ((*print-case* :downcase))
    (if (gethash function-name *nyxt-url-commands*)
        (let ((params (quri:url-encode-params
                       (mapcar (lambda (pair)
                                 (cons (symbol->param-name (first pair))
                                       (value->param-value (rest pair))))
                               (alexandria:plist-alist args))
                       :space-to-plus t)))
          (the (values string &optional)
               (format nil "nyxt:~a~@[~*?~a~]"
                       (symbol->param-name function-name)
                       (not (uiop:emptyp params))
                       params)))
        (error "There's no nyxt:~a page defined" (symbol->param-name function-name)))))

(export-always 'javascript-url)
(defun javascript-url (javascript-string)
  "Return a string that's a suitable value for an HTML href."
  (str:concat "javascript:" (quri:url-encode javascript-string)))

(export-always 'internal-url-p)
(defun internal-url-p (url)
  "Whether the URL is the `internal-page' one."
  ;; FIXME: Too simple. Maybe check for command presence too?
  (string= "nyxt" (quri:uri-scheme (url url))))

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
         ;; FIXME: While we ourselves guarantee the correctness of nyxt:// URLs,
         ;; it would be nice to ensure it processes even the malformed URLs.
         (params (quri:uri-query-params url))
         (internal-page-name (uiop:safe-read-from-string (str:upcase symbol)
                                                         :package (find-package :nyxt))))
    (if (gethash internal-page-name *nyxt-url-commands*)
        (values internal-page-name (query-params->arglist params))
        (error "There's no nyxt:~a internal-page defined" symbol))))

(define-internal-scheme "nyxt"
    ;; NOTE: We don't set any security settings (such as :local-p, :secure-p
    ;; etc.)  because:
    ;; - :local-p makes nyxt:// scheme completely inaccessible from other schemes.
    ;; - :display-isolated-p does not allow embedding a nyxt:// page even inside
    ;;   the page of the same scheme. Self-embedding might be a use-case in the
    ;;   future, thus :display-isolated-p is unusable.
    ;; - :secure-p and :cors-enabled-p are both dangerously permissive for a
    ;;   scheme that allows evaluating Lisp code.
    ;; Not setting any security settings gives the best security/flexibility for
    ;; our use-case:
    ;; - <iframe> embedding and exploitation are impossible.
    ;; - Redirection—both as window.location.(href|assign|replace) and
    ;;   HTTP status code 301—works.
    ;; - nyxt:// pages are linkable from outside Nyxt!
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

(-> lisp-url (&rest t &key
                    (:id string)
                    (:buffer t) ;; `document-buffer', actually.
                    (:title (maybe string))
                    (:callback (or function symbol)))
    string)
(defun lisp-url (&key title (id (princ-to-string (nyxt:new-id))) (buffer (current-buffer))
                   (callback (unless (gethash id (nyxt::lisp-url-callbacks buffer))
                               (alex:required-argument 'callback))))
  (unless buffer
    (error "Cannot make a `lisp-url' without BUFFER or current-buffer."))
  (sera:synchronized ((nyxt::lisp-url-callbacks buffer))
    (unless (gethash id (nyxt::lisp-url-callbacks buffer))
      (log:debug "Registering callback ~a for buffer ~a" id buffer)
      (setf (gethash id (nyxt::lisp-url-callbacks buffer)) callback)))
  (format nil "lisp://~a?~:[~*~;title=~a&~]" id
          title (when title
                  (quri:url-encode title))))


(ps:defpsmacro nyxt/ps::lisp-call (id &key title (buffer '(current-buffer)) args)
  "Call the ID-bound function on the Lisp side.

Return a JS Promise fulfilled after the code runs on the Lisp side.

ID should be an identifier of an already defined `lisp-url'.

The ARGS are used as a keyword arglist for the function bound to the defined URL callback."
  `(fetch (ps:lisp (str:concat
                    (lisp-url :id ,id :buffer ,buffer :title ,title)
                    (quri:url-encode-params
                     (list ,@(loop for (name value) on args by #'cddr
                                   collect `(cons (symbol->param-name ,name)
                                                  (value->param-value ,value))))
                     :space-to-plus t)))
          (ps:create :mode "no-cors")))
(export-always 'nyxt/ps::lisp-call :nyxt/ps)

(export-always 'lisp-url-flet)
(defmacro lisp-url-flet (buffer ((name (&rest args) &body binding-body) &rest other-bindings)
                         &body body)
  "Bind the NAMEs of bindings to both
- the `nyxt/ps:lisp-call'-able IDs, and
- macros expanding to the Parenscript form calling them.

Bindings are only available in local scope, and are only defined for BUFFER.

Example:
\(nyxt::lisp-url-flet (nyxt:current-buffer)
    ((hello (&key name) (nyxt:echo \"Hello, ~a!\" name)))
  (nyxt:ffi-buffer-evaluate-javascript (nyxt:current-buffer) (hello :name \"Stranger\"))
  ;; or equivalent
  (nyxt:ffi-buffer-evaluate-javascript
   (nyxt:current-buffer)
   (ps:ps (nyxt/ps:lisp-call hello :buffer (nyxt:current-buffer) :args '(:name \"Stranger\"))))))"
  (alex:with-gensyms (id)
    `(let* ((,id (princ-to-string (new-id)))
            (,name (progn
                     (lisp-url :id ,id
                               :buffer ,buffer
                               :callback (lambda (,@args) ,@binding-body))
                     ,id)))
       (macrolet ((,name (&rest args)
                    ;; Everything that uses commas comes from the outside,
                    ;; everything with list/cons/quote belongs to this
                    ;; macro. Otherwise it's really hard to keep track of.
                    (list (quote ps:ps)
                          (list (quote nyxt/ps:lisp-call) (quote ,name)
                                :title ,(str:concat "lisp-url-flet " (string name))
                                :buffer (quote ,buffer) :args args))))
         ,@(if other-bindings
               `((lisp-url-flet ,buffer (,@other-bindings)
                   ,@body))
               body)))))

(ps:defpsmacro nyxt/ps::lisp-eval ((&key (buffer '(nyxt:current-buffer))
                                         title callback args)
                                   &body body)
  "Request the lisp: URL and invoke CALLBACK when there's a successful result.

TITLE is purely informative.

BUFFER must be a `document-buffer'.

The ARGS are used as a keyword arglist for the CALLBACK."
  ;; We define it here and not in parenscript-macro because we need
  ;; `nyxt::lisp-url-callbacks' while parenscript-macro is Nyxt-independent.
  `(let ((promise (nyxt/ps:lisp-call
                   (ps:lisp
                    ;; FIXME: We define a URL, but don't use it anywhere, we only use its
                    ;; ID. Quirky idiom. Maybe somehow only define an ID without string
                    ;; generation?
                    (sera:lret ((id (princ-to-string (nyxt:new-id))))
                      (lisp-url
                       :id id
                       :buffer ,buffer
                       :callback ,(if (and (sera:single body)
                                           (member (first (first body)) '(lambda function)))
                                      (first body)
                                      `(lambda () ,@body)))))
                   :buffer ,buffer :title ,title ,@args)))
     ,@(when callback
         `((ps:chain promise
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
                   (params (and url (quri:uri-query-params url)))
                   (title (when params
                            (alex:assoc-value params "title")))
                   (args (alexandria:remove-from-plist (query-params->arglist params) :title)))
              (log:debug "Evaluate Lisp callback ~a from internal page ~a: ~a" request-id buffer (or title "UNTITLED"))
              (values (let ((result (with-current-buffer buffer
                                      (let ((callback (sera:synchronized ((lisp-url-callbacks buffer))
                                                        (gethash request-id (lisp-url-callbacks buffer)))))
                                        (if callback
                                            (run callback args)
                                            (log:warn "Request ~a is bound to no callback for buffer ~a"
                                                      url buffer))))))
                        ;; Objects and other complex structures make cl-json choke.
                        ;; TODO: Maybe encode it to the format that `cl-json'
                        ;; supports, then we can override the encoding and
                        ;; decoding methods and allow arbitrary objects (like
                        ;; buffers) in the nyxt:// URL arguments..
                        (j:encode
                         (when (or (scalar-p result)
                                   (and (sequence-p result)
                                        (every #'scalar-p result)))
                           result)))
                      "application/json"))
            (values "undefined" "application/json;charset=utf8"))))
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
