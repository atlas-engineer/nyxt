(in-package :nyxt)

;; Create necessary hook types. We must forward-declare the class
;; since the hook take the type of the class that hosts them.
(defclass minibuffer () ())
(hooks:define-hook-type minibuffer (function (minibuffer)))
(hooks:define-hook-type download (function (download-manager:download)))

(hooks:define-hook-type resource (function (request-data) (or request-data null)))
(export-always '(make-hook-resource make-handler-resource))

(defclass-export proxy ()
  ((server-address :accessor server-address :initarg :server-address
                   :initform (quri:uri "socks5://127.0.0.1:9050")
                   :type quri:uri
                   :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example: \"http://192.168.1.254:8080\".")
   (allowlist :accessor allowlist :initarg :allowlist
              :initform '("localhost" "localhost:8080")
              :type list-of-strings
              :documentation "A list of URIs not to forward to the proxy.")
   (proxied-downloads-p :accessor proxied-downloads-p :initarg :proxied-downloads-p
                        :initform t
                        :documentation "Non-nil if downloads should also use the proxy."))
  (:documentation "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."))

(export-always 'combine-composed-hook-until-nil)
(defmethod combine-composed-hook-until-nil ((hook hooks:hook) &optional arg)
  "Return the result of the composition of the HOOK handlers on ARG, from
oldest to youngest.  Stop processsing when a handler returns nil.
Without handler, return ARG.  This is an acceptable `combination' for
`hook'."
  (labels ((compose-handlers (handlers result)
             (if handlers
                 (let ((new-result (funcall (first handlers) result)))
                   (when new-result
                     (compose-handlers (rest handlers) new-result)))
                 result)))
    (compose-handlers (mapcar #'hooks:fn (hooks:handlers hook)) arg)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'browser)
  (export '(remote-execution-p
            data-profile
            socket-thread
            focus-on-reopened-buffer-p
            startup-function
            open-external-link-in-new-window-p
            search-engines
            download-path
            history-path
            history-store-function
            history-restore-function
            bookmarks-data
            bookmarks-path
            bookmarks-store-function
            bookmarks-restore-function
            session-path
            session-store-function
            session-restore-function
            session-restore-prompt
            auto-mode-rules
            auto-mode-rules-data-path
            standard-output-path
            error-output-path
            before-exit-hook
            window-make-hook
            buffer-make-hook
            buffer-before-make-hook
            minibuffer-make-hook
            before-download-hook
            after-download-hook
            autofills
            spell-check-language
            external-editor-program)))
(defclass-export browser ()
  ((remote-execution-p :accessor remote-execution-p
                       :initarg remote-execution-p
                       :type boolean
                       :initform nil
                       :documentation "When non-nil, execute Lisp code that is sent to the socket.
You must understand the risks before enabling this: a privliged user with access
to your system can then take control of the browser and execute arbitrary code
under your user profile.")
   (data-profile :accessor data-profile
                 :initarg :data-profile
                 :type data-profile
                 :initform (or (find-data-profile (getf *options* :data-profile))
                               +default-data-profile+)
                 :documentation "Profile to use for all persisted files.
See the `data-path' class and the `expand-path' function.")
   (socket-thread :accessor socket-thread
                  :initform nil
                  :documentation "Thread that listens on socket.
See `*socket-path*'.
This slot is mostly meant to clean up the thread if necessary.")
   (password-interface :accessor password-interface
                       :initform (password:make))
   (messages-content :accessor messages-content
                     :initform '()
                     :type list
                     :documentation "A list of all echoed messages.
Most recent messages are first.")
   (clipboard-ring :accessor clipboard-ring :initform (make-ring))
   (minibuffer-generic-history :accessor minibuffer-generic-history
                               :initform (make-ring))
   (minibuffer-search-history :accessor minibuffer-search-history
                              :initform (make-ring))
   (minibuffer-set-url-history :accessor minibuffer-set-url-history :initform (make-ring))
   (recent-buffers :accessor recent-buffers :initform (make-ring :size 50)
                   :documentation "A ring that keeps track of deleted buffers.")
   (focus-on-reopened-buffer-p :accessor focus-on-reopened-buffer-p ; TODO: Replace this with minibuffer Helm-style actions.
                               :initform t
                               :documentation "When reopening a closed buffer,
focus on it instead of opening it in the background.

Warning: This setting may be deprecated in a future release, don't rely on it.")
   (windows :accessor windows
            :initform (make-hash-table :test #'equal))
   (total-window-count :initform 0
                       :type integer
                       :documentation "This is used to generate unique window
identifiers in `get-unique-window-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (last-active-window :initform nil
                       :type (or window null)
                       :documentation "Records the last active window.  This is
useful when no Nyxt window is focused and we still want `ffi-window-active' to
return something.
See `current-window' for the user-facing function.")
   (last-active-buffer :accessor last-active-buffer :initform nil)
   (buffers :initform (make-hash-table :test #'equal)
            :documentation "To manipulate the list of buffers,
see `buffer-list', `buffers-get', `buffers-set' and `buffers-delete'.")
   (total-buffer-count :initform 0
                       :type integer
                       :documentation "This is used to generate unique buffer
identifiers in `get-unique-buffer-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (startup-function :accessor startup-function
                     :type (or function null)
                     :initform (make-startup-function)
                     :documentation "The function run on startup.  It takes a
list of URLs (strings) as optional argument (the command line positional
arguments).  It is run after the renderer has been initialized and after the
`*after-init-hook*' has run.")
   (startup-error-reporter-function :accessor startup-error-reporter-function
                                    :initarg :startup-error-reporter-function
                                    :type (or function null)
                                    :initform nil
                                    :documentation "When supplied, upon startup,
if there are errors, they will be reported by this function.")
   (open-external-link-in-new-window-p :accessor open-external-link-in-new-window-p
                                       :initform nil
                                       :documentation "When open links from an external program, or
when C-cliking on a URL, decide whether to open in a new
window or not.")
   (search-engines :accessor search-engines
                   :initform (list (make-instance 'search-engine
                                                  :shortcut "default"
                                                  :search-url "https://duckduckgo.com/?q=~a"
                                                  :fallback-url "https://duckduckgo.com/")
                                   (make-instance 'search-engine
                                                  :shortcut "wiki"
                                                  :search-url "https://en.wikipedia.org/w/index.php?search=~a"
                                                  :fallback-url "https://en.wikipedia.org/"))
                   :type list-of-search-engines
                   :documentation "A list of the `search-engine' objects.
You can invoke them from the minibuffer by prefixing your query with SHORTCUT.
If the query is empty, FALLBACK-URL is loaded instead.  If
FALLBACK-URL is empty, SEARCH-URL is used on an empty search.

The engine with the \"default\" shortcut (or the first engine if there is no
\"default\") is used when the query is not a valid URL, or the first keyword is
not recognized.")
   (key-stack :accessor key-stack :initform '()
              :documentation "A stack that keeps track of the key chords a user has pressed.")
   (downloads :accessor downloads :initform '()
              :documentation "List of downloads.")
   (download-watcher :accessor download-watcher :initform nil
                     :documentation "List of downloads.")
   (download-path :accessor download-path
                  :type data-path
                  :initform (make-instance 'download-data-path
                                           :dirname (xdg-download-dir))
                  :documentation "Path of directory where downloads will be
stored.  Nil means use system default.")
   (startup-timestamp :initarg :startup-timestamp :accessor startup-timestamp
                      :type local-time:timestamp
                      :initform nil
                      :documentation "`local-time:timestamp' of when Nyxt was started.")
   (init-time :initform 0.0 :type number
              :documentation "Init time in seconds.")
   (history-data :initform nil
                 :documentation "
The history data kept in memory.")
   (history-path :initarg :history-path
                 :accessor history-path
                 :type data-path
                 :initform (make-instance 'history-data-path :basename "history")
                 :documentation "
The path where the system will create/save the global history.")
   (history-store-function :initarg :history-store-function
                           :accessor history-store-function
                           :type (or function null)
                           :initform #'store-sexp-history
                           :documentation "
The function which stores the global history into `history-path'.")
   (history-restore-function :initarg :history-restore-function
                             :accessor history-restore-function
                             :type (or function null)
                             :initform #'restore-sexp-history
                             :documentation "
The function which restores the global history from `history-path'.")
   (bookmarks-data :initform nil
                   :documentation "
The bookmarks kept in memory.")
   (bookmarks-path :initarg :bookmarks-path
                   :accessor bookmarks-path
                   :type data-path
                   :initform (make-instance 'bookmarks-data-path :basename "bookmarks")
                   :documentation "
The path where the system will create/save the bookmarks.")
   (bookmarks-store-function :initarg :bookmarks-store-function
                             :accessor bookmarks-store-function
                             :type (or function null)
                             :initform #'store-sexp-bookmarks
                             :documentation "
The function which stores the bookmarks into `bookmarks-path'.")
   (bookmarks-restore-function :initarg :bookmarks-restore-function
                               :accessor bookmarks-restore-function
                               :type (or function null)
                               :initform #'restore-sexp-bookmarks
                               :documentation "
The function which restores the bookmarks from `bookmarks-path'.")
   (session-path :initarg :session-path
                 :accessor session-path
                 :type data-path
                 :initform (make-instance 'session-data-path
                                          :basename "default"
                                          :dirname (uiop:xdg-data-home +data-root+ "sessions"))
                 :documentation "
The path where the system will create/save the session.")
   (session-store-function :accessor session-store-function
                           :type (or function null)
                           :initform #'store-sexp-session
                           :documentation "The function which stores the session
into `session-path'.")
   (session-restore-function :accessor session-restore-function
                             :type (or function null)
                             :initform #'restore-sexp-session
                             :documentation "The function which restores the session
from `session-path'.")
   (session-restore-prompt :accessor session-restore-prompt
                           :initform :always-ask
                           :documentation "Ask whether to restore the
session. Possible values are :always-ask :always-restore :never-restore.")
   (auto-mode-rules :accessor auto-mode-rules
                    :type list
                    :initform '()
                    :documentation "The list of auto-mode rules kept in memory.")
   (auto-mode-rules-data-path :accessor auto-mode-rules-data-path
                              :type data-path
                              :initform (make-instance 'auto-mode-rules-data-path
                                                       :basename "auto-mode-rules")
                 :documentation "The path where the auto-mode rules are saved.")
   (standard-output-path :accessor standard-output-path
                         :type data-path
                         :initform (make-instance 'data-path :basename "standard-out.txt")
                         :documentation "Path where `*standard-output*' can be written to.")
   (error-output-path :accessor error-output-path
                      :type data-path
                      :initform (make-instance 'data-path :basename "standard-error.txt")
                      :documentation "Path where `*error-output*' can be written to.")
   ;; Hooks follow:
   (before-exit-hook :accessor before-exit-hook
                     :initform (hooks:make-hook-void)
                     :type hooks:hook-void
                     :documentation "Hook run before both `*browser*' and the
renderer get terminated.  The handlers take no argument.")
   (window-make-hook :accessor window-make-hook
                     :initform (make-hook-window)
                     :type hook-window
                     :documentation "Hook run after `window-make'.
The handlers take the window as argument.")
   (buffer-make-hook :accessor buffer-make-hook
                     :initform (make-hook-buffer)
                     :type hook-buffer
                     :documentation "Hook run after `buffer-make' and before `ffi-buffer-load'.
It is run before `initialize-modes' so that the default mode list can still be
altered from the hooks.
The handlers take the buffer as argument.")
   (buffer-before-make-hook :accessor buffer-before-make-hook
                            :initform (make-hook-buffer)
                            :type hook-buffer
                            :documentation "Hook run before `buffer-make'.
This hook is mostly useful to set the `cookies-path'.
The buffer web view is not allocated, so it's not possible to run any
parenscript from this hook.  See `buffer-make-hook' for a hook.
The handlers take the buffer as argument.")
   (minibuffer-make-hook :accessor minibuffer-make-hook
                         :initform (make-hook-minibuffer)
                         :type hook-minibuffer
                         :documentation "Hook run after the `minibuffer' class
is instantiated and before initializing the minibuffer modes.
The handlers take the minibuffer as argument.")
   (before-download-hook :accessor before-download-hook
                         :initform (make-hook-download)
                         :type hook-download
                         :documentation "Hook run before downloading a URL.
The handlers take the URL as argument.")
   (after-download-hook :accessor after-download-hook
                        :initform (make-hook-download)
                        :type hook-download
                        :documentation "Hook run after a download has completed.
The handlers take the `download-manager:download' class instance as argument.")
   (autofills :accessor autofills
              :initform (list (make-autofill :key "Name" :fill "My Name")
                              (make-autofill :name "Hello Printer"
                                             :key "Function example"
                                             :fill (lambda () (format nil "hello!"))))
              :documentation "To autofill run the command `autofill'.
Use this slot to customize the autofill values available.

The fill can be a string value it or a function.  The latter allows you to
provide content dynamic to the context.")

   (spell-check-language :accessor spell-check-language
                         :initform "en_US"
                         :documentation "Spell check language used by Nyxt. For
a list of more languages available, please view the documentation for
cl-enchant (broker-list-dicts).")
   (external-editor-program :accessor external-editor-program
                  :type (or string null)
                  :initform nil
                  :documentation "The external editor to use for
editing files. It should be specified as a complete string path to the
editor executable.")))

(defmethod get-containing-window-for-buffer ((buffer buffer)
                                             (browser browser))
  "Get the window containing a buffer."
  (find buffer (alex:hash-table-values (windows browser)) :key #'active-buffer))

(defmethod finalize ((browser browser) urls startup-timestamp)
  "Run `*after-init-hook*' then BROWSER's `startup-function'."
  ;; `messages-appender' requires `*browser*' to be initialized.
  (log4cl:add-appender log4cl:*root-logger* (make-instance 'messages-appender))
  (handler-case
      (hooks:run-hook *after-init-hook*)
    (error (c)
      (log:error "In *after-init-hook*: ~a" c)))
  (funcall-safely (startup-function browser) urls)
  ;; Set 'init-time at the end of finalize to take the complete startup time
  ;; into account.
  (setf (slot-value *browser* 'init-time)
        (local-time:timestamp-difference (local-time:now) startup-timestamp)))

;; Catch a common case for a better error message.
(defmethod buffers :before ((browser t))
  (when (null browser)
    (error "There is no current *browser*. Is Nyxt started?")))

(defmethod history-data ((browser browser))
  "Return the `history-data' slot from BROWSER.
If empty, the history data is initialized with `history-restore-function'."
  (when (and (null (slot-value browser 'history-data))
             (history-restore-function browser))
    (funcall-safely (history-restore-function browser)))
  (slot-value browser 'history-data))

(defmethod (setf history-data) (value (browser browser))
  "Set `history-data' to VALUE.
Persist the `history-data' slot from BROWSER to `history-path' with
`history-store-function'."
  (setf (slot-value browser 'history-data) value)
  (match (history-store-function browser)
    ((guard f f) (funcall-safely f))))

(defmethod bookmarks-data ((browser browser))
  "Return the `bookmarks-data' slot from BROWSER.
If empty, the bookmarks data is initialized with `bookmarks-restore-function'."
  (when (and (null (slot-value browser 'bookmarks-data))
             (bookmarks-restore-function browser))
    (funcall-safely (bookmarks-restore-function browser)))
  (slot-value browser 'bookmarks-data))

(defmethod (setf bookmarks-data) (value (browser browser))
  "Set `bookmarks-data' to VALUE.
Persist the `bookmarks-data' slot from BROWSER to `bookmarks-path' with
`bookmarks-store-function'."
  (setf (slot-value browser 'bookmarks-data) value)
  (match (bookmarks-store-function browser)
    ((guard f f) (funcall-safely f))))

(defun download-watch ()
  "Update the download-list buffer.
This function is meant to be run in the background."
  ;; TODO: Add a (sleep ...)?  If we have many downloads, this loop could result
  ;; in too high a frequency of refreshes.
  (when download-manager:*notifications*
    (loop for d = (lparallel:receive-result download-manager:*notifications*)
          while d
          when (download-manager:finished-p d)
            do (hooks:run-hook (after-download-hook *browser*))
          do (let ((buffer (find-buffer 'download-mode)))
               ;; Only update if buffer exists.  We update even when out of focus
               ;; because if we switch to the buffer after all downloads are
               ;; completed, we won't receive notifications so the content needs
               ;; to be updated already.
               ;; TODO: Disable when out of focus?  Maybe need hook for that.
               (when buffer
                 (ffi-within-renderer-thread *browser* #'download-refresh))))))

;; TODO: To download any URL at any moment and not just in resource-query, we
;; need to query the cookies for URL.  Thus we need to add an IPC endpoint to
;; query cookies.
(declaim (ftype (function (quri:uri &key
                                    (:cookies (or string null))
                                    (:proxy-address t))
                          (values (or null download-manager:download) &rest t))
                download))
(export-always 'download)
(defun download (url &key
                     cookies
                     (proxy-address :auto))
  "Download URL.
When PROXY-ADDRESS is :AUTO (the default), the proxy address is guessed from the
current buffer."
  (hooks:run-hook (before-download-hook *browser*) url) ; TODO: Set URL to download-hook result?
  (when (eq proxy-address :auto)
    (setf proxy-address (proxy-address (current-buffer)
                                       :downloads-only t)))
  (let ((download-dir (expand-path (download-path *browser*))))
    (declare (type (or quri:uri null) proxy-address))
    (when download-dir
      (let* ((download nil))
        (flet ((unsafe-download ()
                 (setf download (download-manager:resolve
                                 url
                                 :directory download-dir
                                 :cookies cookies
                                 :proxy proxy-address))
                 (push download (downloads *browser*))
                 download))
          (if *keep-alive*
              (unsafe-download)
              (handler-case
                  (unsafe-download)
                (error (c)
                  (echo-warning "Download error: ~a" c)
                  nil))))))))

(defmethod get-unique-window-identifier ((browser browser))
  (format nil "~s" (incf (slot-value browser 'total-window-count))))

(defmethod get-unique-buffer-identifier ((browser browser))
  (format nil "~s" (incf (slot-value browser 'total-buffer-count))))

(declaim (ftype (function (&optional window buffer)) set-window-title))
(export-always 'set-window-title)
(defun set-window-title (&optional (window (current-window)) (buffer (current-buffer)))
  "Set current window title to 'Nyxt - TITLE - URL.
If Nyxt was started from a REPL, use 'Nyxt REPL...' instead.
This is useful to tell REPL instances from binary ones."
  (let ((url (url buffer))
        (title (title buffer)))
    (setf title (if (str:emptyp title) "" title))
    (setf url (if (url-empty-p url) "<no url/name>" (object-display url)))
    (ffi-window-set-title window
                          (str:concat "Nyxt" (when *keep-alive* " REPL") " - "
                                       title (unless (str:emptyp title) " - ")
                                       url))))

(declaim (ftype (function (list-of-strings &key (:no-focus boolean)))))
(defun open-urls (urls &key no-focus)
  "Create new buffers from URLs.
First URL is focused if NO-FOCUS is nil."
  (handler-case
      (let ((first-buffer (first (mapcar
                                  (lambda (url)
                                    (let ((buffer (make-buffer)))
                                      (buffer-load url :buffer buffer)
                                      buffer))
                                  urls))))
        (when (and first-buffer (not no-focus))
          (if (open-external-link-in-new-window-p *browser*)
              (let ((window (window-make *browser*)))
                (window-set-active-buffer window first-buffer))
              (set-current-buffer first-buffer))))
    (error (c)
      (echo-warning "Could not make buffer to open ~a: ~a" urls c))))

(defun scheme-keymap (buffer buffer-scheme)
  "Return the keymap in BUFFER-SCHEME corresponding to the BUFFER `keymap-scheme-name'.
If none is found, fall back to `scheme:cua'."
  (or (keymap:get-keymap (keymap-scheme-name buffer)
                         buffer-scheme)
      (keymap:get-keymap scheme:cua
                         buffer-scheme)))

(defun request-resource-open-url (&key url &allow-other-keys)
  (open-urls (list url) :no-focus t))

(defun request-resource-open-url-focus (&key url &allow-other-keys)
  (open-urls (list url) :no-focus nil))

(defclass-export request-data ()
  ((buffer :initarg :buffer
           :accessor buffer
           :type buffer
           :initform (current-buffer)
           :documentation "Buffer targetted by the request.")
   (url :initarg :url ; TODO: Rename to URI since it's a quri:uri and not a string?  Or leave URL everywhere, since we almost never use strings.
        :accessor url
        :type quri:uri
        :initform (quri:uri "")
        :documentation "URL of the request")
   (event-type :initarg :event-type
               ;; :accessor event-type ; TODO: No public accessor for now, we first need a use case.
               :type keyword
               :initform :other
               :documentation "The type of request, e.g. `:link-click'.")
   (new-window-p :initarg :new-window-p
                 :accessor new-window-p
                 :type boolean
                 :initform nil
                 :documentation "Whether the request wants to happen in a new window.")
   (known-type-p :initarg :known-type-p
                 :accessor known-type-p
                 :type boolean
                 :initform nil
                 :documentation "Whether the request is for a contented with
supported MIME-type (e.g. a picture that can be displayed in
the web view.")
   (keys :initarg :keys
         :accessor keys
         :type list
         :initform '()
         :documentation "The key sequence that was pressed to generate the request.")))

(export-always 'request-resource)
(defun request-resource (request-data)
  "Candidate for `request-resource-hook'.
Deal with REQUEST-DATA with the following rules:
- If a binding matches KEYS in `request-resource-scheme', run the bound function.
- If `new-window-p' is non-nil, load in new buffer.
- If `known-type-p' is nil, download the file.
- Otherwise let the renderer load the request."
  (with-slots (url buffer keys) request-data
    (let* ((keymap (scheme-keymap buffer (request-resource-scheme buffer)))
           (bound-function (the (or symbol keymap:keymap null)
                                (keymap:lookup-key keys keymap))))
      (declare (type quri:uri url))
      (cond
        ((internal-buffer-p buffer)
         (evaluate (quri:url-decode (quri:uri-domain url)))
         nil)
        (bound-function
         (log:debug "Resource request key sequence ~a" (keyspecs-with-optional-keycode keys))
         (funcall-safely bound-function :url url)
         nil)
        ((new-window-p request-data)
         (log:debug "Load URL in new buffer: ~a" (object-display url))
         (open-urls (list (object-string url)))
         nil)
        ((not (known-type-p request-data))
         (log:debug "Buffer ~a initiated download of ~s." (id buffer) (object-display url))
         (download url :proxy-address (proxy-address buffer :downloads-only t)
                       :cookies "")
         (unless (find-buffer 'download-mode)
           (download-list))
         nil)
        (t
         (log:debug "Forwarding: ~a" (object-display url))
         request-data)))))

(export-always 'url-dispatching-handler)
(declaim (ftype (function (symbol
                           (function (quri:uri) boolean)
                           (or string (function (quri:uri) (or quri:uri null)))))
                url-dispatching-handler))
(defun url-dispatching-handler (name test action)
  "Return a `resource' handler that, if `add-hook'ed to the `request-resource-hook',
will automatically apply its ACTION on the URLs that conform to TEST.

TEST should be function of one argument, the requested URL.
ACTION can be either a shell command as a string, or a function taking a URL as argument.
In case ACTION returns nil (always the case for shell command), URL request is aborted.
The new URL returned by ACTION is loaded otherwise.

`match-host', `match-scheme', `match-domain' and `match-file-extension'
can be used to create TEST-functions, but any other function of one argument
would fit the TEST slot as well.

The following example does a few things:
- Forward DOI links to the doi.org website.
- Open magnet links with Transmission.
- Open local files (file:// URIs) with Emacs.

\(define-configuration buffer
    ((request-resource-hook (reduce #'hooks:add-hook
                                    (list (url-dispatching-handler
                                           'doi-link-dispatcher
                                           (match-scheme \"doi\")
                                           (lambda (url)
                                             (quri:uri (format nil \"https://doi.org/~a\"
                                                               (quri:uri-path url)))))
                                          (url-dispatching-handler
                                           'transmission-magnet-links
                                           (match-scheme \"magnet\")
                                           \"transmission-remote --add ~a\")
                                          (url-dispatching-handler
                                           'emacs-file
                                           (match-scheme \"file\")
                                           (lambda (url)
                                             (uiop:launch-program
                                              `(\"emacs\" ,(quri:uri-path url)))
                                             nil)))
                                    :initial-value %slot-default))))"
  (make-handler-resource
   #'(lambda (request-data)
       (let ((url (url request-data)))
         (if (funcall-safely test url)
             (etypecase action
               (function
                (let* ((new-url (funcall-safely action url)))
                  (log:info "Applied ~s URL-dispatcher on ~s and got ~s"
                            (symbol-name name)
                            (object-display url)
                            (object-display new-url))
                  (when new-url
                    (setf (url request-data) new-url)
                    request-data)))
               (string (let ((action #'(lambda (url)
                                         (uiop:launch-program
                                          (format nil action
                                                  (object-string url)))
                                         nil)))
                         (funcall-safely action url)
                         (log:info "Applied ~s shell-command URL-dispatcher on ~s"
                            (symbol-name name)
                            (object-display url)))))
             request-data)))
   :name name))

(declaim (ftype (function (string &rest string) (function (quri:uri) boolean))
                match-scheme match-host match-domain
                match-file-extension match-regex match-url))
(export-always 'match-scheme)
(defun match-scheme (scheme &rest other-schemes)
  "Return a predicate for URLs matching one of SCHEME or OTHER-SCHEMES."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-scheme url))
            (cons scheme other-schemes))))

(export-always 'match-host)
(defun match-host (host &rest other-hosts)
  "Return a predicate for URLs matching one of HOST or OTHER-HOSTS."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-host url))
            (cons host other-hosts))))

(export-always 'match-domain)
(defun match-domain (domain &rest other-domains)
  "Return a predicate for URLs matching one of DOMAIN or OTHER-DOMAINS."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-domain url))
            (cons domain other-domains))))

(export-always 'match-file-extension)
(defun match-file-extension (extension &rest other-extensions)
  "Return a predicate for URLs matching one of EXTENSION or OTHER-EXTENSIONS."
  #'(lambda (url)
      (some (alex:curry #'string= (pathname-type (or (quri:uri-path url) "")))
            (cons extension other-extensions))))

(export-always 'match-regex)
(defun match-regex (regex &rest other-regex)
  "Return a predicate for URLs matching one of REGES or OTHER-REGEX."
  #'(lambda (url)
      (some (alex:rcurry #'cl-ppcre:scan (object-display url))
            (cons regex other-regex))))

(export-always 'match-url)
(defun match-url (one-url &rest other-urls)
  "Return a predicate for URLs exactly matching ONE-URL or OTHER-URLS."
  #'(lambda (url)
      (some (alex:rcurry #'string= (object-display url))
            (mapcar #'quri:url-decode (cons one-url other-urls)))))

(defun javascript-error-handler (condition)
  (echo-warning "JavaScript error: ~a" condition))

(defun format-status (window)
  (let ((buffer (current-buffer window)))
    (str:concat
     (markup:markup
      (:b (format nil "[~{~a~^ ~}]"
                  (mapcar (lambda (m) (str:replace-all "-mode" ""
                                                       (str:downcase
                                                        (class-name (class-of m)))))
                          (modes buffer))))
      (:span :class (when (eq (slot-value buffer 'load-status) :loading) "loader") "")
      (:span (if (eq (slot-value buffer 'load-status) :loading) "Loading: " ""))
      (:span
       (format nil " ~a — ~a"
               (object-display (url buffer))
               (title buffer)))))))

(defun print-message (message &optional window)
  (let ((window (or window (current-window))))
    (when window
      (ffi-print-message window message))))

(export-always 'current-window)
(defun current-window (&optional no-rescan)
  ;; TODO: Get rid of the NO-RESCAN option and find a fast way to retrieve
  ;; current window reliably.
  ;; Tests:
  ;; - Make two windows and make sure minibuffer gets spawned in the right window.
  ;; - Delete the second window and see if the minibuffer still works in the first one.
  "Return the currently active window.
If NO-RESCAN is non-nil, fetch the window from the `last-active-window' cache
instead of asking the renderer for the active window.  It is faster but
sometimes yields the wrong reasult."
  (when *browser*
    (if (and no-rescan (slot-value *browser* 'last-active-window))
        (slot-value *browser* 'last-active-window)
        (ffi-window-active *browser*))))

(defparameter %buffer nil)              ; TODO: Make a monad?

(export-always 'current-buffer)
(defun current-buffer (&optional window)
  "Get the active buffer for WINDOW, or the active window otherwise."
  (or %buffer
      (match (or window (current-window))
        ((guard w w) (active-buffer w))
        (_ (when *browser*
             (log:debug "No active window, picking last active buffer.")
             (last-active-buffer *browser*))))))

(export-always 'with-current-buffer)
(defmacro with-current-buffer (buffer &body body)
  "Execute BODY in a context in which `current-buffer' returns BUFFER."
  `(let ((%buffer ,buffer))
     ,@body))

(declaim (ftype (function (buffer)) set-current-buffer))
;; (declaim (ftype (function ((and buffer (not minibuffer)))) set-current-buffer)) ; TODO: Better.
;; But we can't use "minibuffer" here since it's not declared yet.  It will
;; crash Nyxt if we call set-current-buffer before instantiating the first
;; minibuffer.
(export-always 'set-current-buffer)
(defun set-current-buffer (buffer)
  "Set the active buffer for the active window."
  (unless (eq 'minibuffer (class-name (class-of buffer)))
    (if (current-window)
        (window-set-active-buffer (current-window) buffer)
        (make-window buffer))
    buffer))

(export-always 'current-minibuffer)
(defun current-minibuffer ()
  "Return the currently active minibuffer."
  (first (active-minibuffers (current-window))))

(defmethod write-output-to-log ((browser browser))
  "Set the *standard-output* and *error-output* to write to a log file."
  (values
   (setf *standard-output*
         (open (expand-path (standard-output-path browser))
               :direction :output
               :if-does-not-exist :create
               :if-exists :append))
   (setf *error-output*
         (open (expand-path (error-output-path browser))
               :direction :output
               :if-does-not-exist :create
               :if-exists :append))))

(defmacro define-ffi-method (name arguments)
  `(progn
     (export-always ',name)
     (defgeneric ,name (,@arguments))))

(define-ffi-method ffi-window-delete (window))
(define-ffi-method ffi-window-fullscreen (window))
(define-ffi-method ffi-window-unfullscreen (window))
(define-ffi-method ffi-buffer-uri (buffer))
(define-ffi-method ffi-buffer-title (buffer))
(define-ffi-method ffi-window-make (browser))
(define-ffi-method ffi-window-to-foreground (window))
(define-ffi-method ffi-window-set-title (window title))
(define-ffi-method ffi-window-active (browser))
(define-ffi-method ffi-window-set-active-buffer (window buffer))
(define-ffi-method ffi-window-set-minibuffer-height (window height))
(define-ffi-method ffi-buffer-make (browser))
(define-ffi-method ffi-buffer-delete (buffer))
(define-ffi-method ffi-buffer-load (buffer uri))
(define-ffi-method ffi-buffer-evaluate-javascript (buffer javascript))
(define-ffi-method ffi-minibuffer-evaluate-javascript (window javascript))
(define-ffi-method ffi-buffer-enable-javascript (buffer value))
(define-ffi-method ffi-buffer-enable-javascript-markup (buffer value))
(define-ffi-method ffi-buffer-enable-smooth-scrolling (buffer value))
(define-ffi-method ffi-buffer-enable-media (buffer value))
(define-ffi-method ffi-buffer-auto-load-image (buffer value))
(define-ffi-method ffi-buffer-user-agent (buffer value))
(define-ffi-method ffi-buffer-set-proxy (buffer &optional proxy-uri ignore-hosts))
(define-ffi-method ffi-buffer-get-proxy (buffer))
(define-ffi-method ffi-generate-input-event (window event))
(define-ffi-method ffi-generated-input-event-p (window event))
(define-ffi-method ffi-within-renderer-thread (browser thunk))
(define-ffi-method ffi-kill-browser (browser))
(define-ffi-method ffi-initialize (browser urls startup-timestamp))
(define-ffi-method ffi-inspector-show (buffer))
(define-ffi-method ffi-print-status (window text))
(define-ffi-method ffi-print-message (window message))
(define-ffi-method ffi-buffer-cookie-policy (buffer value))

(defmacro within-renderer-thread (&body body)
  "Convenience macro to run FFI-calling code from the REPL.
It should not be used to write code."
  `(ffi-within-renderer-thread
    *browser*
    (lambda ()
      ,@body)))
