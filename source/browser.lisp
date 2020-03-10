;;; interface.lisp --- interface state

(in-package :next)
(annot:enable-annot-syntax)

;; Create necessary hook types. We must forward-declare the class
;; since the hook take the type of the class that hosts them.
(defclass buffer () ())
(defclass minibuffer () ())
(defclass window () ())
(next-hooks:define-hook-type window (function (window)))
(next-hooks:define-hook-type buffer (function (buffer)))
(next-hooks:define-hook-type minibuffer (function (minibuffer)))
(next-hooks:define-hook-type download (function (download-manager:download)))
(next-hooks:define-hook-type window-buffer (function (window buffer)))

@export
@export-accessors
(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (active-minibuffers :accessor active-minibuffers :initform nil
                       :documentation "The stack of currently active minibuffers.")
   (status-buffer :accessor status-buffer
                  :initform (make-minibuffer)
                  :documentation "Buffer for displaying information such as
                  current URL or event messages.")
   (status-buffer-height :accessor status-buffer-height :initform 24
                         :type integer
                         :documentation "The height of the status buffer in pixels.")
   (minibuffer-open-height :accessor minibuffer-open-height :initform 256
                           :type integer
                           :documentation "The height of the minibuffer when open.")
   (window-set-active-buffer-hook :accessor window-set-active-buffer-hook
                                  :initform (make-hook-window-buffer)
                                  :type hook-window-buffer
                                  :documentation "Hook run before
   `window-set-active-buffer' takes effect. The handlers take the
   window and the buffer as argument.")
   (window-delete-hook :accessor window-delete-hook
                       :initform (make-hook-window)
                       :type hook-window
                       :documentation "Hook run after
    `ipc-window-delete' takes effect.  The handlers take the window as
    argument.")))

@export
@export-accessors
(defclass proxy ()
  ((server-address :accessor server-address :initarg :server-address
                   :initform "socks5://127.0.0.1:9050"
                   :type string
                   :documentation "The address of the proxy server.
                   It's made of three components: protocol, host and port.
                   Example:
                   http://192.168.1.254:8080")
   (whitelist :accessor whitelist :initarg :whitelist
              :initform '("localhost" "localhost:8080")
              :type list-of-strings
              :documentation "A list of URI not to forward to the
              proxy. It must be a list of strings.")
   (proxied-downloads-p :accessor proxied-downloads-p :initarg :proxied-downloads-p
                        :initform t
                        :documentation "Non-nil if downloads should
                        also use the proxy."))
  (:documentation "Enable forwarding of all network requests to a
                   specific host. This can apply to specific
                   buffer."))

(define-class-type proxy)
(declaim (type (proxy-type) *proxy-class*))
@export
(defparameter *proxy-class* 'proxy)

;; TODO: Reimplement certificate whitelist mode
@export
@export-accessors
(defclass certificate-whitelist ()
  ((whitelist :accessor whitelist :initarg :whitelist
              :initform '()
              :type list-of-strings
              :documentation "A list of hostnames for which certificate errors shall be ignored.
It must be a list of strings."))
  (:documentation "Enable ignoring of certificate errors.
This can apply to specific buffers."))

(define-class-type certificate-whitelist)
(declaim (type (certificate-whitelist-type) *certificate-whitelist-class*))
@export
(defparameter *certificate-whitelist-class* 'certificate-whitelist)

@export
@export-accessors
(defclass buffer ()
  ((id :accessor id :initarg :id :initform ""
       :documentation "Unique identifier for a buffer.  Dead buffers (i.e. those
not associated with a web view) have an empty ID.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (url :accessor url :initarg :url :type string :initform "")
   (title :accessor title :initarg :title :type string :initform "")
   (last-access :accessor last-access
                :initform (local-time:now)
                :type number
                :documentation "Timestamp when the buffer was last switched to.")
   (modes :accessor modes :initarg :modes :initform '()
          :documentation "The list of mode instances.")
   (default-modes :accessor default-modes :initarg :default-modes
                  :initform '(web-mode root-mode)
                  :type trivial-types:proper-list
                  :documentation "The list of symbols of class to
instantiate on buffer creation, unless specified.")
   (current-keymap-scheme ; TODO: Name keymap-scheme instead?
    :accessor current-keymap-scheme
    :initarg :current-keymap-scheme
    :initform :emacs
    :documentation "The keymap scheme that will be used
for all modes in the current buffer.")
   (override-map :accessor override-map
                 :initarg :override-map
                 :initform (let ((map (make-keymap)))
                             (define-key :keymap map
                               "M-x" #'execute-command)
                             map)
                 :documentation "This keymap is always looked up first, it
overrides all other bindings.  No libraries should ever touch the override-map,
this is left for the user to customize to their needs.")
   (forward-input-events-p :accessor forward-input-events-p :initarg :forward-input-events-p
                           :initform t
                           :documentation "When non-nil, keyboard events are
forwarded to the platform port when no binding is found.  Pointer
events (e.g. mouse events) are not affected by this, they are always
forwarded when no binding is found.")
   (last-event :accessor last-event
               :initform nil
               :type 'gdk:gdk-event
               ;; TODO: Store multiple events?  Maybe when implementing keyboard macros.
               :documentation "The last event that was received for the current buffer.")
   (resource-query-function :accessor resource-query-function
                            ;; TODO: What about having multiple functions?  And what about moving this to modes?
                            :initarg :resource-query-function
                            :initform #'request-resource)
   (default-new-buffer-url :accessor default-new-buffer-url :initform "https://next.atlas.engineer/start"
                           :documentation "The URL set to a new blank buffer opened by Next.")
   (scroll-distance :accessor scroll-distance :initform 50 :type number
                    :documentation "The distance scroll-down or scroll-up will scroll.")
   (horizontal-scroll-distance :accessor horizontal-scroll-distance :initform 50 :type number
                               :documentation "Horizontal scroll distance. The
distance scroll-left or scroll-right will scroll.")
   (current-zoom-ratio :accessor current-zoom-ratio :initform 1.0 :type number
                       :documentation "The current zoom relative to the default zoom.")
   (zoom-ratio-step :accessor zoom-ratio-step :initform 0.2 :type number
                    :documentation "The step size for zooming in and out.")
   (zoom-ratio-min :accessor zoom-ratio-min :initform 0.2 :type number
                   :documentation "The minimum zoom ratio relative to the default.")
   (zoom-ratio-max :accessor zoom-ratio-max :initform 5.0 :type number
                   :documentation "The maximum zoom ratio relative to the default.")
   (zoom-ratio-default :accessor zoom-ratio-default :initform 1.0 :type number
                       :documentation "The default zoom ratio.")
   (page-scroll-ratio :accessor page-scroll-ratio
                      :type number
                      :initform 0.90
                      :documentation "The ratio of the page to scroll.
A value of 0.95 means that the bottom 5% will be the top 5% when scrolling
down.")
   (cookies-path :accessor cookies-path
                 :initform (xdg-data-home "cookies.txt")
                 :documentation "The path where cookies are stored.  Not all
platform ports might support this.")
   (box-style :accessor box-style
              :initform (cl-css:css
                         '((".next-hint"
                            :background "linear-gradient(#fcff9e, #efcc00)"
                            :color "black"
                            :border "1px black solid"
                            :padding "1px 3px 1px 3px"
                            :border-radius "2px"
                            :z-index #.(1- (expt 2 31)))))
              :documentation "The style of the boxes, e.g. link hints.")
   (highlighted-box-style :accessor highlighted-box-style
                          :initform (cl-css:css
                                     '((".next-hint.next-highlight-hint"
                                        :font-weight "500"
                                        :background "#fcff9e")))

                          :documentation "The style of highlighted boxes, e.g. link hints.")
   (proxy :initform nil :type :proxy
          :documentation "Proxy for buffer.")
   (certificate-whitelist :initform nil :type :certificate-whitelist
                          :documentation "Certificate host whitelisting for buffer.")
   ;; TODO: Rename `load-hook' to `set-url-hook'?
   (load-hook :accessor load-hook
              :initform (next-hooks:make-hook-string->string
                         :combination #'next-hooks:combine-composed-hook)
              :type next-hooks:hook-string->string
              :documentation "Hook run in `set-url' after `parse-url' was)))))
processed.  The handlers take the URL going to be loaded as argument and must
return a (possibly new) URL.")
   (buffer-delete-hook :accessor buffer-delete-hook
                       :initform (make-hook-buffer)
                       :type hook-buffer
                       :documentation "Hook run before `buffer-delete' takes effect.
The handlers take the buffer as argument.")))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (ipc-buffer-set-proxy buffer
                            (server-address proxy)
                            (whitelist proxy))
      (ipc-buffer-set-proxy buffer
                            ""
                            nil)))

;; TODO: Find a better way to uniquely identify commands from mode methods.
;; What about symbol properties?  We could use:
;;
;; (setf (get name 'commandp) t)
;;
;; But that doesn't seem to work properly, some commands need to be evaluated
;; twice before they appear in the list.  We could use a class (we used to have
;; a COMMAND class) or intern the symbol into a special package (see `intern'
;; documentation).
(defparameter %%command-list '()
  "The list of known commands, for internal use only.")

(defun mode-list ()
  "Return the list of all namespaced mode symbols."
  (delete-if (complement (lambda (m)
                           (str:suffixp (list (symbol-name m) "-MODE")
                                        "-MODE")))
             (mapcar #'sym %%command-list)))

(defun mode-command (mode-symbol)
  "Return the mode toggle command.
We loop over `%%command-list' to find mode command since a mode may be
defined in any package and is unique."
  (find-if (lambda (c)
             (eq (find-symbol (string mode-symbol) (pkg c))
                 (sym c)))
           %%command-list))

(defmethod initialize-modes ((buffer buffer))
  "Initialize BUFFER modes.
   This must be called after BUFFER has been created on the platform port.
   See `buffer-make'."
  (let ((root-mode (make-instance 'root-mode :buffer buffer)))
    (dolist (mode-class (reverse (default-modes buffer)))
      ;; ":activate t" should not be necessary here since (modes buffer) should be
      ;; empty.
      ;; For now, root-mode does not have an associated command.
      (if (eq mode-class 'root-mode)
          (push root-mode (modes buffer))
          (progn
            (log:debug mode-class buffer (mode-command mode-class))
            (match (mode-command mode-class)
              ((guard c c) (funcall (sym c) :buffer buffer :activate t))
              (_ (log:warn "Mode command ~a not found." mode-class))))))))

;; A struct used to describe a key-chord
(defstruct key-chord
  key-code
  key-string
  modifiers
  position
  low-level-data)

(defmethod did-commit-navigation ((buffer buffer) url)
  (setf (url buffer) url)
  (with-result (title (%%buffer-get-title :buffer buffer))
    (setf (title buffer) title))
  (dolist (mode (modes buffer))
    (did-commit-navigation mode url)))

(defmethod did-finish-navigation ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (did-finish-navigation mode url)))

@export
@export-accessors
(defclass browser ()
  ((socket-thread :accessor socket-thread
                  :initform nil
                  :documentation "Thread that listens on socket.
See `socket-path'.
This slot is mostly meant to clean up the thread if necessary.")
   (socket-path :accessor socket-path
                :initarg :socket-path
                :initform (namestring (xdg-data-home "next.socket"))
                :type string
                :documentation "Path string of the Unix socket used to communicate
                between different instances of Next.")
   (single-instance-p :accessor single-instance-p
                      :initarg :single-instance-p
                      :initform t
                      :type boolean
                      :documentation "If non-nil, executing Next when a first
                      instance is already running will bring that instance to
                      the foreground and query it to load the URLs passed as
                      command line arguments.")
   (password-interface :accessor password-interface
                       :initform (password:make))
   (messages-content :accessor messages-content :initform nil :type :list
                     :documentation "A cl-markup plist of all echoed messages.
Most recent messages are first.")
   (clipboard-ring :accessor clipboard-ring :initform (ring:make))
   (minibuffer-generic-history :accessor minibuffer-generic-history :initform (ring:make))
   (minibuffer-search-history :accessor minibuffer-search-history :initform (ring:make))
   (minibuffer-set-url-history :accessor minibuffer-set-url-history :initform (ring:make))
   (recent-buffers :accessor recent-buffers :initform (ring:make :size 50)
                   :documentation "A ring that keeps track of deleted buffers.")
   (focus-on-reopened-buffer-p :accessor focus-on-reopened-buffer-p :initform t) ; TODO: Replace this with minibuffer Helm-style actions.
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (total-window-count :accessor total-window-count
                       :initform 0
                       :type integer
                       :documentation "This is used to generate unique window
identifiers in `get-unique-window-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (last-active-window :accessor last-active-window :initform nil) ; TODO: Replace by CURRENT-WINDOW?
   (last-active-buffer :accessor last-active-buffer :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count
                       :initform 0
                       :type integer
                       :documentation "This is used to generate unique buffer
identifiers in `get-unique-buffer-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (startup-function :accessor startup-function
                     :type function
                     :initform #'default-startup
                     :documentation "The function run on startup.  It takes a
list of URLs (strings) as argument (the command line positional arguments).  It
is run after the platform port has been initialized and after the
`*after-init-hook*' has run.")
   (start-page-url :accessor start-page-url :initform "https://next.atlas.engineer/quickstart"
                   :documentation "The URL of the first buffer opened by Next when started.")
   (open-external-link-in-new-window-p :accessor open-external-link-in-new-window-p
                                       :initform nil
                                       :documentation "When open links from an external program, or
when C-cliking on a URL, decide whether to open in a new
window or not.")
   (search-engines :accessor search-engines
                   :initform '(("default" "https://duckduckgo.com/?q=~a" "https://duckduckgo.com/")
                               ("wiki" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/"))
                   :type alist-of-3tuples-strings
                   :documentation "A list of the search engines.

The elements are in the form of a 3-tuple of strings (SHORTCUT SEARCH-URL FALLBACK-URL).
You can invoke them from the minibuffer by prefixing your query with SHORTCUT.
If the query is empty, FALLBACK-URL is loaded instead.  If
FALLBACK-URL is empty, SEARCH-URL is used on an empty search.

The 'default' engine is used when the query is not a valid URL, or the first
keyword is not recognized.")
   (key-chord-stack :accessor key-chord-stack :initform '()
                    :documentation "A stack that keeps track of the key chords a user has inputted.")
   (downloads :accessor downloads :initform '()
              :documentation "List of downloads.")
   (download-watcher :accessor download-watcher :initform nil
                     :documentation "List of downloads.")
   (download-directory :accessor download-directory :initform nil
                       :documentation "Path of directory where downloads will be
stored.  Nil means use system default.")
   (startup-timestamp :initarg :startup-timestamp :accessor startup-timestamp
                      :type local-time:timestamp
                      :initform nil
                      :documentation "`local-time:timestamp' of when Next was started.")
   (init-time :initform 0.0 :type number
              :documentation "Init time in seconds.")
   (history-data :accessor history-data
                 :initform nil
                 :documentation "
The history data kept in memory.")
   (history-path :initarg :history-path
                 :accessor history-path
                 :type pathname
                 :initform (xdg-data-home "history.lisp")
                 :documentation "
The path where the system will create/save the global history.")
   (history-store-function :initarg :history-store-function
                           :accessor history-store-function
                           :type function
                           :initform #'store-sexp-history
                           :documentation "
The function which stores the global history into `history-path'.")
   (history-restore-function :initarg :history-restore-function
                             :accessor history-restore-function
                             :type function
                             :initform #'restore-sexp-history
                             :documentation "
The function which restores the global history from `history-path'.")
   (bookmarks-data :accessor bookmarks-data
                   :initform nil
                   :documentation "
The bookmarks kept in memory.")
   (bookmarks-path :initarg :bookmarks-path
                   :accessor bookmarks-path
                   :initform (xdg-data-home "bookmarks.lisp")
                   :documentation "
The path where the system will create/save the bookmarks.")
   (bookmarks-store-function :initarg :bookmarks-store-function
                             :accessor bookmarks-store-function
                             :type function
                             :initform #'store-sexp-bookmarks
                             :documentation "
The function which stores the bookmarks into `bookmarks-path'.")
   (bookmarks-restore-function :initarg :bookmarks-restore-function
                               :accessor bookmarks-restore-function
                               :type function
                               :initform #'restore-sexp-bookmarks
                               :documentation "
The function which restores the bookmarks from `bookmarks-path'.")
   (session-path :accessor session-path
                 :type string
                 :initform (xdg-data-home "session.lisp")
                 :documentation "The path where the session is persisted.")
   (session-store-function :accessor session-store-function
                           :type function
                           :initform #'store-sexp-session
                           :documentation "The function which stores the session
into `session-path'.")
   (session-restore-function :accessor session-restore-function
                             :type function
                             :initform #'restore-sexp-session
                             :documentation "The function which restores the session
from `session-path'.")
   (standard-output-path :accessor standard-output-path
                         :initform (xdg-data-home "standard-out.txt")
                         :documentation "Path where `*standard-output*'
                         can be written to.")
   (error-output-path :accessor error-output-path
                      :initform (xdg-data-home "standard-error.txt")
                      :documentation "Path where `*error-output*' can be
                      written to.")
   ;; Hooks follow:
   (before-exit-hook :accessor before-exit-hook
                     :initform (next-hooks:make-hook-void)
                     :type next-hooks:hook-void
                     :documentation "Hook run before both `*browser*' and the
platform port get terminated.  The handlers take no argument.")
   (window-make-hook :accessor window-make-hook
                     :initform (make-hook-window)
                     :type hook-window
                     :documentation "Hook run after `window-make'.
The handlers take the window as argument.")
   (buffer-make-hook :accessor buffer-make-hook
                     :initform (make-hook-buffer)
                     :type hook-buffer
                     :documentation "Hook run after `buffer-make' and before `ipc-buffer-load'.
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
The handlers take the `download-manager:download' class instance as argument.")))

(defmethod finalize ((browser browser) urls startup-timestamp)
  "Run `*after-init-hook*' then BROWSER's `startup-function'."
  ;; `messages-appender' requires `*browser*' to be initialized.
  (log4cl-impl:add-appender log4cl:*root-logger* (make-instance 'messages-appender))
  (handler-case
      (next-hooks:run-hook *after-init-hook*)
    (error (c)
      (log:error "In *after-init-hook*: ~a" c)))
  (handler-case
      (funcall (startup-function browser) (or urls *free-args*))
    (error (c)
      (log:error "In startup-function ~a: ~a" (startup-function browser) c)))
  ;; Set 'init-time at the end of finalize to take the complete startup time
  ;; into account.
  (setf (slot-value *browser* 'init-time)
        (local-time:timestamp-difference (local-time:now) startup-timestamp)))

;; Catch a common case for a better error message.
(defmethod buffers :before ((browser t))
  (when (null browser)
    (error "There is no current *browser*. Is Next started?")))

(defun search-engines-names (&optional (browser *browser*))
  "Return a list of search engines names."
  (mapcar (lambda (tuple)
            (car tuple))
          (search-engines browser)))

(defun search-engine-starting-with (prefix)
  "Return the first search engine name that starts with PREFIX."
  (loop for name in (search-engines-names)
     when (str:starts-with-p prefix name)
     return name))

(defmethod history-data ((browser browser))
  "Return the `history-data' slot from BROWSER.
If empty, the history data is initialized with `history-restore-function'."
  (when (and (null (slot-value browser 'history-data))
             (history-restore-function browser))
    (funcall (history-restore-function browser)))
  (slot-value browser 'history-data))

(defmethod (setf history-data) (value (browser browser))
  "Set `history-data' to VALUE.
Persist the `history-data' slot from BROWSER to `history-path' with
`history-store-function'."
  (setf (slot-value browser 'history-data) value)
  (match (history-store-function browser)
    ((guard f f) (funcall f))))

(defmethod bookmarks-data ((browser browser))
  "Return the `bookmarks-data' slot from BROWSER.
If empty, the bookmarks data is initialized with `bookmarks-restore-function'."
  (when (and (null (slot-value browser 'bookmarks-data))
             (bookmarks-restore-function browser))
    (funcall (bookmarks-restore-function browser)))
  (slot-value browser 'bookmarks-data))

(defmethod (setf bookmarks-data) (value (browser browser))
  "Set `bookmarks-data' to VALUE.
Persist the `bookmarks-data' slot from BROWSER to `bookmarks-path' with
`bookmarks-store-function'."
  (setf (slot-value browser 'bookmarks-data) value)
  (match (bookmarks-store-function browser)
    ((guard f f) (funcall f))))

(declaim (ftype (function (buffer)) add-to-recent-buffers))
(defun add-to-recent-buffers (buffer)
  "Create a recent-buffer from given buffer and add it to `recent-buffers'."
  ;; Make sure it's a dead buffer:
  (setf (id buffer) "")
  (ring:delete-match (recent-buffers *browser*) (buffer-match-predicate buffer))
  (ring:insert (recent-buffers *browser*) buffer))

(defun download-watch ()
  "Update the download-list buffer.
This function is meant to be run in the background."
  ;; TODO: Add a (sleep ...)?  If we have many downloads, this loop could result
  ;; in too high a frequency of refreshes.
  (loop for d = (lparallel:receive-result download-manager:*notifications*)
        while d
        when (download-manager:finished-p d)
          do (next-hooks:run-hook (after-download-hook *browser*))
        do (let ((buffer (find-buffer 'download-mode)))
             ;; Only update if buffer exists.  We update even when out of focus
             ;; because if we switch to the buffer after all downloads are
             ;; completed, we won't receive notifications so the content needs
             ;; to be updated already.
             ;; TODO: Disable when out of focus?  Maybe need hook for that.
             (when buffer
               (download-refresh)))))

(defun proxy-address (buffer &key (downloads-only nil))
  "Return the proxy address, nil if not set.
If DOWNLOADS-ONLY is non-nil, then it only returns the proxy address (if any)
when `proxied-downloads-p' is true."
  (let* ((proxy (and buffer (proxy buffer)))
         (proxied-downloads (and proxy (proxied-downloads-p proxy))))
    (when (or (not downloads-only)
              proxied-downloads)
      (server-address proxy))))

;; TODO: To download any URL at any moment and not just in resource-query, we
;; need to query the cookies for URL.  Thus we need to add an IPC endpoint to
;; query cookies.
(defun download (url &key
                     cookies
                     (proxy-address :auto))
  "Download URI.
When PROXY-ADDRESS is :AUTO (the default), the proxy address is guessed from the
current buffer."
  (next-hooks:run-hook (before-download-hook *browser*) url)
  (when (eq proxy-address :auto)
    (setf proxy-address (proxy-address (current-buffer)
                                       :downloads-only t)))
  (let* ((download nil))
    (handler-case
        (progn
          (setf download (download-manager:resolve
                          url
                          :directory (download-directory *browser*)
                          :cookies cookies
                          :proxy proxy-address))
          (push download (downloads *browser*))
          download)
      (error (c)
        (echo-warning "Download error: ~a" c)
        nil))))

(defmethod get-unique-window-identifier ((browser browser))
  (incf (total-window-count browser)))

(defmethod get-unique-buffer-identifier ((browser browser))
  (incf (total-buffer-count browser)))

(declaim (ftype (function (window buffer)) set-window-title))
@export
(defun set-window-title (window buffer)
  "Set current window title to 'Next - TITLE - URL."
  (let ((url (url buffer))
        (title (title buffer)))
    (setf title (if (str:emptyp title) "" title))
    (setf url (if (str:emptyp url) "<no url/name>" url))
    (ipc-window-set-title window
                          (concatenate 'string "Next - "
                                       title (unless (str:emptyp title) " - ")
                                       url))))

(declaim (ftype (function (window buffer)) window-set-active-buffer))
@export
(defun window-set-active-buffer (window buffer) ; TODO: Rename window-set-buffer.
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                       :key #'active-buffer)))
    (next-hooks:run-hook (window-set-active-buffer-hook window) window buffer)
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (buffer-make *browser*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (ipc-window-set-active-buffer window-with-same-buffer temp-buffer)
          (ipc-window-set-active-buffer window buffer)
          (ipc-window-set-active-buffer window-with-same-buffer buffer-swap)
          (buffer-delete temp-buffer))
        (ipc-window-set-active-buffer window buffer))
    (setf (last-access buffer) (local-time:now))
    (setf (last-active-buffer *browser*) buffer)
    (set-window-title window buffer)
    (echo-dismiss)
    (setf (active-buffer window) buffer)))

(defun %get-inactive-buffer ()
  "Return inactive buffer or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer (window-list)))
        (buffers (buffer-list)))
    (match (set-difference buffers active-buffers)
      ((guard diff diff)
       ;; Display the most recent inactive buffer.
       (first (sort diff #'local-time:timestamp> :key #'last-access))))))

(defmethod push-url-at-point ((buffer buffer) url)
  ;; TODO: Use buffer local queue to avoid many/duplicate echo
  (declare (ignore buffer))
  (if (str:emptyp url)
      (echo-dismiss)
      (echo "→ ~a" url)))

(defun open-urls (urls &key no-focus)
  "Create new buffers from URLs.
   First URL is focused if NO-FOCUS is nil."
  (handler-case
      (let ((first-buffer (first (mapcar
                                  (lambda (url)
                                    (let ((buffer (make-buffer)))
                                      (set-url url :buffer buffer)
                                      buffer))
                                  urls))))
        (unless no-focus
          (if (open-external-link-in-new-window-p *browser*)
              (let ((window (window-make *browser*)))
                (window-set-active-buffer window first-buffer))
              (set-current-buffer first-buffer))))
    (error (c)
      (error "Could not make buffer to open ~a: ~a" urls c))))

@export
(defmethod request-resource ((buffer buffer) &key url event-type
                             (is-new-window nil) (is-known-type t) (mouse-button "")
                             (modifiers '()) &allow-other-keys)
  "Return non-nil to let platform port load URL.
   Return nil otherwise.

   Deal with URL with the following rules:
   - If IS-NEW-WINDOW is non-nil or if C-button1 was pressed, load in new buffer.
   - If IS-KNOWN-TYPE is nil, download the file.  (TODO: Implement it!)
   - Otherwise return non-nil to let the platform port load the URL."
  (declare (ignore event-type)) ; TODO: Do something with the event type?
  (cond
    ((or is-new-window
         ;; TODO: Streamline the customization of this binding.
         (and (or (equal modifiers '("C"))
                  (equal modifiers '("s")))
              (string= mouse-button "button1"))
         (string= mouse-button "button2"))
     (log:debug "Load URL in new buffer: ~a" url)
     (open-urls (list url) :no-focus (equal modifiers '("s")))
     t)
    ((not is-known-type)
     (log:info "Buffer ~a downloads ~a" buffer url)
     (download url :proxy-address (proxy-address buffer :downloads-only t)
                   :cookies "")
     (unless (find-buffer 'download-mode)
       (download-list))
     t)
    (t
     (log:debug "Forwarding back to platform port: ~a" url)
     nil)))

@export
(defun current-buffer ()
  "Get the active buffer for the active window."
  (match (ipc-window-active *browser*)
    ((guard w w) (active-buffer w))
    (_ (log:warn "No active window, picking last active buffer.")
       (last-active-buffer *browser*))))

(declaim (ftype (function (buffer)) set-current-buffer))
;; (declaim (ftype (function ((and buffer (not minibuffer)))) set-current-buffer)) ; TODO: Better.
;; But we can't use "minibuffer" here since it's not declared yet.  It will
;; crash Next if we call set-current-buffer before instantiating the first
;; minibuffer.
@export
(defun set-current-buffer (buffer)
  "Set the active buffer for the active window."
  (unless (eq 'minibuffer (class-name (class-of buffer)))
    (let ((active-window (ipc-window-active *browser*)))
      (if active-window
          (window-set-active-buffer active-window buffer)
          (make-window buffer))
      buffer)))

@export
(defun current-minibuffer ()
  "Return the currently active minibuffer."
  (first (active-minibuffers (last-active-window *browser*))))
