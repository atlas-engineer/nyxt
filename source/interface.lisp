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
   `ipc-window-set-active-buffer' takes effect. The handlers take the
   window and the buffer as argument.")
   (window-delete-hook :accessor window-delete-hook
                       :initform (make-hook-window)
                       :type hook-window
                       :documentation "Hook run after
    `ipc-window-delete' takes effect.  The handlers take the window as
    argument.")))

(define-class-type window)
(declaim (type (window-type) *window-class*))
@export
(defparameter *window-class* 'window)

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
   (last-key-chords :accessor last-key-chords
                    :initform '()
                    :type trivial-types:proper-list
                    ;; TODO: Store multiple key chords?  Maybe when implementing keyboard macros.
                    :documentation "The last key chords that were received for the current buffer.
For now we only store the very last key chord.")
   (resource-query-function :accessor resource-query-function
                            ;; TODO: What about having multiple functions?  And what about moving this to modes?
                            :initarg :resource-query-function
                            :initform #'resource-query-default)
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
                       :documentation "Hook run before `ipc-buffer-delete' takes effect.
The handlers take the buffer as argument.")))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
@export
(defparameter *buffer-class* 'buffer)

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (ipc-set-proxy buffer
                     (server-address proxy)
                     (whitelist proxy))
      (ipc-set-proxy buffer
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
   See `ipc-buffer-make'."
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
(defclass interface ()
  ((port :accessor port :initform nil
         :documentation "The CLOS object responible for handling the platform port.")
   (platform-port-poll-duration :accessor platform-port-poll-duration :initform 1.0
                                 :type number
                                :documentation "The duration in seconds to wait
for the platform port to start up.")
   (platform-port-poll-interval :accessor platform-port-poll-interval :initform 0.025
                                :type number
                                :documentation "The speed at which to poll the
IPC endpoint of a platform-port to see if it is ready to begin accepting IPC
commands.")
   (active-connection :accessor active-connection :initform nil)
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
   (total-window-count :accessor total-window-count :initform 0)
   (last-active-window :accessor last-active-window :initform nil)
   (last-active-buffer :accessor last-active-buffer :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count :initform 0 :type integer)
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
   (history-db-path :initarg :history-db-path
                    :accessor history-db-path
                    :type pathname
                    :initform (xdg-data-home "history.lisp")
                    :documentation "
Deprecated.  See `history-path'.")
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
   (bookmark-db-path :initarg :bookmark-db-path
                     :accessor bookmark-db-path
                     :initform (xdg-data-home "bookmarks.lisp")
                     :documentation "
Deprecated.  See `bookmarks-path'.")
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
   ;; Hooks follow:
   (before-exit-hook :accessor before-exit-hook
                     :initform (next-hooks:make-hook-void)
                     :type next-hooks:hook-void
                     :documentation "Hook run before both `*interface*' and the
platform port get terminated.  The handlers take no argument.")
   (window-make-hook :accessor window-make-hook
                     :initform (make-hook-window)
                     :type hook-window
                     :documentation "Hook run after `ipc-window-make'.
The handlers take the window as argument.")
   (buffer-make-hook :accessor buffer-make-hook
                     :initform (make-hook-buffer)
                     :type hook-buffer
                     :documentation "Hook run after `ipc-buffer-make' and before `ipc-buffer-load'.
It is run before `initialize-modes' so that the default mode list can still be
altered from the hooks.
The handlers take the buffer as argument.")
   (buffer-before-make-hook :accessor buffer-before-make-hook
                            :initform (make-hook-buffer)
                            :type hook-buffer
                            :documentation "Hook run before `ipc-buffer-make'.
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

(define-class-type interface)
(declaim (type (interface-type) *interface-class*))
@export
(defparameter *interface-class* 'interface)

;; Catch a common case for a better error message.
(defmethod buffers :before ((interface t))
  (when (null interface)
    (error "There is no current *interface*. Is Next started?")))

(defun search-engines-names (&optional (interface *interface*))
  "Return a list of search engines names."
  (mapcar (lambda (tuple)
            (car tuple))
          (search-engines interface)))

(defun search-engine-starting-with (prefix)
  "Return the first search engine name that starts with PREFIX."
  (loop for name in (search-engines-names)
     when (str:starts-with-p prefix name)
     return name))

(defmethod bookmark-db-path ((interface interface))
  (log:warn "Deprecated, use `bookmarks-path' instead.")
  (bookmarks-path interface))

(defmethod history-db-path ((interface interface))
  (log:warn "Deprecated, use `history-path' instead.")
  (history-path interface))

(defmethod history-data ((interface interface))
  "Return the `history-data' slot from INTERFACE.
If empty, the history data is initialized with `history-restore-function'."
  (when (and (null (slot-value interface 'history-data))
             (history-restore-function interface))
    (funcall (history-restore-function interface)))
  (slot-value interface 'history-data))

(defmethod (setf history-data) (value (interface interface))
  "Set `history-data' to VALUE.
Persist the `history-data' slot from INTERFACE to `history-path' with
`history-store-function'."
  (setf (slot-value interface 'history-data) value)
  (match (history-store-function interface)
    ((guard f f) (funcall f))))

(defmethod bookmarks-data ((interface interface))
  "Return the `bookmarks-data' slot from INTERFACE.
If empty, the bookmarks data is initialized with `bookmarks-restore-function'."
  (when (and (null (slot-value interface 'bookmarks-data))
             (bookmarks-restore-function interface))
    (funcall (bookmarks-restore-function interface)))
  (slot-value interface 'bookmarks-data))

(defmethod (setf bookmarks-data) (value (interface interface))
  "Set `bookmarks-data' to VALUE.
Persist the `bookmarks-data' slot from INTERFACE to `bookmarks-path' with
`bookmarks-store-function'."
  (setf (slot-value interface 'bookmarks-data) value)
  (match (bookmarks-store-function interface)
    ((guard f f) (funcall f))))

(declaim (ftype (function (buffer)) add-to-recent-buffers))
(defun add-to-recent-buffers (buffer)
  "Create a recent-buffer from given buffer and add it to `recent-buffers'."
  ;; Make sure it's a dead buffer:
  (setf (id buffer) "")
  (ring:delete-match (recent-buffers *interface*) (buffer-match-predicate buffer))
  (ring:insert (recent-buffers *interface*) buffer))

(defun download-watch ()
  "Update the download-list buffer.
This function is meant to be run in the background."
  ;; TODO: Add a (sleep ...)?  If we have many downloads, this loop could result
  ;; in too high a frequency of refreshes.
  (loop for d = (lparallel:receive-result download-manager:*notifications*)
        while d
        when (download-manager:finished-p d)
          do (next-hooks:run-hook (after-download-hook *interface*))
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
  (next-hooks:run-hook (before-download-hook *interface*) url)
  (when (eq proxy-address :auto)
    (setf proxy-address (proxy-address (current-buffer)
                                       :downloads-only t)))
  (let* ((download nil))
    (handler-case
        (progn
          (setf download (download-manager:resolve
                          url
                          :directory (download-directory *interface*)
                          :cookies cookies
                          :proxy proxy-address))
          (push download (downloads *interface*))
          download)
      (error (c)
        (echo-warning "Download error: ~a" c)
        nil))))

(defmethod get-unique-window-identifier ((interface interface))
  (incf (total-window-count interface)))

(defmethod get-unique-buffer-identifier ((interface interface))
  (incf (total-buffer-count interface)))

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
(defun window-set-active-buffer (window buffer)
  ;; TODO: Replace this swapping business with a simple swap + a "refresh rendering" IPC call?
  (let ((window-with-same-buffer (find-if
                                  (lambda (other-window) (and (not (eq other-window window))
                                                              (eql (active-buffer other-window) buffer)))
                                  (alexandria:hash-table-values (windows *interface*)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (ipc-buffer-make *interface*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (ipc-window-set-active-buffer window-with-same-buffer temp-buffer)
          (ipc-window-set-active-buffer window buffer)
          (ipc-window-set-active-buffer window-with-same-buffer buffer-swap)
          (ipc-buffer-delete temp-buffer))
        (ipc-window-set-active-buffer window buffer))
    (set-window-title window buffer)
    (echo-dismiss)
    (setf (active-buffer window) buffer)))

(defun %get-inactive-buffer ()
  "Return inactive buffer or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer
                  (alexandria:hash-table-values (windows *interface*))))
        (buffers (buffer-list)))
    (match (set-difference buffers active-buffers)
      ((guard diff diff)
       ;; Display the most recent inactive buffer.
       (first (sort diff #'local-time:timestamp> :key #'last-access))))))

(defun buffer-did-commit-navigation (buffer-id url)
    (let ((buffer (gethash buffer-id (buffers *interface*))))
      (did-commit-navigation buffer url))
  (values))

(defun buffer-did-finish-navigation (buffer-id url)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-finish-navigation buffer url))
  (values))

(defun buffer-uri-at-point (url)
  (if (str:emptyp url)
      (echo-dismiss)
      (echo "→ ~a" url))
  (values))

(defun make-buffers (urls)
  (open-urls urls)
  (values))

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
          (if (open-external-link-in-new-window-p *interface*)
              (let ((window (ipc-window-make)))
                (window-set-active-buffer window first-buffer))
              (set-current-buffer first-buffer))))
    (error (c)
      (error "Could not make buffer to open ~a: ~a" urls c))))

@export
(defmethod resource-query-default ((buffer buffer)
                                   &key url
                                     (cookies "")
                                     event-type
                                     (is-new-window nil)
                                     (is-known-type t)
                                     (mouse-button "")
                                     (modifiers '())
                                   &allow-other-keys)
  "Return non-nil to let platform port load URL.
Return nil otherwise.

Deal with URL with the following rules:
- If IS-NEW-WINDOW is non-nil or if C-button1 was pressed, load in new buffer.
- If IS-KNOWN-TYPE is nil, download the file.  (TODO: Implement it!)
- Otherwise return non-nil to let the platform port load the URL."
  (declare (ignore event-type))         ; TODO: Do something with the event type?
  (cond
    ((or is-new-window
         ;; TODO: Streamline the customization of this binding.
         (and (or (equal modifiers '("C"))
                  (equal modifiers '("s")))
              (string= mouse-button "button1"))
         (string= mouse-button "button2"))
     (log:debug "Load in new buffer: ~a" url)
     (open-urls
      (list url)
      :no-focus (equal modifiers '("s")))
     nil)
    ((not is-known-type)
     (log:info "Buffer ~a downloads ~a" buffer url)
     (download url :proxy-address (proxy-address buffer :downloads-only t)
               :cookies cookies)
     (unless (find-buffer 'download-mode)
       (download-list))
     nil)
    (t
     (log:debug "Forwarding back to platform port: ~a" url)
     t)))

;; Return non-nil to tell the platform port to load the URL.
(defun request-resource
    (buffer-id url cookies event-type is-new-window
     is-known-type mouse-button modifiers)
  (unless (member-string event-type '("other"
                                      "link-click"
                                      "form-submission"
                                      "form-resubmission"
                                      "backward-or-forward"
                                      "reload"))
    (setf event-type "other"))
  (setf event-type (intern event-type "KEYWORD"))
  ;; TODO: We need to define an EVENT type, e.g. with
  ;; (deftype event () '(member :other :link-click ...))
  ;; https://stackoverflow.com/questions/578290/common-lisp-equivalent-to-c-enums#
  (log:debug "Request resource ~s with mouse ~s, modifiers ~a" url mouse-button modifiers)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (if buffer
        (funcall (resource-query-function buffer)
                 buffer
                 :url url
                 :cookies cookies
                 :event-type event-type
                 :is-new-window is-new-window
                 :is-known-type is-known-type
                 :mouse-button mouse-button
                 :modifiers modifiers)
        (progn
          (log:debug "no buffer of id '~a'~&" buffer-id)
          t))))

;; Convenience methods and functions for users of the API.

@export
(defun current-buffer ()
  "Get the active buffer for the active window."
  (match (ipc-window-active *interface*)
    ((guard w w) (active-buffer w))
    (_ (log:warn "No active window, picking last active buffer.")
       (last-active-buffer *interface*))))

(declaim (ftype (function (buffer)) set-current-buffer))
;; (declaim (ftype (function ((and buffer (not minibuffer)))) set-current-buffer)) ; TODO: Better.
;; But we can't use "minibuffer" here since it's not declared yet.  It will
;; crash Next if we call set-current-buffer before instantiating the first
;; minibuffer.
@export
(defun set-current-buffer (buffer)
  "Set the active buffer for the active window."
  (unless (eq 'minibuffer (class-name (class-of buffer)))
    (let ((active-window (ipc-window-active *interface*)))
      (if active-window
          (window-set-active-buffer active-window buffer)
          (make-window buffer))
      buffer)))

@export
(defun current-minibuffer ()
  "Return the currently active minibuffer."
  (first (active-minibuffers (last-active-window *interface*))))
