;;; interface.lisp --- interface state

(in-package :next)

;; Create necessary hook types. We must forward-declare the class
;; since the hook take the type of the class that hosts them.
(defclass buffer () ())
(defclass minibuffer () ())
(defclass window () ())
(hooks:define-hook-type window (function (window)))
(hooks:define-hook-type buffer (function (buffer)))
(hooks:define-hook-type minibuffer (function (minibuffer)))
(hooks:define-hook-type download (function (download-manager:download)))
(hooks:define-hook-type window-buffer (function (window buffer)))
(hooks:define-hook-type resource resource-handler-type)
(export-always 'make-handler-resource)

(defclass-export window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (active-minibuffers :accessor active-minibuffers :initform nil
                       :documentation "The stack of currently active minibuffers.")
   (status-buffer-height :accessor status-buffer-height :initform 16
                         :type integer
                         :documentation "The height of the status buffer in pixels.")
   (status-buffer-style :accessor status-buffer-style :initform
                        (cl-css:css
                         '((body
                            :background "rgb(224, 224, 224)"
                            :font-size "12px"
                            :color "rgb(32, 32, 32)"
                            :padding 0
                            :padding-left "4px"
                            :margin 0))))
   (message-buffer-height :accessor message-buffer-height :initform 16
                          :type integer
                          :documentation "The height of the message buffer in pixels.")
   (message-buffer-style :accessor message-buffer-style :initform
                         (cl-css:css
                          '((body
                             :font-size "12px"
                             :padding 0
                             :padding-left "4px"
                             :margin 0))))
   (minibuffer-open-height :accessor minibuffer-open-height :initform 256
                           :type integer
                           :documentation "The height of the minibuffer when open.")
   (input-dispatcher :accessor input-dispatcher
                     :initform #'dispatch-input-event
                     :type function
                     :documentation "Function to process input events.
It takes EVENT, BUFFER, WINDOW and PRINTABLE-P parameters.")
   (window-set-active-buffer-hook :accessor window-set-active-buffer-hook
                                  :initform (make-hook-window-buffer)
                                  :type hook-window-buffer
                                  :documentation "Hook run before `window-set-active-buffer' takes effect.
The handlers take the window and the buffer as argument.")
   (status-formatter :accessor status-formatter
                     :initform #'format-status
                     :type (function (window) string)
                     :documentation "Function of a window argument that returns
a string to be printed in the status view.

Example formatter that prints the buffer indices over the total number of buffers:

\(defun my-format-status (window)
  (declare (ignore window))
  (let* ((buffer (current-buffer))
         (buffer-count (1+ (or (position buffer
                                         (sort (alexandria:hash-table-values (buffers *browser*))
                                               #'<
                                               :key #'id))
                               0)))
    (format nil \"[~{~a~^ ~}] (~a/~a) ~a — ~a\"
            (mapcar (lambda (m) (str:replace-all \"-mode\" \"\"
                                                 (str:downcase
                                                  (class-name (class-of m)))))
                    (modes buffer))
            buffer-count
            (hash-table-count (buffers *browser*))
            (url buffer)
            (title buffer))))")
   (window-delete-hook :accessor window-delete-hook
                       :initform (make-hook-window)
                       :type hook-window
                       :documentation "Hook run after `ffi-window-delete' takes effect.
The handlers take the window as argument.")))

(defclass-export proxy ()
  ((server-address :accessor server-address :initarg :server-address
                   :initform "socks5://127.0.0.1:9050"
                   :type string
                   :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example: \"http://192.168.1.254:8080\".")
   (whitelist :accessor whitelist :initarg :whitelist
              :initform '("localhost" "localhost:8080")
              :type list-of-strings
              :documentation "A list of URIs not to forward to the proxy.")
   (proxied-downloads-p :accessor proxied-downloads-p :initarg :proxied-downloads-p
                        :initform t
                        :documentation "Non-nil if downloads should also use the proxy."))
  (:documentation "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."))

(define-class-type proxy)
(declaim (type (proxy-type) *proxy-class*))
(export-always '*proxy-class*)
(defparameter *proxy-class* 'proxy)

(defclass-export buffer ()
  ((id :accessor id :initarg :id :initform ""
       :documentation "Unique identifier for a buffer.
Dead buffers (i.e. those not associated with a web view) have an empty ID.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (url :accessor url :initarg :url :type string :initform "")
   (url-at-point :accessor url-at-point :type string :initform "")
   (title :accessor title :initarg :title :type string :initform "")
   (last-access :accessor last-access
                :initform (local-time:now)
                :type number
                :documentation "Timestamp when the buffer was last switched to.")
   (modes :accessor modes :initarg :modes :initform '()
          :documentation "The list of mode instances.")
   (default-modes :accessor default-modes :initarg :default-modes
                  :initform '(certificate-whitelist-mode web-mode root-mode)
                  :type list-of-symbols
                  :documentation "The symbols of the classes to instantiate on buffer creation.")
   (keymap-scheme-name
    :accessor keymap-scheme-name
    :initarg :keymap-scheme-name
    :initform scheme:cua
    :type keymap:scheme-name
    :documentation "The keymap scheme that will be used for all modes in the current buffer.")
   (override-map :accessor override-map
                 :initarg :override-map
                 :initform (let ((map (make-keymap "overide-map")))
                             (define-key map
                               "M-x" #'execute-command)
                             map)
                 :documentation "Keymap that overrides all other bindings.
No libraries should ever touch the override-map, this is left for the user to
customize to their needs.

Example:

\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"overide-map\")))
                             (define-key map
                               \"M-x\" #'execute-command
                               \"C-q\" #'quit)
                   map))))")
   (forward-input-events-p :accessor forward-input-events-p
                           :initarg :forward-input-events-p
                           :type boolean
                           :initform t
                           :documentation "When non-nil, keyboard events are
forwarded to the renderer when no binding is found.  Pointer
events (e.g. mouse events) are not affected by this, they are always
forwarded when no binding is found.")
   (last-event :accessor last-event
               :initform nil
               ;; TODO: Store multiple events?  Maybe when implementing keyboard macros.
               :documentation "The last event that was received for the current buffer.")
   (request-resource-scheme :accessor request-resource-scheme
                            :initarg :request-resource-scheme
                            :initform (define-scheme "request-resource"
                                        scheme:cua
                                        (list
                                         "C-button1" #'request-resource-open-url-focus
                                         "button2" #'request-resource-open-url-focus
                                         "C-shift-button1" #'request-resource-open-url))
                            :documentation "This keymap can be looked up when
`request-resource-hook' handlers run.
The functions are expected to take key arguments like `:url'.")
   (request-resource-hook :accessor request-resource-hook
                          :initarg :request-resource-hook
                          :initform (make-hook-resource
                                     :combination #'hooks:combine-hook-until-success
                                     :handlers (list (make-handler-resource #'request-resource)))
                          :documentation "Hook run on every resource load.
The handlers are run sequentially until one returns non-nil.
Newest hook is run first.
If :FORWARD is returned, the resource loading is deferred to the renderer.
If :STOP (or T) is returned, stop the the hook.")
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
renderers might support this.")
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
   (certificate-whitelist :accessor certificate-whitelist
                          :initform '()
                          :type list-of-strings
                          :documentation  "A list of hostnames for which certificate errors shall be ignored.")
   ;; TODO: Rename `load-hook' to `set-url-hook'?
   (load-hook :accessor load-hook
              :initform (hooks:make-hook-string->string
                         :combination #'hooks:combine-composed-hook)
              :type hooks:hook-string->string
              :documentation "Hook run in `set-url*' after `parse-url' was)))))
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
      (ffi-buffer-set-proxy buffer
                            (server-address proxy)
                            (whitelist proxy))
      (ffi-buffer-set-proxy buffer
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
(defparameter *command-list* '()
  "The list of known commands, for internal use only.")

(defun mode-list ()
  "Return the list of all namespaced mode symbols."
  (delete-if (complement (lambda (m)
                           (str:suffixp (list (symbol-name m) "-MODE")
                                        "-MODE")))
             (mapcar #'sym *command-list*)))

(defun mode-command (mode-symbol)
  "Return the mode toggle command.
We loop over `*command-list*' to find the mode command since a mode may be
defined in any package and is unique."
  (find-if (lambda (c)
             (eq (find-symbol (string mode-symbol) (pkg c))
                 (sym c)))
           *command-list*))

(defmethod initialize-modes ((buffer buffer))
  "Initialize BUFFER modes.
   This must be called after BUFFER has been created by the renderer.
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

(defmethod on-signal-load-committed ((buffer buffer) url)
  nil)

(defmethod on-signal-load-finished ((buffer buffer) url)
  (setf (url buffer) url)
  (with-result (title (%%buffer-get-title :buffer buffer))
    (setf (title buffer) title)
    ;; Warning: We can only dispatch on-signal-load-committed after the title has
    ;; been set, lest we get a race condition with the title being set too late.
    (dolist (mode (modes buffer))
      (on-signal-load-finished mode url))))

(defclass-export browser ()
  ((socket-thread :accessor socket-thread
                  :initform nil
                  :documentation "Thread that listens on socket.
See `socket-path'.
This slot is mostly meant to clean up the thread if necessary.")
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
   (total-window-count :initform 0
                       :type integer
                       :documentation "This is used to generate unique window
identifiers in `get-unique-window-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (last-active-window :initform nil
                       :type (or window null)
                       :documentation "Records the last active window.  This is
useful when no Next window is focused and we still want `ffi-window-active' to
return something.
See `current-window' for the user-facing function.")
   (last-active-buffer :accessor last-active-buffer :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :initform 0
                       :type integer
                       :documentation "This is used to generate unique buffer
identifiers in `get-unique-buffer-identifier'.  We can't rely on the windows
count since deleting windows may reseult in duplicate identifiers.")
   (startup-function :accessor startup-function
                     :type function
                     :initform #'default-startup
                     :documentation "The function run on startup.  It takes a
list of URLs (strings) as argument (the command line positional arguments).  It
is run after the renderer has been initialized and after the
`*after-init-hook*' has run.")
   (startup-error-reporter-function :accessor startup-error-reporter-function
                                    :type function
                                    :initarg :startup-error-reporter-function
                                    :initform nil
                                    :documentation "When supplied,
                                    upon startup, if there are errors,
                                    they will be reported by this
                                    function.")
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
                   :type trivial-types:association-list ; Value can be 1 or 2 strings.
                   :documentation "A list of the search engines.

The elements are in the form of a 3-tuple of strings (SHORTCUT SEARCH-URL FALLBACK-URL).
You can invoke them from the minibuffer by prefixing your query with SHORTCUT.
If the query is empty, FALLBACK-URL is loaded instead.  If
FALLBACK-URL is empty, SEARCH-URL is used on an empty search.

The 'default' engine is used when the query is not a valid URL, or the first
keyword is not recognized.")
   (key-stack :accessor key-stack :initform '()
              :documentation "A stack that keeps track of the key chords a user has pressed.")
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
   (history-data :initform nil
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
   (bookmarks-data :initform nil
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
   (session-path :initarg :session-path
                 :accessor session-path
                 :initform (expand-session-path "default.lisp")
                 :documentation "
The path where the system will create/save the session.")
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
                              (make-autofill :name "Hello Printer" :key "Function example" :fill (lambda () (format nil "hello!")))))
   (spell-check-language :accessor spell-check-language
                         :initform "en_US"
                         :documentation "Spell check language used by
                         Next. For a list of more languages available,
                         please view the documentation for
                         cl-enchant (broker-list-dicts).")))

(defmethod finalize ((browser browser) urls startup-timestamp)
  "Run `*after-init-hook*' then BROWSER's `startup-function'."
  ;; `messages-appender' requires `*browser*' to be initialized.
  (log4cl-impl:add-appender log4cl:*root-logger* (make-instance 'messages-appender))
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
          do (hooks:run-hook (after-download-hook *browser*))
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
  (hooks:run-hook (before-download-hook *browser*) url)
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
  (incf (slot-value browser 'total-window-count)))

(defmethod get-unique-buffer-identifier ((browser browser))
  (incf (slot-value browser 'total-buffer-count)))

(declaim (ftype (function (window buffer)) set-window-title))
(export-always 'set-window-title)
(defun set-window-title (window buffer)
  "Set current window title to 'Next - TITLE - URL.
If Next was started from a REPL, use 'Next REPL...' instead.
This is useful to tell REPL instances from binary ones."
  (let ((url (url buffer))
        (title (title buffer)))
    (setf title (if (str:emptyp title) "" title))
    (setf url (if (str:emptyp url) "<no url/name>" url))
    (ffi-window-set-title window
                          (str:concat "Next" (when *keep-alive* " REPL") " - "
                                       title (unless (str:emptyp title) " - ")
                                       url))))

(declaim (ftype (function (window buffer)) window-set-active-buffer))
(export-always 'window-set-active-buffer)
(defun window-set-active-buffer (window buffer) ; TODO: Rename window-set-buffer.
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                       :key #'active-buffer)))
    (hooks:run-hook (window-set-active-buffer-hook window) window buffer)
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (buffer-make *browser*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (ffi-window-set-active-buffer window-with-same-buffer temp-buffer)
          (ffi-window-set-active-buffer window buffer)
          (ffi-window-set-active-buffer window-with-same-buffer buffer-swap)
          (buffer-delete temp-buffer))
        (ffi-window-set-active-buffer window buffer))
    (setf (last-access buffer) (local-time:now))
    (setf (last-active-buffer *browser*) buffer)
    (set-window-title window buffer)
    (print-status)
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

(defun open-urls (urls &key no-focus)
  "Create new buffers from URLs.
   First URL is focused if NO-FOCUS is nil."
  (handler-case
      (let ((first-buffer (first (mapcar
                                  (lambda (url)
                                    (let ((buffer (make-buffer)))
                                      (set-url* url :buffer buffer)
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

(export-always 'request-resource)
(defun request-resource (buffer
                         &key url event-type
                           (is-new-window nil) (is-known-type t) (keys '())
                         &allow-other-keys)
  "Candidate for `request-resource-hook'.
Deal with URL with the following rules:
- If IS-NEW-WINDOW is non-nil or if C-button1 was pressed, load in new buffer.
- If IS-KNOWN-TYPE is nil, download the file.
- Otherwise return :FORWARD to let the renderer load the URL."
  (declare (ignore event-type)) ; TODO: Do something with the event type?
  (let* ((keymap (scheme-keymap buffer (request-resource-scheme buffer)))
         (bound-function (keymap:lookup-key keys keymap)))
    (cond
      (bound-function
       (log:debug "Resource request key sequence ~a" (keyspecs-with-optional-keycode keys))
       (funcall-safely bound-function :url url)
       t)
      (is-new-window
       (log:debug "Load URL in new buffer: ~a" url)
       (open-urls (list url))
       t)
      ((not is-known-type)
       (log:info "Buffer ~a downloads ~a" buffer url)
       (download url :proxy-address (proxy-address buffer :downloads-only t)
                     :cookies "")
       (unless (find-buffer 'download-mode)
         (download-list))
       t)
      (t
       (log:debug "Forwarding back to renderer: ~a" url)
       :forward))))

(defun javascript-error-handler (condition)
  (echo-warning "JavaScript error: ~a" condition))

(defun format-status (window)
  (declare (ignore window))
  (let ((buffer (current-buffer)))
    (format nil "[~{~a~^ ~}] ~a — ~a"
            (mapcar (lambda (m) (str:replace-all "-mode" ""
                                                 (str:downcase
                                                  (class-name (class-of m)))))
                    (modes buffer))
            (url buffer)
            (title buffer))))

(defun print-status ()
  (let ((window (current-window)))
    (when window
      (ffi-print-status
       window
       (funcall-safely (status-formatter window) window)))))

(defun print-message (message)
  (when (current-window)
    (ffi-print-message (current-window) message)))

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

(export-always 'current-buffer)
(defun current-buffer ()
  "Get the active buffer for the active window."
  (match (current-window)
    ((guard w w) (active-buffer w))
    (_ (log:debug "No active window, picking last active buffer.")
       (last-active-buffer *browser*))))

(declaim (ftype (function (buffer)) set-current-buffer))
;; (declaim (ftype (function ((and buffer (not minibuffer)))) set-current-buffer)) ; TODO: Better.
;; But we can't use "minibuffer" here since it's not declared yet.  It will
;; crash Next if we call set-current-buffer before instantiating the first
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

;;; FFI Declarations:
;;; These are used to deal with the asymmetry of supported operations
;;; in each renderer. If a given method does not exist for a renderer,
;;; it will bubble up to one of these methods and signal a condition
;;; to the user.

(define-condition unsupported-operation (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Unsupported operation for this renderer."))))

(defmacro define-ffi-method (name arguments)
  `(progn
     (export-always ',name)
     (defmethod ,name (,@arguments)
       (declare (ignore ,@(delete-if (lambda (sym) (str:starts-with-p "&" (string sym)))
                                     (mapcar (alex:compose #'first #'uiop:ensure-list)
                                             arguments))))
       (error 'unsupported-operation))))

(define-ffi-method ffi-window-delete ((window window)))
(define-ffi-method ffi-window-fullscreen ((window window)))
(define-ffi-method ffi-window-unfullscreen ((window window)))
(define-ffi-method ffi-window-make ((browser browser)))
(define-ffi-method ffi-window-to-foreground ((window window)))
(define-ffi-method ffi-window-set-title ((window window) title))
(define-ffi-method ffi-window-active ((browser browser)))
(define-ffi-method ffi-window-set-active-buffer ((window window) (buffer buffer)))
(define-ffi-method ffi-window-set-minibuffer-height ((window window) height))
(define-ffi-method ffi-buffer-make ((browser browser) &key title default-modes))
(define-ffi-method ffi-buffer-delete ((buffer buffer)))
(define-ffi-method ffi-buffer-load ((buffer buffer) uri))
(define-ffi-method ffi-buffer-evaluate-javascript ((buffer buffer) javascript &key callback))
(define-ffi-method ffi-minibuffer-evaluate-javascript ((window window) javascript &key callback))
(define-ffi-method ffi-buffer-enable-javascript ((buffer buffer) value))
(define-ffi-method ffi-buffer-enable-javascript-markup ((buffer buffer) value))
(define-ffi-method ffi-buffer-set-proxy ((buffer buffer) &optional proxy-uri (ignore-hosts (list nil))))
(define-ffi-method ffi-buffer-get-proxy ((buffer buffer)))
(define-ffi-method ffi-generate-input-event ((window window) event))
(define-ffi-method ffi-generated-input-event-p ((window window) event))
(define-ffi-method ffi-within-renderer-thread ((browser browser) thunk))
(define-ffi-method ffi-kill-browser ((browser browser)))
(define-ffi-method ffi-initialize ((browser browser) urls startup-timestamp))
(define-ffi-method ffi-inspector-show ((buffer buffer)))
(define-ffi-method ffi-print-status ((window window) text))
(define-ffi-method ffi-print-message ((window window) message))

(define-class-type browser)
(declaim (type (browser-type) *browser-class*))
(export-always '*browser-class*)
(defparameter *browser-class* 'browser)
