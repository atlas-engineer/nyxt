;;; remote.lisp --- remote gui interface

;; We prefix all functions communicating over RPC with "rpc-".

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (active-minibuffers :accessor active-minibuffers :initform nil
                       :documentation "The stack of currently active minibuffers.")
   (status-buffer :accessor status-buffer ;; TODO: Use a separate class for status bars once the platform port has a separate display.
                  :initform (make-instance 'minibuffer)
                  :documentation "Buffer for displaying information such as
current URL or event messages.")
   (status-buffer-height :accessor status-buffer-height :initform 36
                         :documentation "The height of the status buffer in pixels.")
   (minibuffer-callbacks :accessor minibuffer-callbacks
                         :initform (make-hash-table :test #'equal))
   (minibuffer-open-height :accessor minibuffer-open-height :initform 200
                           :type integer
                           :documentation "The height of the minibuffer when open.")
   (window-set-active-buffer-hook :accessor window-set-active-buffer-hook :initform '() :type list
                                  :documentation "Hook run before `rpc-window-set-active-buffer' takes effect.
The handlers take the window and the buffer as argument.")
   (window-delete-hook :accessor window-delete-hook :initform '() :type list
                       :documentation "Hook run after `rpc-window-delete' takes effect.
The handlers take the window as argument.")))

@export
@export-accessors
(defclass proxy ()
  ;; TODO: for the PyQt side, we now want the protocol, the IP and the
  ;; port on different slots.
  ((server-address :accessor server-address :initarg :server-address
                   :initform "socks5://127.0.0.1:9050"
                   :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example:
  http://192.168.1.254:8080")
   (whitelist :accessor whitelist :initarg :whitelist
              :initform '("localhost" "localhost:8080")
              :documentation "A list of URI not to forward to the proxy.
It must be a list of strings.")
   (proxied-downloads-p :accessor proxied-downloads-p :initarg :proxied-downloads-p
                        :initform t
                        :documentation "Non-nil if downloads should also use
the proxy."))
  (:documentation "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."))

@export
@export-accessors
(defclass buffer ()
  ((id :accessor id :initarg :id :initform ""
       :documentation "Unique identifier for a buffer.  Dead buffers (i.e. those
not associated with a web view) have an empty ID.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (url :accessor url :initarg :url :type string :initform "")
   (title :accessor title :initarg :title :type string :initform "")
   (modes :accessor modes :initarg :modes :initform '()
          :documentation "The list of mode instances.")
   (default-modes :accessor default-modes :initarg :default-modes
                  :initform '(web-mode root-mode)
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
                    ;; TODO: Store multiple key chords?  Maybe when implementing keyboard macros.
                    :documentation "The last key chords that were received for the current buffer.
For now we only store the very last key chord.")
   (resource-query-function :accessor resource-query-function
                            ;; TODO: What about having multiple functions?  And what about moving this to modes?
                            :initarg :resource-query-function
                            :initform #'resource-query-default)
   (callbacks :accessor callbacks
              :initform (make-hash-table :test #'equal))
   (default-new-buffer-url :accessor default-new-buffer-url :initform "https://next.atlas.engineer/start"
                           :documentation "The URL set to a new blank buffer opened by Next.")
   (scroll-distance :accessor scroll-distance :initform 50
                    :documentation "The distance scroll-down or scroll-up will scroll.")
   (horizontal-scroll-distance :accessor horizontal-scroll-distance :initform 50
                               :documentation "Horizontal scroll distance. The
distance scroll-left or scroll-right will scroll.")
   (current-zoom-ratio :accessor current-zoom-ratio :initform 1.0
                       :documentation "The current zoom relative to the default zoom.")
   (zoom-ratio-step :accessor zoom-ratio-step :initform 0.2
                    :documentation "The step size for zooming in and out.")
   (zoom-ratio-min :accessor zoom-ratio-min :initform 0.2
                   :documentation "The minimum zoom ratio relative to the default.")
   (zoom-ratio-max :accessor zoom-ratio-max :initform 5.0
                   :documentation "The maximum zoom ratio relative to the default.")
   (zoom-ratio-default :accessor zoom-ratio-default :initform 1.0
                       :documentation "The default zoom ratio.")
   (cookies-path :accessor cookies-path
                 :initform (xdg-data-home "cookies.txt")
                 :documentation "The path where cookies are stored.  Not all
platform ports might support this.")
   (box-style :accessor box-style
              :initform (cl-css:inline-css
                         '(:background "linear-gradient(to bottom, #FFF785, #C38A22)"
                           :color "black"
                           :border "1px #C38A22 solid"
                           :font-weight "bold"
                           :padding "1px 3px 0px 3px"
                           :text-transform "lowercase"
                           :padding "1px 3px 0px 3px"
                           :text-align "center"
                           :text-shadow "0 3px 7px 0px rgba(0,0,0,0.3)"
                           :border-radius "3px"
                           ;; Ensure the hint is above all the page elements.
                           ;; https://developer.mozilla.org/en-US/docs/Web/CSS/z-index
                           ;; TODO: The highest integer value is non-standard,
                           ;; so the following might depend on the web renderer.
                           ;; https://developer.mozilla.org/en-US/docs/Web/CSS/integer#Syntax
                           :z-index #.(1- (expt 2 31))))
              :documentation "The style of the boxes, e.g. link hints.")
   (proxy :initform nil :type :proxy
          :documentation "Proxy for buffer.")
   ;; TODO: Rename `load-hook' to `set-url-hook'?
   (load-hook :accessor load-hook :initform '() :type list
              :documentation "Hook run in `set-url' after `parse-url' was
processed.  The handlers take the URL going to be loaded as argument and must
return a (possibly new) URL.")
   (buffer-delete-hook :accessor buffer-delete-hook :initform '() :type list
                       :documentation "Hook run before `rpc-buffer-delete' takes effect.
The handlers take the buffer as argument.")))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (rpc-set-proxy buffer
                     (server-address proxy)
                     (whitelist proxy))
      (rpc-set-proxy buffer
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
See `rpc-buffer-make'."
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
(defclass remote-interface ()
  ((port :accessor port :initform (make-instance 'port)
         :documentation "The CLOS object responible for handling the platform port.")
   (platform-port-poll-duration :accessor platform-port-poll-duration :initform 1.0
                                :documentation "The duration in seconds to wait
for the platform port to start up.")
   (platform-port-poll-interval :accessor platform-port-poll-interval :initform 0.025
                                :documentation "The speed at which to poll the
RPC endpoint of a platform-port to see if it is ready to begin accepting RPC
commands.")
   (active-connection :accessor active-connection :initform nil)
   (password-interface :accessor password-interface
                       :initform (cond ((executable-find "pass")
                                        (make-instance 'password-store-interface))
                                       ((executable-find "keepassxc-cli")
                                        (make-instance 'keepassxc-interface))
                                       (t nil)))
   (dbus-pid :accessor dbus-pid :initform nil :type :number
             :documentation "The process identifier of the dbus instance started
by Next when the user session dbus instance is not available.")
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
   (total-buffer-count :accessor total-buffer-count :initform 0)
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
                   :documentation "An association list of the search engines.

The elements are in the form (SHORTCUT SEARCH-URL FALLBACK-URL).  You can invoke
them from the minibuffer by prefixing your query with SHORTCUT.  If the query is
empty, FALLBACK-URL is loaded instead.  If FALLBACK-URL is empty, SEARCH-URL is
used on an empty search.

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
   (before-exit-hook :accessor before-exit-hook :initform '() :type list
                     :documentation "Hook run before both `*interface*' and the
platform port get terminated.  The handlers take no argument.")
   (window-make-hook :accessor window-make-hook :initform '() :type list
                     :documentation "Hook run after `rpc-window-make'.
The handlers take the window as argument.")
   (buffer-make-hook :accessor buffer-make-hook :initform '() :type list
                     :documentation "Hook run after `rpc-buffer-make' and before `rpc-buffer-load'.
It is run before `initialize-modes' so that the default mode list can still be
altered from the hooks.
The handlers take the buffer as argument.")
   (buffer-before-make-hook :accessor buffer-make-hook :initform '() :type list
                     :documentation "Hook run before `rpc-buffer-make'.
This hook is mostly useful to set the `cookies-path'.
The buffer web view is not allocated, so it's not possible to run any
parenscript from this hook.  See `buffer-make-hook' for a hook.
The handlers take the buffer as argument.")
   (minibuffer-make-hook :accessor minibuffer-make-hook :initform '() :type list
                         :documentation "Hook run after the `minibuffer' class
is instantiated and before initializing the minibuffer modes.
The handlers take the minibuffer as argument.")
   (before-download-hook :accessor buffer-download-hook :initform '() :type list
                         :documentation "Hook run before downloading a URL.
The handlers take the URL as argument.")
   (after-download-hook :accessor after-download-hook :initform '() :type list
                        :documentation "Hook run after a download has completed.
The handlers take the `download-manager:download' class instance as argument.")))

(defmethod bookmark-db-path ((interface remote-interface))
  (log:warn "Deprecated, use `bookmarks-path' instead.")
  (bookmarks-path interface))

(defmethod history-db-path ((interface remote-interface))
  (log:warn "Deprecated, use `history-path' instead.")
  (history-path interface))

(defmethod history-data ((interface remote-interface))
  "Return the `history-data' slot from INTERFACE.
If empty, the history data is initialized with `history-restore-function'."
  (when (and (null (slot-value interface 'history-data))
             (history-restore-function interface))
    (funcall (history-restore-function interface)))
  (slot-value interface 'history-data))

(defmethod (setf history-data) (value (interface remote-interface))
  "Set `history-data' to VALUE.
Persist the `history-data' slot from INTERFACE to `history-path' with
`history-store-function'."
  (setf (slot-value interface 'history-data) value)
  (match (history-store-function interface)
    ((guard f f) (funcall f))))

(defmethod bookmarks-data ((interface remote-interface))
  "Return the `bookmarks-data' slot from INTERFACE.
If empty, the bookmarks data is initialized with `bookmarks-restore-function'."
  (when (and (null (slot-value interface 'bookmarks-data))
             (bookmarks-restore-function interface))
    (funcall (bookmarks-restore-function interface)))
  (slot-value interface 'bookmarks-data))

(defmethod (setf bookmarks-data) (value (interface remote-interface))
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
          do (hooks:run-hook (hooks:object-hook *interface* 'after-download-hook))
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
;; need to query the cookies for URL.  Thus we need to add an RPC endpoint to
;; query cookies.
(defun download (url &key
                     cookies
                     (proxy-address :auto))
  "Download URI.
When PROXY-ADDRESS is :AUTO (the default), the proxy address is guessed from the
current buffer."
  (hooks:run-hook (hooks:object-hook *interface* 'before-download-hook) url)
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

(declaim (ftype (function (remote-interface &key (:non-interactive boolean))) ensure-dbus-session))
(defun ensure-dbus-session (interface &key non-interactive)
  "Start a dbus session if necessary."
  (handler-case
      (dbus:with-open-bus (bus (session-server-addresses))
        ;; Dummy call to make sure dbus session is accessible.
        ;; We make sure we are authorized to request a name on the bus.  This is
        ;; important to, among others, detect if dbus-broker is running instead
        ;; of dbus, which is not compatible with cl-dbus as of 2019-09-05.
        (dbus:request-name bus +core-name+ :do-not-queue)
        t)
    (error ()
      (match (mapcar (lambda (s) (str:split "=" s :limit 2))
                     (str:split "
"
                                (handler-case
                                    (apply #'run-program-to-string +dbus-launch-command+)
                                  (error (c)
                                    (log:error "Failed to run ~a: ~a" +dbus-launch-command+ c)
                                    (when non-interactive
                                      (uiop:quit))))))
        ((list (list _ address) (list _ pid))
         (log:info "D-Bus session inaccessible, starting our own one.~%  Old D-Bus addresses: ~a~%  New D-Bus address: ~a"
                   (list (uiop:getenv "DBUS_SESSION_BUS_ADDRESS")
                         (uiop:getenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET"))
                   address)
         (setf (uiop:getenv "DBUS_SESSION_BUS_ADDRESS") address)
         (setf (uiop:getenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET") address)
         (setf (dbus-pid interface) (parse-integer pid)))))))

(defmethod initialize-instance :after ((interface remote-interface)
                                       &key non-interactive
                                       &allow-other-keys)
  "Start the RPC server."
  (ensure-dbus-session interface
                       :non-interactive non-interactive)
  (let ((lock (bt:make-lock))
        (condition (bt:make-condition-variable)))
    (setf (active-connection interface)
          (bt:make-thread
           (lambda ()
             (log:info "D-Bus addresses: ~a"
                       (mapcar #'dbus/transport-unix::server-address-socket-address
                               (session-server-addresses)))
             (dbus:with-open-bus (bus (session-server-addresses))
               (let ((status (dbus:request-name bus +core-name+ :do-not-queue)))
                 (when (eq status :exists)
                   (let ((url-list (or *free-args*
                                       (list (get-default 'buffer 'default-new-buffer-url)))))
                     (log:info "Next already started, requesting to open URL(s) ~a." url-list)
                     (handler-case
                         (%rpc-send-self "make_buffers" "as" url-list)
                       (error ()
                         (log:error "Can't communicate with existing Next.
Make sure to kill existing processes or if you were running Next from a REPL, kill the interface:

(next::kill-interface next::*interface*)
")))
                     (when non-interactive
                       (uiop:quit)))))
               (log:info "Bus connection name: ~A" (dbus:bus-name bus))
               (bt:condition-notify condition)
               (dbus:publish-objects bus)))))
    (bt:acquire-lock lock)
    (bt:condition-wait condition lock)))

(defun session-server-addresses ()
  (or
   (dbus:session-server-addresses)
   ;; Check for MacOS dbus session address.
   (dbus:parse-server-addresses-string
    (format nil "unix:path=~a"
            (uiop:getenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET")))))

(defmethod kill-interface ((interface remote-interface))
  "Stop the RPC server."
  (when (active-connection interface)
    (log:debug "Stopping server")
    ;; TODO: How do we close the connection properly?
    (ignore-errors (bt:destroy-thread (active-connection interface)))
    (when (dbus-pid interface)
      (kill-program (dbus-pid interface)))))

(defun %rpc-send-self (method-name signature &rest args)
  "Call METHOD over ARGS.
SIGNATURE must be the D-Bus encoded type string of ARGS.
For an array of string, that would be \"as\"."
  (dbus:with-open-bus (bus (session-server-addresses))
    (dbus:invoke-method (dbus:bus-connection bus) method-name
                        :path +core-object-path+
                        :destination +core-name+
                        :interface +core-interface+
                        :signature signature
                        :arguments args)))

(declaim (ftype (function (string &rest t)) %rpc-send))
(defun %rpc-send (method &rest args)
  "Call RPC method METHOD over ARGS."
  ;; D-Bus calls should raise an error so that we can detect errors when telling
  ;; another instance to open URLs.
  (dbus:with-open-bus (bus (session-server-addresses))
    ;; TODO: Make %rpc-send asynchronous?
    ;; If the platform port ever hangs, the next %rpc-send will hang the Lisp core too.
    (dbus:with-introspected-object (platform-port bus +platform-port-object-path+ +platform-port-name+)
      (apply #'platform-port +platform-port-interface+ method args))))

;; TODO: Move to separate packages:
;; - next-rpc
;; - next-script (?)
(defun rpc-list-methods ()
  "Return the unsorted list of RPC methods supported by the platform port."
  ;; TODO: Find the right way to do this in dbus.
  (%rpc-send "listMethods"))

(defun get-unique-window-identifier ()
  (format nil "~a" (1+ (total-window-count *interface*))))

(defmethod get-unique-buffer-identifier ()
  (format nil "~a" (1+ (total-buffer-count *interface*))))

@export
(defun rpc-window-make ()
  "Create a window and return the window object.
Run INTERFACE's `window-make-hook' over the created window."
  (let* ((window-id (get-unique-window-identifier))
         (window (make-instance 'window :id window-id)))
    (setf (gethash window-id (windows *interface*)) window)
    (incf (total-window-count *interface*))
    (%rpc-send "window_make" window-id)
    (unless (last-active-window *interface*)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and rpc-window-active would then return nil.
      (setf (last-active-window *interface*) window))
    (hooks:run-hook (hooks:object-hook *interface* 'window-make-hook) window)
    window))

(declaim (ftype (function (window string)) rpc-window-set-title))
@export
(defun rpc-window-set-title (window title)
  "Set the title for a given window."
  (%rpc-send "window_set_title" (id window) title))

(declaim (ftype (function (window)) rpc-window-delete))
@export
(defun rpc-window-delete (window)
  "Delete a window object and remove it from the hash of windows.
Once deleted, the `window-will-close' RPC endpoint will be called, running
INTERFACE's `window-delete-hook' over WINDOW."
  (%rpc-send "window_delete" (id window)))

@export
(defun rpc-window-active ()
  "Return the window object for the currently active window."
  (let ((window (gethash (%rpc-send "window_active")
                         (windows *interface*))))
    (when window
      (setf (last-active-window *interface*) window))
    (last-active-window *interface*)))

(declaim (ftype (function (window)) rpc-window-exists))
@export
(defun rpc-window-exists (window)
  "Return if a window exists."
  (%rpc-send "window_exists" (id window)))

(declaim (ftype (function (window buffer)) rpc-window-set-active-buffer))
@export
(defun rpc-window-set-active-buffer (window buffer)
  "Set INTERFACE's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (hooks:object-hook window 'window-set-active-buffer-hook) window buffer)
  (%rpc-send "window_set_active_buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer)
  (when (and window buffer)
    (setf (last-active-buffer *interface*) buffer))
  buffer)

(declaim (ftype (function (window buffer)) set-window-title))
@export
(defun set-window-title (window buffer)
  "Set current window title to 'Next - TITLE - URL."
  (let ((url (url buffer))
        (title (title buffer)))
    (setf title (if (str:emptyp title) "" title))
    (setf url (if (str:emptyp url) "<no url/name>" url))
    (rpc-window-set-title window
                          (concatenate 'string "Next - "
                                       title (unless (str:emptyp title) " - ")
                                       url))))

(declaim (ftype (function (window buffer)) window-set-active-buffer))
@export
(defun window-set-active-buffer (window buffer)
  ;; TODO: Replace this swapping business with a simple swap + a "refresh rendering" RPC call?
  (let ((window-with-same-buffer (find-if
                                  (lambda (other-window) (and (not (eq other-window window))
                                                              (eql (active-buffer other-window) buffer)))
                                  (alexandria:hash-table-values (windows *interface*)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (rpc-buffer-make))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (rpc-window-set-active-buffer window-with-same-buffer temp-buffer)
          (rpc-window-set-active-buffer window buffer)
          (rpc-window-set-active-buffer window-with-same-buffer buffer-swap)
          (rpc-buffer-delete temp-buffer))
        (rpc-window-set-active-buffer window buffer))
    (set-window-title window buffer)
    (setf (active-buffer window) buffer)))

(declaim (ftype (function (window integer)) rpc-window-set-minibuffer-height))
@export
(defun rpc-window-set-minibuffer-height (window height)
  (%rpc-send "window_set_minibuffer_height" (id window) height))

(declaim (ftype (function (&key (:title string) (:default-modes list) (:dead-buffer buffer))) rpc-buffer-make))
@export
(defun rpc-buffer-make (&key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
Run `*interface*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let* ((buffer (if dead-buffer
                     (progn (setf (id dead-buffer) (get-unique-buffer-identifier))
                            dead-buffer)
                     (apply #'make-instance 'buffer :id (get-unique-buffer-identifier)
                            (append (when title `(:title ,title))
                                    (when default-modes `(:default-modes ,default-modes)))))))
    (hooks:run-hook (hooks:object-hook *interface* 'buffer-before-make-hook) buffer)
    (unless (str:emptyp (namestring (cookies-path buffer)))
      (ensure-parent-exists (cookies-path buffer)))
    (setf (gethash (id buffer) (buffers *interface*)) buffer)
    (incf (total-buffer-count *interface*))
    (%rpc-send "buffer_make" (id buffer)
               `(("cookies-path" ,(namestring (cookies-path buffer)))))
    (unless (last-active-buffer *interface*)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer *interface*) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (hooks:run-hook (hooks:object-hook *interface* 'buffer-make-hook) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after it has been created on the platform port.
    (initialize-modes buffer)
    buffer))

(defun %get-inactive-buffer ()
  "Return inactive buffer or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer
                  (alexandria:hash-table-values (windows *interface*))))
        (buffers (alexandria:hash-table-values (buffers *interface*))))
    (match (set-difference buffers active-buffers)
      ((guard diff diff)
       (alexandria:last-elt diff)))))

(declaim (ftype (function (buffer)) rpc-buffer-delete))
@export
(defun rpc-buffer-delete (buffer)
  "Delete BUFFER from `*interface*'.
Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (hooks:run-hook (hooks:object-hook buffer 'buffer-delete-hook) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer)
                                (rpc-buffer-make))))
    (%rpc-send "buffer_delete" (id buffer))
    (when parent-window
      (window-set-active-buffer parent-window replacement-buffer))
    (remhash (id buffer) (buffers *interface*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *interface*)
      ((guard f f) (funcall f)))))

(declaim (ftype (function (buffer string)) rpc-buffer-load))
@export
(defun rpc-buffer-load (buffer uri)
  (%rpc-send "buffer_load" (id buffer) uri))

(declaim (ftype (function (buffer string &key (:callback function))) rpc-buffer-evaluate-javascript))
@export
(defun rpc-buffer-evaluate-javascript (buffer javascript &key callback)
  (let ((callback-id
          (%rpc-send "buffer_evaluate_javascript" (id buffer) javascript)))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

(declaim (ftype (function (window string &key (:callback function))) rpc-minibuffer-evaluate-javascript))
@export
(defun rpc-minibuffer-evaluate-javascript (window javascript &key callback)
  ;; JS example: document.body.innerHTML = 'hello'
  (let ((callback-id
          (%rpc-send "minibuffer_evaluate_javascript" (id window) javascript)))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

(declaim (ftype (function (window key-chord)) rpc-generate-input-event))
@export
(defun rpc-generate-input-event (window event)
  "For now, we only generate keyboard events.
In the future, we could also support other input device events such as mouse
events."
  (log:debug "Generate input ~a for window ~a"
             (list
              (key-chord-key-code event)
              (key-chord-modifiers event)
              (key-chord-low-level-data event)
              (key-chord-position event))
             (id window))
  (%rpc-send "generate_input_event"
             (id window)
             (key-chord-key-code event)
             (or (key-chord-modifiers event) (list ""))
             (key-chord-low-level-data event)
             (float (or (first (key-chord-position event)) -1.0))
             (float (or (second (key-chord-position event)) -1.0))))

(declaim (ftype (function (buffer &optional string list)) rpc-set-proxy))
@export
(defun rpc-set-proxy (buffer &optional (proxy-uri "") (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
For the user-level interface, see `proxy-mode'.

Note: WebKit supports three proxy \"modes\": default (the system proxy),
custom (the specified proxy) and none.
TODO: We don't use \"none\" here, but it could be useful to expose it to the
user."
  (%rpc-send "set_proxy" (list (id buffer))
             (if (string= proxy-uri "")
                 "default"
                 "custom")
             proxy-uri ignore-hosts))

(declaim (ftype (function (buffer)) rpc-get-proxy))
@export
(defun rpc-get-proxy (buffer)
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy configuration.
MODE is one of \"default\" (use system configuration), \"custom\" or \"none\".
ADDRESS is in the form PROTOCOL://HOST:PORT."
  (%rpc-send "get_proxy" (id buffer)))

(declaim (ftype (function (buffer string boolean)) rpc-buffer-set))
@export
(defun rpc-buffer-set (buffer setting value)
  "Set SETTING to VALUE for BUFFER.
The valid SETTINGs are specified by the platform, e.g. for WebKitGTK it is
https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html.

TODO: Only booleans are supported for now."
  (%rpc-send "buffer_set" (id buffer) setting value))


;; Expose Lisp Core RPC endpoints.

(dbus:define-dbus-object core-object
  (:path +core-object-path+))

(dbus:define-dbus-method (core-object buffer-javascript-call-back)
    ((buffer-id :string) (javascript-response :string) (callback-id :string))
    ()
  (:interface +core-interface+)
  (:name "buffer_javascript_call_back")
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    ;; Buffer might not exist, e.g. if it has been deleted in the mean time.
    (when buffer
      (let ((callback (gethash callback-id (callbacks buffer))))
        (when callback
          (funcall callback javascript-response)))))
  (values))

(dbus:define-dbus-method (core-object minibuffer-javascript-call-back)
    ((window-id :string) (javascript-response :string) (callback-id :string))
    ()
  (:interface +core-interface+)
  (:name "minibuffer_javascript_call_back")
  (let ((window (gethash window-id (windows *interface*))))
    ;; Window might not exist, e.g. if it has been deleted in the mean time.
    (when window
      (let ((callback (gethash callback-id (minibuffer-callbacks window))))
        (when callback
          (funcall callback javascript-response)))))
  (values))

(dbus:define-dbus-method (core-object buffer-did-commit-navigation)
    ((buffer-id :string) (url :string))
    ()
  (:interface +core-interface+)
  (:name "buffer_did_commit_navigation")
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-commit-navigation buffer url))
  (values))

(dbus:define-dbus-method (core-object buffer-did-finish-navigation)
    ((buffer-id :string) (url :string))
    ()
  (:interface +core-interface+)
  (:name "buffer_did_finish_navigation")
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-finish-navigation buffer url))
  (values))

(dbus:define-dbus-method (core-object buffer-uri-at-point)
    ((url :string))
    ()
  (:interface +core-interface+)
  (:name "buffer_uri_at_point")
  (if (str:emptyp url)
      (echo-dismiss)
      (echo "→ ~a" url))
  (values))

(dbus:define-dbus-method (core-object window-will-close)
    ((window-id :string))
    ()
  (:interface +core-interface+)
  (:name "window_will_close")
  (let* ((windows (windows *interface*))
         (window (gethash window-id windows)))
    (log:debug "Closing window ID ~a (new total: ~a)" window-id
               (1- (hash-table-count windows)))
    (hooks:run-hook (hooks:object-hook window 'window-delete-hook)
                    window)
    (remhash window-id windows))
  (values))

(dbus:define-dbus-method (core-object make-buffers)
    ((urls (:array :string)))
    ()
  (:interface +core-interface+)
  (:name "make_buffers")
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
              (let ((window (rpc-window-make)))
                (window-set-active-buffer window first-buffer))
              (set-current-buffer first-buffer))))
    (error (c)
      ;; Forward error or else external caller of "next foo.url" won't know
      ;; there was an error.
      (error "Could not make buffer to open ~a: ~a~%Is the platform port running?" urls c))))

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
(dbus:define-dbus-method (core-object request-resource)
    ((buffer-id :string) (url :string)
     (cookies :string)
     (event-type :string) (is-new-window :boolean)
     (is-known-type :boolean) (mouse-button :string) (modifiers (:array :string)))
    (:boolean)
  (:interface +core-interface+)
  (:name "request_resource")
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
  (match (rpc-window-active)
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
    (let ((rpc-window-active (rpc-window-active)))
      (if rpc-window-active
          (window-set-active-buffer rpc-window-active buffer)
          (make-window buffer))
      buffer)))

@export
(defun current-minibuffer ()
  "Return the currently active minibuffer."
  (first (active-minibuffers (last-active-window *interface*))))
