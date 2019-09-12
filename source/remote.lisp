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
   (status-buffer-height :accessor status-buffer-height :initform 25
                         :documentation "The height of the status buffer.")
   (minibuffer-callbacks :accessor minibuffer-callbacks
                         :initform (make-hash-table :test #'equal))
   (minibuffer-closed-height :accessor minibuffer-closed-height :initform 25
                             ;; TODO: Until we have a mode-line, it's best to
                             ;; keep the closed-height equal to the echo-height
                             ;; to avoid the stuttering, especially when
                             ;; hovering over links.
                             :documentation "The height of the minibuffer when closed.")
   (minibuffer-open-height :accessor minibuffer-open-height :initform 200
                           :documentation "The height of the minibuffer when open.")
   (history-db-path :accessor history-db-path :initform (xdg-data-home "history.db")
                    :documentation "The path where the system will create/save the history database.")
   (bookmark-db-path :accessor bookmark-db-path :initform (xdg-data-home "bookmark.db")
                     :documentation "The path where the system will create/save the bookmark database.")
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
into `session-path'.")
   (search-engines :accessor search-engines :initform '(("default" . "https://duckduckgo.com/?q=~a")
                                                        ("wiki" . "https://en.wikipedia.org/w/index.php?search=~a"))
                   :documentation "An association list of all the search engines you can use in the minibuffer.
The 'default' engine is used when the query is not a valid URL, or the first
keyword is not recognized.")
   (window-set-active-buffer-hook :accessor window-set-active-buffer-hook :initform '() :type list
                                  :documentation "Hook run before `rpc-window-set-active-buffer' takes effect.
The handlers take the window and the buffer as argument.")
   (window-delete-hook :accessor window-delete-hook :initform '() :type list
                       :documentation "Hook run before `rpc-window-delete' takes effect.
The handlers take the window as argument.")))

;; TODO: Move session to `remote-interface'?
(defun store-sexp-session ()
  "Store the current Next session to the last window's `session-path'.
Currently we store the list of current URLs of all buffers."
  (with-open-file (file (session-path (last-active-window *interface*))
                        :direction :output
                        :if-exists :overwrite)
    (s-serialization:serialize-sexp
     (mapcar #'name (alexandria:hash-table-values (buffers *interface*)))
     file)))

(defun restore-sexp-session ()
  "Store the current Next session to the last window's `session-path'.
Currently we store the list of current URLs of all buffers."
  (let ((url-list
         (with-open-file (file (session-path (last-active-window *interface*))
                               :direction :input
                               :if-does-not-exist nil)
           (when file
             (s-serialization:deserialize-sexp
              file)))))
    (when url-list
      (make-buffers
       ;; TODO: Find a better way to clean up special buffers.  Or should we
       ;; restore them?
       (delete-if (complement (alexandria:curry #'str:starts-with? "*"))
                  url-list)))))

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
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name)
   (title :accessor title :initarg :title :initform nil)
   (modes :accessor modes :initarg :modes :initform '()
          :documentation "The list of mode instances.")
   (default-modes :accessor default-modes :initarg :default-modes
                  :initform '(document-mode root-mode)
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
   (cookies-path :accessor cookies-path :initform (xdg-data-home "cookies.txt")
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
                           :border-radius "3px"))
              :documentation "The style of the boxes, e.g. link hints.")
   (proxy :initform nil :type :proxy
          :documentation "Proxy for buffer.")
   ;; TODO: Rename `load-hook' to `set-url-hook'?
   (load-hook :accessor load-hook :initform '() :type list
              :documentation "Hook run in `set-url' after `parse-url' was processed.")
   (buffer-delete-hook :accessor buffer-delete-hook :initform '() :type list
                       :documentation "Hook run before `rpc-buffer-delete' takes effect.
The handlers take the buffer as argument.")))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (rpc-set-proxy *interface* buffer
                     (server-address proxy)
                     (whitelist proxy))
      (rpc-set-proxy *interface* buffer
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
  (setf (name buffer) url)
  (with-result (title (buffer-get-title))
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
   (dbus-pid :accessor dbus-pid :initform nil :type :number
             :documentation "The process identifier of the dbus instance started
by Next when the user session dbus instance is not available.")
   (clipboard-ring :accessor clipboard-ring :initform (ring:make))
   (minibuffer-generic-history :accessor minibuffer-generic-history :initform (ring:make))
   (minibuffer-search-history :accessor minibuffer-search-history :initform (ring:make))
   (minibuffer-set-url-history :accessor minibuffer-set-url-history :initform (ring:make))
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (total-window-count :accessor total-window-count :initform 0)
   (last-active-window :accessor last-active-window :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count :initform 0)
   (startup-function :accessor startup-function
                     :type function
                     :initform #'default-startup
                     :documentation "The function run on startup.  It takes a
list of URLs (strings) as argument (the command line positional arguments).  It
is run after the platform port has been initialized and after the
`after-init-hook' has run.")
   (start-page-url :accessor start-page-url :initform "https://next.atlas.engineer/quickstart"
                   :documentation "The URL of the first buffer opened by Next when started.")
   (open-external-link-in-new-window-p :accessor open-external-link-in-new-window-p :initform nil
                                       :documentation "When open links from an external program, or
when C-cliking on a URL, decide whether to open in a new
window or not.")
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
   (after-init-hook :accessor after-init-hook :initform '() :type list
                    :documentation "Hook run after both `*interface*' and the
platform port have started.  The handlers take no argument.")
   (before-exit-hook :accessor before-exit-hook :initform '() :type list
                     :documentation "Hook run before both `*interface*' and the
platform port get terminated.  The handlers take no argument.")
   (window-make-hook :accessor window-make-hook :initform '() :type list
                     :documentation "Hook run after `rpc-window-make'.
The handlers take the window as argument.")
   (buffer-make-hook :accessor buffer-make-hook :initform '() :type list
                     :documentation "Hook run after `rpc-buffer-make'.
The handlers take the buffer as argument.")
   (before-download-hook :accessor buffer-download-hook :initform '() :type list
                         :documentation "Hook run before a downloading a URL.
The handlers take the URL as argument.")
   (after-download-hook :accessor after-download-hook :initform '() :type list
                        :documentation "Hook run after a download has completed.
The handlers take the `download-manager:download' class instance as argument.")))

@export
(defmethod minibuffer ((interface remote-interface))
  "Currently active minibuffer"
  (first (active-minibuffers (last-active-window interface))))

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
(defmethod download ((interface remote-interface) url &key
                                                        cookies
                                                        (proxy-address :auto))
  "Download URI.
When PROXY-ADDRESS is :AUTO (the default), the proxy address is guessed from the
current buffer."
  (hooks:run-hook (hooks:object-hook *interface* 'before-download-hook) url)
  (when (eq proxy-address :auto)
    (setf proxy-address (proxy-address (active-buffer interface)
                                       :downloads-only t)))
  (let* ((download nil))
    (handler-case
        (progn
          (setf download (download-manager:resolve
                          url
                          :directory (download-directory interface)
                          :cookies cookies
                          :proxy proxy-address))
          (push download (downloads interface))
          download)
      (error (c)
        (echo "Download error: ~a" c)
        nil))))

(defun ensure-dbus-session (interface)
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
                                (apply #'run-program-to-string +dbus-launch-command+)))
        ((list (list _ address) (list _ pid))
         (log:info "D-Bus session inaccessible, starting our own one.~%  Old D-Bus addresses: ~a~%  New D-Bus address: ~a"
                   (list (uiop:getenv "DBUS_SESSION_BUS_ADDRESS")
                         (uiop:getenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET"))
                   address)
         (setf (uiop:getenv "DBUS_SESSION_BUS_ADDRESS") address)
         (setf (uiop:getenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET") address)
         (setf (dbus-pid interface) (parse-integer pid)))))))

(defmethod initialize-instance :after ((interface remote-interface)
                                       &key &allow-other-keys)
  "Start the RPC server."
  (ensure-dbus-session interface)
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
                     (uiop:quit))))
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

(defmethod %rpc-send ((interface remote-interface) (method string) &rest args)
  ;; TODO: Make %rpc-send asynchronous?
  ;; If the platform port ever hangs, the next %rpc-send will hang the Lisp core too.
  ;; TODO: Catch connection errors and execution errors.
  (dbus:with-open-bus (bus (session-server-addresses))
    (dbus:with-introspected-object (platform-port bus +platform-port-object-path+ +platform-port-name+)
      (apply #'platform-port +platform-port-interface+ method args))))

;; TODO: Move to separate packages:
;; - next-rpc
;; - next-script (?)
(defmethod rpc-list-methods ((interface remote-interface))
  "Return the unsorted list of RPC methods supported by the platform port."
  ;; TODO: Find the right way to do this in dbus.
  (%rpc-send interface "listMethods"))

(defmethod get-unique-window-identifier ((interface remote-interface))
  (format nil "~a" (1+ (total-window-count interface))))

(defmethod get-unique-buffer-identifier ((interface remote-interface))
  (format nil "~a" (1+ (total-buffer-count interface))))

@export
(defmethod rpc-window-make ((interface remote-interface))
  "Create a window and return the window object.
Run INTERFACE's `window-make-hook' over the created window."
  (let* ((window-id (get-unique-window-identifier interface))
         (window (make-instance 'window :id window-id)))
    (setf (gethash window-id (windows interface)) window)
    (incf (total-window-count interface))
    (%rpc-send interface "window_make" window-id)
    (unless (last-active-window interface)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and rpc-window-active would then return nil.
      (setf (last-active-window interface) window))
    (hooks:run-hook (hooks:object-hook interface 'window-make-hook) window)
    window))

@export
(defmethod rpc-window-set-title ((interface remote-interface) (window window) title)
  "Set the title for a given window."
  (%rpc-send interface "window_set_title" (id window) title))

@export
(defmethod rpc-window-delete ((interface remote-interface) (window window))
  "Delete a window object and remove it from the hash of windows.
Run INTERFACE's `window-delete-hook' over WINDOW before deleting it."
  (hooks:run-hook (hooks:object-hook window 'window-delete-hook) window)
  (%rpc-send interface "window_delete" (id window))
  (with-slots (windows) interface
    (remhash (id window) windows)))

@export
(defmethod rpc-window-active ((interface remote-interface))
  "Return the window object for the currently active window."
  (with-slots (windows) interface
    (let ((window (gethash (%rpc-send interface "window_active")
                           windows)))
      (when window
        (setf (last-active-window interface) window))
      (last-active-window interface))))

@export
(defmethod rpc-window-exists ((interface remote-interface) (window window))
  "Return if a window exists."
  (%rpc-send interface "window_exists" (id window)))

@export
(defmethod rpc-window-set-active-buffer ((interface remote-interface)
                                         (window window)
                                         (buffer buffer))
  "Set INTERFACE's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (hooks:object-hook window 'window-set-active-buffer-hook) window buffer)
  (%rpc-send interface "window_set_active_buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer))

@export
(defmethod set-window-title ((interface remote-interface)
                             (window window)
                             (buffer buffer))
  "Set current window title to 'Next - TITLE - URL."
  (let ((url (name buffer)))
    (with-result* ((title (buffer-get-title)))
      (setf title (if (str:emptyp title) "" title))
      (setf url (if (str:emptyp url) "<no url/name>" url))
      (rpc-window-set-title interface window
                            (concatenate 'string "Next - "
                                         title (unless (str:emptyp title) " - ")
                                         url)))))

@export
(defmethod window-set-active-buffer ((interface remote-interface)
                                     (window window)
                                     (buffer buffer))
  ;; TODO: Replace this swapping business with a simple swap + a "refresh rendering" RPC call?
  (let ((window-with-same-buffer (find-if
                                  (lambda (other-window) (and (not (eq other-window window))
                                                              (eql (active-buffer other-window) buffer)))
                                  (alexandria:hash-table-values (windows *interface*)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (rpc-buffer-make *interface*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (rpc-window-set-active-buffer interface window-with-same-buffer temp-buffer)
          (rpc-window-set-active-buffer interface window buffer)
          (rpc-window-set-active-buffer interface window-with-same-buffer buffer-swap)
          (rpc-buffer-delete interface temp-buffer))
        (rpc-window-set-active-buffer interface window buffer))
    (set-window-title interface window buffer)
    (setf (active-buffer window) buffer)))

@export
(defmethod rpc-window-set-minibuffer-height ((interface remote-interface)
                                             window height)
  (%rpc-send interface "window_set_minibuffer_height" (id window) height))

@export
(defmethod rpc-buffer-make ((interface remote-interface)
                            &key name default-modes)
  "Make buffer with name NAME and modes DEFAULT-MODES.
Run INTERFACE's `buffer-make-hook' over the created buffer before returning it."
  (let* ((buffer-id (get-unique-buffer-identifier interface))
         (buffer (apply #'make-instance 'buffer :id buffer-id
                        (append (when name `(:name ,name))
                                (when default-modes `(:default-modes ,default-modes))))))
    (ensure-parent-exists (cookies-path buffer))
    (setf (gethash buffer-id (buffers interface)) buffer)
    (incf (total-buffer-count interface))
    (%rpc-send interface "buffer_make" buffer-id
               `(("cookies-path" ,(namestring (cookies-path buffer)))))
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after it has been created on the platform port.
    (initialize-modes buffer)
    (hooks:run-hook (hooks:object-hook interface 'buffer-make-hook) buffer)
    buffer))

(defmethod %get-inactive-buffer ((interface remote-interface))
  (let ((active-buffers
          (mapcar #'active-buffer
                      (alexandria:hash-table-values (windows *interface*))))
        (buffers (alexandria:hash-table-values (buffers *interface*))))
    (alexandria:last-elt (set-difference buffers active-buffers))))

@export
(defmethod rpc-buffer-delete ((interface remote-interface) (buffer buffer))
  "Delete BUFFER from INTERFACE.
Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (hooks:run-hook (hooks:object-hook buffer 'buffer-delete-hook) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer interface)
                                (rpc-buffer-make interface))))
    (%rpc-send interface "buffer_delete" (id buffer))
    (when parent-window
      (window-set-active-buffer interface parent-window replacement-buffer))
    (with-slots (buffers) interface
      (remhash (id buffer) buffers))))

@export
(defmethod rpc-buffer-load ((interface remote-interface) (buffer buffer) uri)
  (%rpc-send interface "buffer_load" (id buffer) uri))

@export
(defmethod rpc-buffer-evaluate-javascript ((interface remote-interface)
                                           (buffer buffer) javascript
                                           &key callback)
  (let ((callback-id
          (%rpc-send interface "buffer_evaluate_javascript" (id buffer) javascript)))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

@export
(defmethod rpc-minibuffer-evaluate-javascript ((interface remote-interface)
                                               (window window) javascript
                                               &key callback)
  ;; JS example: document.body.innerHTML = 'hello'
  (let ((callback-id
          (%rpc-send interface "minibuffer_evaluate_javascript" (id window) javascript)))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

@export
(defmethod rpc-generate-input-event ((interface remote-interface)
                                   (window window)
                                   (event key-chord))
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
  (%rpc-send interface "generate_input_event"
                 (id window)
                 (key-chord-key-code event)
                 (or (key-chord-modifiers event) (list ""))
                 (key-chord-low-level-data event)
                 (float (or (first (key-chord-position event)) -1.0))
                 (float (or (second (key-chord-position event)) -1.0))))

@export
(defmethod rpc-set-proxy ((interface remote-interface) (buffer buffer)
                          &optional (proxy-uri "") (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
For the user-level interface, see `proxy-mode'.

Note: WebKit supports three proxy \"modes\": default (the system proxy),
custom (the specified proxy) and none.
TODO: We don't use \"none\" here, but it could be useful to expose it to the
user."
  (%rpc-send interface "set_proxy" (list (id buffer))
             (if (string= proxy-uri "")
                 "default"
                 "custom")
             proxy-uri ignore-hosts))

@export
(defmethod rpc-get-proxy ((interface remote-interface) (buffer buffer))
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy configuration.
MODE is one of \"default\" (use system configuration), \"custom\" or \"none\".
ADDRESS is in the form PROTOCOL://HOST:PORT."
  (%rpc-send interface "get_proxy" (id buffer)))

@export
(defmethod rpc-buffer-set ((interface remote-interface) (buffer buffer)
                       (setting string) value)
  "Set SETTING to VALUE for BUFFER.
The valid SETTINGs are specified by the platform, e.g. for WebKitGTK it is
https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html.

TODO: Only booleans are supported for now."
  (%rpc-send interface "buffer_set" (id buffer) setting value))


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
  (let ((windows (windows *interface*)))
    (log:debug "Closing window ID ~a (new total: ~a)" window-id
               (1- (length (alexandria:hash-table-values windows))))
    (remhash window-id windows))
  (values))

(dbus:define-dbus-method (core-object make-buffers)
    ((urls (:array :string)))
    ()
  (:interface +core-interface+)
  (:name "make_buffers")
  (make-buffers urls)
  (values))

(defun make-buffers (urls)
  "Create new buffers from URLs."
  ;; The new active buffer should be the first created buffer.
  (when urls
    (let ((buffer (make-buffer)))
      (set-url (first urls) :buffer buffer)
      (if (open-external-link-in-new-window-p *interface*)
          (window-set-active-buffer *interface* (rpc-window-make *interface*) buffer)
          (set-active-buffer *interface* buffer)))
    (loop for url in (rest urls) do
      (let ((buffer (make-buffer)))
        (set-url url :buffer buffer)))))

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
         (and (equal modifiers '("C"))
              (string= mouse-button "button1"))
         (string= mouse-button "button2"))
     (log:info "Load ~a in new buffer" url)
     (make-buffers (list url))
     nil)
    ((not is-known-type)
     (log:info "Buffer ~a downloads ~a" buffer url)
     (download *interface* url :proxy-address (proxy-address buffer :downloads-only t)
               :cookies cookies)
     (unless (find-buffer 'download-mode)
       (download-list))
     nil)
    (t
     (log:info "Forwarding ~a back to platform port" url)
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

;; TODO: `(active-buffer *interface*)' is too verbose considering how frequently
;; we use it.  Remove `window's `active-buffer' accessor and make this a defun
;; with optional argument.
@export
(defmethod active-buffer ((interface remote-interface))
  "Get the active buffer for the active window."
  (match (rpc-window-active interface)
    ((guard w w) (active-buffer w))))

;; TODO: Prevent setting the minibuffer as the active buffer.
@export
(defmethod set-active-buffer ((interface remote-interface)
                              (buffer buffer))
  "Set the active buffer for the active window."
  (let ((rpc-window-active (rpc-window-active interface)))
    (window-set-active-buffer interface rpc-window-active buffer)))
