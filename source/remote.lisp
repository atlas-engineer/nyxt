;;; remote.lisp --- remote gui interface

;; We prefix all functions communicating over RPC with "%%".

(in-package :next)

(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (minibuffer-active :accessor minibuffer-active :initform nil)
   (minibuffer-callbacks :accessor minibuffer-callbacks
                         :initform (make-hash-table :test #'equal))
   (minibuffer-closed-height :accessor minibuffer-closed-height :initform 0
                             :documentation "The height of the minibuffer when closed.")
   (minibuffer-open-height :accessor minibuffer-open-height :initform 200
                           :documentation "The height of the minibuffer when open.")
   (minibuffer-echo-height :accessor minibuffer-echo-height :initform 25
                           :documentation "The height of the minibuffer when echoing.")
   (history-db-path :accessor history-db-path :initform (xdg-data-home "history.db")
                    :documentation "The path where the system will create/save the history database.")
   (bookmark-db-path :accessor bookmark-db-path :initform (xdg-data-home "bookmark.db")
                     :documentation "The path where the system will create/save the bookmark database.")
   (search-engines :accessor search-engines :initform '(("default" . "https://duckduckgo.com/?q=~a")
                                                        ("wiki" . "https://en.wikipedia.org/w/index.php?search=~a"))
                   :documentation "An association list of all the search engines you can use in the minibuffer.
The 'default' engine is used when the query is not a valid URL, or the first
keyword is not recognized.")))

(defclass buffer ()
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name)
   (modes :accessor modes :initarg :modes :initform '()
          :documentation "The list of mode instances.")
   (default-modes :accessor default-modes :initarg :default-modes
                  :initform '(document-mode root-mode)
                  :documentation "The list of symbols of class to
instantiate on buffer creation, unless specified.")
   (current-keymap-scheme :accessor current-keymap-scheme ; TODO: Name keymap-scheme instead?
                          :initarg :current-keymap-scheme
                          :initform :emacs
                          :documentation "The keymap scheme that will be used
for all modes in the current buffer.")
   (override-map :accessor override-map
                 :initarg :override-map
                 :initform (let ((map (make-keymap)))
                             (define-key :keymap map
                               "M-x" 'execute-command)
                             map)
                 :documentation "This keymap is always looked up first, it
overrides all other bindings.  No libraries should ever touch the override-map,
this is left for the user to customize to their needs.")
   (forward-input-events :accessor forward-input-events :initarg :forward-input-events
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
   (view :accessor view :initarg :view)
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
platform ports might support this.")))

(defmethod initialize-instance :after ((buffer buffer) &key)
  (let ((root-mode (make-instance 'root-mode :buffer buffer)))
    (dolist (mode-class (reverse (default-modes buffer)))
      ;; ":activate t" should not be necessary here since (modes buffer) should be
      ;; empty.
      ;; For now, root-mode does not have an associated command.
      (if (eq mode-class 'root-mode)
          (push root-mode (modes buffer))
          (funcall mode-class root-mode :buffer buffer :activate t)))))

;; A struct used to describe a key-chord
(defstruct key-chord
  key-code
  key-string
  modifiers
  position
  low-level-data)

(defmethod did-commit-navigation ((buffer buffer) url)
  (setf (name buffer) url)
  (dolist (mode (modes buffer))
    (did-commit-navigation mode url)))

(defmethod did-finish-navigation ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (did-finish-navigation mode url)))

(defclass remote-interface ()
  ((port :accessor port :initform (make-instance 'port)
         :documentation "The CLOS object responible for handling the platform port.")
   (platform-port-poll-interval :accessor platform-port-poll-interval :initform 0.015
                                :documentation "The speed at which to poll the
RPC endpoint of a platform-port to see if it is ready to begin accepting RPC
commands.")
   (active-connection :accessor active-connection :initform nil)
   (minibuffer :accessor minibuffer :initform (make-instance 'minibuffer)
               :documentation "The minibuffer object.")
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (total-window-count :accessor total-window-count :initform 0)
   (last-active-window :accessor last-active-window :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count :initform 0)
   (start-page-url :accessor start-page-url :initform "https://next.atlas.engineer/quickstart"
                   :documentation "The URL of the first buffer opened by Next when started.")
   (key-chord-stack :accessor key-chord-stack :initform '()
                    :documentation "A stack that keeps track of the key chords a user has inputted.")
   (downloads :accessor downloads :initform '()
              :documentation "List of downloads.")
   (download-watcher :accessor download-watcher :initform nil
                     :documentation "List of downloads.")
   (download-directory :accessor download-directory :initform nil
                     :documentation "Path of directory where downloads will be
stored.  Nil means use system default.")))

(defun download-watch ()
  ;; TODO: Add a (sleep ...)?  If we have many downloads, this loop could result
  ;; in too high a frequency of refreshes.
  (loop while (lparallel:receive-result download-manager:*notifications*)
        do (let ((buffer (find-buffer 'download-mode)))
             ;; Only update if buffer exists.  We update even when out of focus
             ;; because if we switch to the buffer after all downloads are
             ;; completed, we won't receive notifications so the content needs
             ;; to be updated already.
             ;; TODO: Disable when out of focus?  Maybe need hook for that.
             (when buffer
               (download-refresh)))))

(defmethod download-add ((interface remote-interface) (download download-manager:download))
  (unless (download-watcher interface)
    (setf (download-watcher interface) (bt:make-thread #'download-watch)))
  (unless (find-buffer 'download-mode)
    (download-list (make-instance 'root-mode)))
  (push download (downloads interface)))

(defmethod initialize-instance :after ((interface remote-interface)
                                       &key &allow-other-keys)
  "Start the RPC server."
  (let ((lock (bt:make-lock))
        (condition (bt:make-condition-variable)))
    (setf (active-connection interface)
          (bt:make-thread
           (lambda ()
             (dbus:with-open-bus (bus (dbus:session-server-addresses))
               (let ((status (dbus:request-name bus +core-name+ :do-not-queue)))
                 (when (eq status :exists)
                   (let ((url-list (or *free-args*
                                       (list (get-default 'buffer 'default-new-buffer-url)))))
                     (log:info  "Next already started, requesting to open URL(s) ~a." url-list)
                     (%rpc-send-self "make_buffers" "as" url-list)
                     (uiop:quit))))
               (log:info "Bus connection name: ~A" (dbus:bus-name bus))
               (bt:condition-notify condition)
               (dbus:publish-objects bus)))))
    (bt:acquire-lock lock)
    (bt:condition-wait condition lock)))

(defmethod kill-interface ((interface remote-interface))
  "Stop the RPC server."
  (when (active-connection interface)
    (log:debug "Stopping server")
    ;; TODO: How do we close the connection?
    (ignore-errors (bt:destroy-thread (active-connection interface)))))

(defun %rpc-send-self (method-name signature &rest args)
  "Call METHOD over ARGS.
SIGNATURE must be the D-Bus encoded type string of ARGS.
For an array of string, that would be \"as\"."
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:invoke-method (dbus:bus-connection bus) method-name
                        :path +core-object-path+
                        :destination +core-name+
                        :interface +core-interface+
                        :signature signature
                        :arguments args)))

(defmethod %rpc-send ((interface remote-interface) (method string) &rest args)
  ;; TODO: Make %rpc-send asynchronous?
  ;; If the platform port ever hangs, the next %rpc-send will hang the Lisp core too.
  ;; Always ensure we send the auth token as the first argument
  ;; TODO: Catch connection errors and execution errors.
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object (platform-port bus +platform-port-object-path+ +platform-port-name+)
      (apply #'platform-port +platform-port-interface+ method args))))

;; TODO: Move to separate packages:
;; - next-rpc
;; - next-script (?)
(defmethod %%list-methods ((interface remote-interface))
  "Return the unsorted list of RPC methods supported by the platform port."
  ;; TODO: Find the right way to do this in dbus.
  (%rpc-send interface "listMethods"))

(defmethod get-unique-window-identifier ((interface remote-interface))
  (incf (total-window-count interface))
  (format nil "~a" (total-window-count interface)))

(defmethod get-unique-buffer-identifier ((interface remote-interface))
  (incf (total-buffer-count interface))
  (format nil "~a" (total-buffer-count interface)))

(defmethod %%window-make ((interface remote-interface))
  "Create a window and return the window object."
  (let* ((window-id (get-unique-window-identifier interface))
         (window (make-instance 'window :id window-id)))
    (setf (gethash window-id (windows interface)) window)
    (%rpc-send interface "window_make" window-id)
    (unless (last-active-window interface)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and %%window-active would then return nil.
      (setf (last-active-window interface) window))
    window))

(defmethod %%window-set-title ((interface remote-interface) (window window) title)
  "Set the title for a given window."
  (%rpc-send interface "window_set_title" (id window) title))

(defmethod %%window-delete ((interface remote-interface) (window window))
  "Delete a window object and remove it from the hash of windows."
  (%rpc-send interface "window_delete" (id window))
  (with-slots (windows) interface
    (remhash (id window) windows)))

(defmethod %%window-active ((interface remote-interface))
  "Return the window object for the currently active window."
  (with-slots (windows) interface
    (let ((window (gethash (%rpc-send interface "window_active")
                           windows)))
      (when window
        (setf (last-active-window interface) window))
      (last-active-window interface))))

(defmethod %%window-exists ((interface remote-interface) (window window))
  "Return if a window exists."
  (%rpc-send interface "window_exists" (id window)))

(defmethod %%window-set-active-buffer ((interface remote-interface)
                                      (window window)
                                      (buffer buffer))
  (%rpc-send interface "window_set_active_buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer))

(defmethod window-set-active-buffer ((interface remote-interface)
                                     (window window)
                                     (buffer buffer))
  ;; TODO: Replace this swapping business with a simple swap + a "refresh rendering" RPC call?
  (let ((window-with-same-buffer (find-if
                                  (lambda (other-window) (and (not (eq other-window window))
                                                              (eql (active-buffer other-window) buffer)))
                                  (alexandria:hash-table-values (windows *interface*)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (%%buffer-make *interface*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (%%window-set-active-buffer interface window-with-same-buffer temp-buffer)
          (%%window-set-active-buffer interface window buffer)
          (%%window-set-active-buffer interface window-with-same-buffer buffer-swap)
          (%%buffer-delete interface temp-buffer))
        (%%window-set-active-buffer interface window buffer))))

(defmethod %%window-set-minibuffer-height ((interface remote-interface)
                                         window height)
  (%rpc-send interface "window_set_minibuffer_height" (id window) height))

(defmethod %%buffer-make ((interface remote-interface)
                          &key name default-modes)
  (let* ((buffer-id (get-unique-buffer-identifier interface))
         (buffer (apply #'make-instance 'buffer :id buffer-id
                        (append (when name `(:name ,name))
                                (when default-modes `(:default-modes ,default-modes))))))
    (ensure-parent-exists (cookies-path buffer))
    (setf (gethash buffer-id (buffers interface)) buffer)
    (%rpc-send interface "buffer_make" buffer-id
               `(("cookies-path" ,(namestring (cookies-path buffer)))))
    buffer))

(defmethod %get-inactive-buffer ((interface remote-interface))
  (let ((active-buffers
          (mapcar #'active-buffer
                      (alexandria:hash-table-values (windows *interface*))))
        (buffers (alexandria:hash-table-values (buffers *interface*))))
    (first (set-difference buffers active-buffers))))

(defmethod %%buffer-delete ((interface remote-interface) (buffer buffer))
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer interface)
                                (%%buffer-make interface))))
    (%rpc-send interface "buffer_delete" (id buffer))
    (when parent-window
      (window-set-active-buffer interface parent-window replacement-buffer))
    (with-slots (buffers) interface
      (remhash (id buffer) buffers))))

(defmethod %%buffer-load ((interface remote-interface) (buffer buffer) uri)
  (%rpc-send interface "buffer_load" (id buffer) uri))

(defmethod %%buffer-evaluate-javascript ((interface remote-interface)
                                         (buffer buffer) javascript
                                         &optional (callback nil))
  (let ((callback-id
          (%rpc-send interface "buffer_evaluate_javascript" (id buffer) javascript)))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

(defmethod %%minibuffer-evaluate-javascript ((interface remote-interface)
                                             (window window) javascript
                                             &optional (callback nil))
  (let ((callback-id
          (%rpc-send interface "minibuffer_evaluate_javascript" (id window) javascript)))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

(defmethod %%generate-input-event ((interface remote-interface)
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

(defmethod %%set-proxy ((interface remote-interface) (buffer buffer)
                      &optional (proxy-uri "") (ignore-hosts (list nil)))
  (%rpc-send interface "set_proxy" (list (id buffer))
                 (if (string= proxy-uri "")
                     "default"
                     "custom")
                 proxy-uri ignore-hosts))

(defmethod %%get-proxy ((interface remote-interface) (buffer buffer))
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy configuration.
MODE is one of \"default\" (use system configuration), \"custom\" or \"none\".
ADDRESS is in the form PROTOCOL://HOST:PORT."
  (%rpc-send interface "get_proxy" (id buffer)))

(defmethod %%buffer-set ((interface remote-interface) (buffer buffer)
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
  (let* ((window (gethash window-id (windows *interface*)))
         (callback (gethash callback-id (minibuffer-callbacks window))))
    (when callback
      (funcall callback javascript-response)))
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
    (let ((buffer (make-buffer))
          (window (%%window-make *interface*)))
      (set-url-buffer (first urls) buffer)
      (window-set-active-buffer *interface* window buffer))
    (loop for url in (rest urls) do
      (let ((buffer (make-buffer)))
        (set-url-buffer url buffer)))))

(defmethod resource-query-default ((buffer buffer)
                                   &key url
                                     (cookies "")
                                     event-type
                                     (is-new-window nil)
                                     (is-known-type t)
                                     (mouse-button "")
                                     (modifiers '()))
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
              (string= mouse-button "button1")))
     (log:info "Load ~a in new buffer" url)
     (make-buffers (list url))
     nil)
    ((not is-known-type)
     (log:info "Buffer ~a downloads ~a" buffer url)
     (let* ((mode (find-mode buffer 'proxy-mode))
            (proxied-downloads (and mode (proxied-downloads-p mode)))
            (download nil))
       (handler-case
           (setf download (download-manager:resolve
                           url
                           :directory (download-directory *interface*)
                           :cookies cookies
                           :proxy (and proxied-downloads
                                       (server-address mode))))
         (error (c)
           (echo "Download error: ~a" c)))
       (download-add *interface* download))
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
    (funcall (resource-query-function buffer)
             buffer
             :url url
             :cookies cookies
             :event-type event-type
             :is-new-window is-new-window
             :is-known-type is-known-type
             :mouse-button mouse-button
             :modifiers modifiers)))


;; Convenience methods and functions for users of the API.

(defmethod active-buffer ((interface remote-interface))
  "Get the active buffer for the active window."
  (active-buffer (%%window-active interface)))

;; TODO: Prevent setting the minibuffer as the active buffer.
(defmethod set-active-buffer ((interface remote-interface)
                              (buffer buffer))
  "Set the active buffer for the active window."
  (let ((%%window-active (%%window-active interface)))
    (window-set-active-buffer interface %%window-active buffer)
    (setf (active-buffer %%window-active) buffer)))
