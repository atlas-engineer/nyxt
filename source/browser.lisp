;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type prompt-buffer (function (prompt-buffer)))
(hooks:define-hook-type download (function (download)))
(hooks:define-hook-type resource (function (request-data) (or request-data null)))
(export-always '(hook-resource))

(define-class proxy ()
  ((url
    (quri:uri "socks5://127.0.0.1:9050")
    :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example: \"http://192.168.1.254:8080\".")
   (allowlist
    '("localhost" "localhost:8080")
    :type list-of-strings
    :documentation "A list of URIs not to forward to the proxy.")
   (proxied-downloads-p
    t
    :documentation "Non-nil if downloads should also use the proxy."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."))

(export-always 'combine-composed-hook-until-nil)
(defmethod combine-composed-hook-until-nil ((hook hooks:hook) &optional arg)
  "Return the result of the composition of the HOOK handlers on ARG, from
oldest to youngest.  Stop processing when a handler returns nil.
Without handler, return ARG.  This is an acceptable `combination' for
`hook'."
  (labels ((compose-handlers (handlers result)
             (if handlers
                 (let ((new-result (funcall (first handlers) result)))
                   (log:debug "Handler (~a ~a): ~a" (first handlers) result new-result)
                   (when new-result
                     (compose-handlers (rest handlers) new-result)))
                 result)))
    (compose-handlers (mapcar #'hooks:fn (hooks:handlers hook)) arg)))

(export-always 'renderer-browser)
(defclass renderer-browser ()
  ()
  (:metaclass interface-class))

(define-class browser (renderer-browser)
  ((remote-execution-p
    nil
    :type boolean
    :documentation "Whether code sent to the socket gets executed.  You must
understand the risks before enabling this: a privileged user with access to your
system can then take control of the browser and execute arbitrary code under
your user profile.")
   (exit-code
    0
    :type alex:non-negative-integer
    :export nil
    :accessor nil
    :documentation "The exit code return to the operating system.
0 means success.
Non-zero means failure.")
   (socket-thread
    nil
    :type t
    :documentation "Thread that listens on socket.
See `*socket-file*'.
This slot is mostly meant to clean up the thread if necessary.")
   (non-terminating-threads
    '()
    :type (or list (cons bt:thread *))
    :documentation "List of threads that don't terminate
and that ought to be killed when quitting.")
   (messages-content
    '()
    :export t
    :reader messages-content
    :documentation "A list of all echoed messages.
Most recent messages are first.")
   (clipboard-ring
    (make-ring)
    :export nil)
   (prompt-buffer-generic-history
    (make-ring)
    :documentation "The default history of all prompt buffer entries.
This history is used if no history is specified for a given prompt buffer.")
   (default-new-buffer-url (quri:uri (nyxt-url 'new))
                           :type url-designator
                           :documentation "The URL set to a new blank buffer opened by Nyxt.")
   (set-url-history
    (make-ring)
    :documentation "The history of all URLs set via set-url")
   (old-prompt-buffers
    '()
    :export nil
    :documentation "The list of old prompt buffers.
This can be used to resume former buffers.")
   (recent-buffers
    (make-ring :size 50)
    :export nil
    :documentation "A ring that keeps track of deleted buffers.")
   (windows
    (make-hash-table)
    :export nil
    :documentation "Table of all windows, indexed by their `id'.")
   (last-active-window
    nil
    :type (or window null)
    :export nil
    :documentation "Records the last active window.  This is
useful when no Nyxt window is focused and we still want `ffi-window-active' to
return something.
See `current-window' for the user-facing function.")
   (buffers
    :initform (make-hash-table)
    :documentation "Table of all live buffers, indexed by their `id'.
see `buffer-list', `buffers-get', `buffers-set' and `buffers-delete'.")
   (startup-error-reporter-function
    nil
    :type (or function null)
    :export nil
    :documentation "When supplied, upon startup, if there are errors, they will
be reported by this function.")
   (open-external-link-in-new-window-p
    nil
    :documentation "Whether to open links issued by an external program or
issued by Control+<button1> in a new window.")
   (downloads
    :documentation "List of downloads. Used for rendering by the download manager.")
   (startup-timestamp
    (local-time:now)
    :export nil
    :documentation "`local-time:timestamp' of when Nyxt was started.")
   (init-time
    0.0
    :export nil
    :documentation "Initialization time in seconds.")
   (ready-p
    nil
    :reader ready-p
    :documentation "If non-nil, the browser is ready for operation (make
buffers, load data files, open prompt buffer, etc).")
   (native-dialogs
    t
    :type boolean
    :documentation "Whether to use prompt-buffer-reliant script dialogs and file-chooser.
If nil, renderer-provided dialogs are used.")
   (theme
    (make-instance 'theme:theme)
    :type theme:theme
    :documentation "The theme to use for all the browser interface elements.")
   (history-file
    (make-instance 'history-file)
    :type history-file
    :documentation "History file to read from when restoring session.
See `restore-session-on-startup-p' to c ontrol this behaviour.
See also `history-file' in `context-buffer' for per-buffer history files.")
   (restore-session-on-startup-p
    t
    :type boolean
    :documentation "Whether to restore buffers from the previous session.
You can store and restore sessions manually to various files with
`store-history-by-name' and `restore-history-by-name'.")
   (default-cookie-policy :no-third-party
                          :type cookie-policy
                          :documentation "Cookie policy of new buffers.
Must be one of `:always' (accept all cookies), `:never' (reject all cookies),
`:no-third-party' (accept cookies for current website only).")
   ;; Hooks follow:
   (before-exit-hook
    (make-instance 'hooks:hook-void)
    :type hooks:hook-void
    :documentation "Hook run before both `*browser*' and the renderer get terminated.
The handlers take no argument.")
   (window-make-hook
    (make-instance 'hook-window)
    :type hook-window
    :documentation "Hook run after `window-make'.
The handlers take the window as argument.")
   (buffer-make-hook
    (make-instance 'hook-buffer)
    :type hook-buffer
    :documentation "Hook run after `buffer' initialization and before the URL is
loaded.
It is run before mode initialization so that the default mode list can still be
altered from the hooks.
The handlers take the buffer as argument.")
   (buffer-before-make-hook
    (make-instance 'hook-buffer)
    :type hook-buffer
    :documentation "Hook run at the beginning of `buffer' initialization.
The buffer web view is not allocated, so it's not possible to run arbitrary
parenscript from this hook.
See `buffer-make-hook' and `buffer-after-make-hook' for other hook options.
The handlers take the buffer as argument.")
   (buffer-after-make-hook
    (make-instance 'hook-buffer)
    :type hook-buffer
    :documentation "Hook run after `buffer' initialization and before the URL is
loaded.
It is run as the very last step of buffer initialization, when everything else is ready.
See also `buffer-make-hook' and `buffer-before-make-hook'.
The handlers take the buffer as argument.")
   (prompt-buffer-make-hook
    (make-instance 'hook-prompt-buffer)
    :type hook-prompt-buffer
    :documentation "Hook run after the `prompt-buffer' class is instantiated and
before initializing the `prompt-buffer' modes.
The handlers take the `prompt-buffer' as argument.")
   (prompt-buffer-ready-hook
    (make-instance 'hook-prompt-buffer)
    :type hook-prompt-buffer
    :documentation "Hook run while waiting for the prompt buffer to be available.
The handlers take the `prompt-buffer' as argument.")
   (before-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run before downloading a URL.
The handlers take the URL as argument.")
   (after-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run after a download has completed.
The handlers take the `download-manager:download' class instance as argument.")
   (external-editor-program
    (or (uiop:getenv "VISUAL")
        (uiop:getenv "EDITOR"))
    :type (or (cons string *) string null)
    :writer t
    :export t
    :documentation "The external editor to use for
editing files.  You can specify the full command line arguments with a list of
strings."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "The browser class defines the overall behaviour of Nyxt, in
the sense that it manages the display of buffers.  For instance, it abstracts
the renderer, and lays the foundations to track and manipulate buffers and
windows.

A typical Nyxt session encompasses a single instance of this class, but nothing
prevents otherwise.")
  (:metaclass user-class))

(defmethod theme ((ignored (eql nil)))
  "Fallback theme in case there `*browser*' is NIL."
  (declare (ignore ignored))
  (make-instance 'theme:theme))

(defmethod external-editor-program ((browser browser))
  (alex:ensure-list (slot-value browser 'external-editor-program)))

(defmethod get-containing-window-for-buffer ((buffer buffer) (browser browser))
  "Get the window containing a buffer."
  (find buffer (alex:hash-table-values (windows browser)) :key #'active-buffer))

(defmethod finalize ((browser browser) urls startup-timestamp)
  "Run `*after-init-hook*' then BROWSER's `startup'."
  ;; `messages-appender' requires `*browser*' to be initialized.
  (unless (find-if (sera:eqs 'messages-appender) (log4cl:all-appenders)
                   :key #'sera:class-name-of)
    (log4cl:add-appender log4cl:*root-logger* (make-instance 'messages-appender)))
  (ignore-errors
   (handler-bind ((error (lambda (c) (log:error "In *after-init-hook*: ~a" c))))
     (hooks:run-hook *after-init-hook*))) ; TODO: Run outside the main loop?
  ;; `startup' must be run _after_ this function returns;
  ;; `ffi-within-renderer-thread' runs its body on the renderer thread when it's
  ;; idle, so it should do the job.  It's not enough since the
  ;; `startup' may invoke the prompt buffer, which cannot be invoked from
  ;; the renderer thread: this is why we run the `startup' in a new
  ;; thread from there.
  (ffi-within-renderer-thread
   browser
   (lambda ()
     (run-thread "finalization"
       ;; Restart on init error, in case `*config-file*' broke the state.
       ;; We only `handler-case' when there is an init file, this way we avoid
       ;; looping indefinitely.
       (let ((restart-on-error? (not (or (getf *options* :no-config)
                                         (getf *options* :no-init) ; TODO: Deprecated, remove in 4.0.
                                         (not (uiop:file-exists-p (files:expand *config-file*)))))))
         ;; Set `*restart-on-error*' globally instead of let-binding it to
         ;; make it visible from all threads.
         (unwind-protect (progn (setf *restart-on-error* restart-on-error?)
                                (startup browser urls))
           (setf *restart-on-error* nil))))))
  ;; Set `init-time' at the end of finalize to take the complete startup time
  ;; into account.
  (setf (slot-value *browser* 'init-time)
        (local-time:timestamp-difference (local-time:now) startup-timestamp))
  (setf (slot-value *browser* 'ready-p) t))

(defmethod startup ((browser browser) urls)
  (labels ((clear-history-owners ()
             "Warning: We clear the previous owners here.
After this, buffers from a previous session are permanently lost, they cannot be
restored."
             (files:with-file-content (history (history-file *browser*))
               (when history
                 (clrhash (htree:owners history))))))
    ;; Remove existing windows.  This may happen if we invoked this function,
    ;; possibly with a different renderer.  To avoid mixing windows with
    ;; different renderers.  REVIEW: A better option would be to have
    ;; `update-instance-for-redefined-class' call `customize-instance', but this
    ;; is tricky to get right, in particular `ffi-buffer-make' seems to hang on
    ;; `web-buffer's.
    (mapcar #'window-delete (window-list))
    (window-make browser)
    (if (restore-session-on-startup-p *browser*)
        (if (restore-history-buffers (files:content (history-file *browser*))
                                     (history-file *browser*))
            (open-urls urls)
            (open-urls (or urls (list (default-new-buffer-url browser)))))
        (progn
          (log:info "Not restoring session.")
          (clear-history-owners)
          (open-urls (or urls (list (default-new-buffer-url browser))))))
    (hooks:run-hook *after-startup-hook*)
    (funcall* (startup-error-reporter-function *browser*))))

;; Catch a common case for a better error message.
(defmethod buffers :before ((browser t))
  (when (null browser)
    (error "There is no current *browser*. Is Nyxt started?")))

(-> set-window-title (&optional window buffer) *)
(export-always 'set-window-title)
(defun set-window-title (&optional (window (current-window)) (buffer (current-buffer)))
  "Set current window title to the return value of (titler window). "
  (declare (ignore buffer)) ; TODO: BUFFER is kept for backward compatibility.  Remove with 3.0.
  (setf (ffi-window-title window) (funcall (titler window) window)))

(-> window-default-title (window) string)
(export-always 'window-default-title)
(defun window-default-title (window)
  "Return a window title in the form 'Nyxt - URL'.
If Nyxt was started from a REPL, use 'Nyxt REPL - URL' instead.
This is useful to tell REPL instances from binary ones."
  (let* ((buffer (active-buffer window))
         (url (url buffer))
         (title (title buffer)))
    (setf title (if (str:emptyp title) "" title))
    (setf url (if (url-empty-p url) "<no url/name>" (render-url url)))
    (the (values string &optional)
         (str:concat "Nyxt" (when *run-from-repl-p* " REPL") " - "
                     title (unless (str:emptyp title) " - ")
                     url))))

;; REVIEW: Do we need :NO-FOCUS? It's not used anywhere.
(-> open-urls ((maybe (cons quri:uri *)) &key (:no-focus boolean)) *)
(defun open-urls (urls &key no-focus)
  "Create new buffers from URLs.
First URL is focused if NO-FOCUS is nil."
  (with-protect ("Could not make buffer to open ~a: ~a" urls :condition)
    (let ((first-buffer (first (mapcar
                                (lambda (url) (make-buffer :url url))
                                urls))))
      (when (and first-buffer (not no-focus))
        (if (open-external-link-in-new-window-p *browser*)
            (let ((window (window-make *browser*)))
              (window-set-buffer window first-buffer))
            (set-current-buffer first-buffer))))))

(defun scheme-keymap (buffer buffer-scheme)
  "Return the keymap in BUFFER-SCHEME corresponding to the BUFFER `keyscheme'.
If none is found, fall back to `keyscheme:cua'."
  (or (keymaps:get-keymap (keyscheme buffer)
                          buffer-scheme)
      (keymaps:get-keymap keyscheme:cua
                          buffer-scheme)))

(defun request-resource-open-url (&key url buffer &allow-other-keys)
  (make-buffer :url url :parent-buffer buffer))

(defun request-resource-open-url-focus (&key url buffer &allow-other-keys)
  (make-buffer-focus :url url :parent-buffer buffer))

(define-class request-data ()
  ((buffer
    (current-buffer)
    :type buffer
    :documentation "Buffer targeted by the request.")
   (url
    (quri:uri "")
    :documentation "URL of the request")
   (event-type
    :other
    :accessor nil ; TODO: No public accessor for now, we first need a use case.
    :export nil
    :documentation "The type of request, e.g. `:link-click'.")
   (new-window-p
    nil
    :documentation "Whether the request takes place in a
new window.")
   (http-method
    nil
    :documentation "The HTTP method (GET, POST and friends) of the request.")
   (toplevel-p
    nil
    :documentation "Whether the request happens in a toplevel frame.")
   (resource-p
    nil
    :documentation "Whether the request is a resource request.
Resource requests cannot be redirected or blocked.")
   (mime-type
    nil
    :documentation "The MIME type of the resource at the other end of the request.")
   (known-type-p
    nil
    :documentation "Whether the request is for content with
supported MIME-type, such as a picture that can be displayed in the web
view.")
   (file-name
    nil
    :documentation "The name this file will be saved on disk with, if downloaded.")
   (keys
    '()
    :documentation "The key sequence that generated the request."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'url-dispatching-handler)
(-> url-dispatching-handler
    (symbol
     (function (quri:uri) boolean)
     (or string (function (quri:uri) (or quri:uri null))))
    *)
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

\(defmethod configure-instance ((buffer buffer))
  (reduce #'hooks:add-hook
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
          :initial-value (request-resource-hook buffer)))"
  (make-instance
   'hooks:handler
   :fn (lambda (request-data)
         (let ((url (url request-data)))
           (if (funcall test url)
               (etypecase action
                 (function
                  (let* ((new-url (funcall action url)))
                    (log:info "Applied ~s URL-dispatcher on ~s and got ~s"
                              (symbol-name name)
                              (render-url url)
                              (when new-url (render-url new-url)))
                    (when new-url
                      (setf (url request-data) new-url)
                      request-data)))
                 (string (let ((action #'(lambda (url)
                                           (uiop:launch-program
                                            (format nil action
                                                    (render-url url)))
                                           nil)))
                           (funcall action url)
                           (log:info "Applied ~s shell-command URL-dispatcher on ~s"
                                     (symbol-name name)
                                     (render-url url)))))
               request-data)))
   :name name))

(defun javascript-error-handler (condition)
  (echo-warning "JavaScript error: ~a" condition))

(defun print-message (message &optional window)
  (let ((window (or window (current-window))))
    (when window
      (ffi-print-message window message))))

(export-always 'current-window)
(defun current-window (&optional no-rescan)
  ;; TODO: Get rid of the NO-RESCAN option and find a fast way to retrieve
  ;; current window reliably.
  ;; Tests:
  ;; - Make two windows and make sure prompt-buffer gets spawned in the right window.
  ;; - Delete the second window and see if the prompt-buffer still works in the first one.
  "Return the current window.
If NO-RESCAN is non-nil, fetch the window from the `last-active-window' cache
instead of asking the renderer for the active window.  It is faster but
sometimes yields the wrong result."
  (when *browser*
    (if (and no-rescan (slot-value *browser* 'last-active-window))
        (slot-value *browser* 'last-active-window)
        ;; No window when browser is not started or does not implement `ffi-window-active'.
        (ignore-errors (ffi-window-active *browser*)))))

(export-always 'set-current-buffer)
(defun set-current-buffer (buffer &key (focus t))
  "Set the active BUFFER for the active window.
Return BUFFER."
  (unless (eq 'prompt-buffer (sera:class-name-of buffer))
    (if (current-window)
        (window-set-buffer (current-window) buffer :focus focus)
        (make-window buffer))
    buffer))

(export-always 'current-prompt-buffer)
(defun current-prompt-buffer ()
  "Return the current prompt-buffer."
  (first (active-prompt-buffers (current-window))))

(export-always 'focused-buffer)
(defun focused-buffer (&optional (window (current-window)) )
  "Return the currently focused buffer."
  ;; TODO: Add message-buffer when we have the slot in `window'.
  (find-if #'ffi-focused-p
           (list (first (active-prompt-buffers window))
                 (active-buffer window)
                 (status-buffer window))))

(defmethod write-output-to-log ((browser browser))
  "Set the *standard-output* and *error-output* to write to a log file."
  (let ((buffer (current-buffer)))
    (values
     (sera:and-let* ((path (files:expand (standard-output-file buffer))))
       (setf *standard-output*
             (open path
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :append)))
     (sera:and-let* ((path (files:expand (error-output-file buffer))))
       (setf *error-output*
             (open path
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :append))))))

(define-internal-page-command-global reduce-to-buffer (&key (delete t))
    (reduced-buffer "*Reduced Buffers*")
  "Query the buffer(s) to \"reduce \" by copying their titles/URLs to a
single buffer, optionally delete them. This function is useful for archiving a
set of useful URLs or preparing a list to send to a someone else."
  (let ((buffers (prompt
                  :prompt "Reduce buffer(s)"
                  :sources (make-instance 'buffer-source
                                          :constructor (remove-if #'internal-url-p (buffer-list)
                                                                  :key #'url)
                                          :return-actions '(identity)
                                          :multi-selection-p t))))
    (unwind-protect
         (spinneret:with-html-string
           (:style (style reduced-buffer))
           (:h1 "Reduced Buffers:")
           (:div
            (if buffers
                (loop for buffer in buffers
                      collect
                      (with-current-buffer buffer
                        (:div
                         (:p (:b "Title: ") (title buffer))
                         (:p (:b "URL: ") (:a :href (render-url (url buffer))
                                              (render-url (url buffer))))
                         (:p (:b "Automatically generated summary: ")
                             (:ul
                              (loop for summary-bullet in (analysis:summarize-text
                                                           (document-get-paragraph-contents :limit 10000))
                                    collect (:li (str:collapse-whitespaces summary-bullet)))))
                         (:hr ""))))
                (:p "None chosen."))))
      (when delete (mapcar #'buffer-delete buffers)))))
