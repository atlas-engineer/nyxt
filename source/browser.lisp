;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type prompt-buffer (function (prompt-buffer))
  "Hook acting on `prompt-buffer'.")
(hooks:define-hook-type resource (function (request-data) (or request-data null))
  "Hook acting on `request-data' resource.
Returns:
- Possibly modified `request-data'---redirect/block request.
- NIL---block request.")
(hooks:define-hook-type browser (function (browser))
  "Hook acting on `browser' (likely `*browser*').")
(export-always '(hook-resource))

(define-class proxy ()
  ((url
    (quri:uri "socks5://127.0.0.1:9050")
    :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example: \"http://192.168.1.254:8080\".")
   (allowlist
    '("localhost" "localhost:8080")
    :type (list-of string)
    :documentation "A list of URIs not to forward to the proxy.")
   (proxied-downloads-p
    t
    :documentation "Non-nil if downloads should also use the proxy."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."))

(export-always 'combine-composed-hook-until-nil)
(define-generic combine-composed-hook-until-nil ((hook hooks:hook) &optional arg)
  "Return the composition of the HOOK handlers on ARG, from oldest to youngest.

Stop processing when a handler returns nil. Without handlers, return ARG.

This is an acceptable `hooks:combination' for `hooks:hook'."
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
  (:metaclass interface-class)
  (:documentation "Renderer-specific representation for the global browser.
Should be redefined by the renderer."))

(define-class browser (renderer-browser)
  ((profile
    (global-profile)
    :type nyxt-profile
    :documentation "Global profile used to specialize the behavior of
various parts, such as the path of all data files.
This profile is used when there is no context buffer.
See also the `profile' slot in the `buffer' class.")
   (remote-execution-p
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
    :type (list-of bt:thread)
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
    :documentation "The ring with all the clipboard contents Nyxt could cache.
Note that it may be incomplete.")
   (command-model
    (make-instance 'analysis:sequence-model)
    :documentation "This model is used to generate predictions for what the user will do.
Which commands will they invoke next?")
   (last-command
    nil
    :type (maybe function)
    :documentation "The last command invoked by the user.")
   (prompt-buffer-generic-history
    (make-ring)
    :documentation "The default history of all prompt buffer entries.
This history is used if no history is specified for a given prompt buffer.")
   (default-new-buffer-url
    (quri:uri (nyxt-url 'new))
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
See `buffer-list', `buffers-get', `buffers-set' and `buffers-delete'.")
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
    (time:now)
    :export nil
    :documentation "`time:timestamp' of when Nyxt was started.")
   (startup-promise
    (lpara:promise)
    :export nil
    :accessor nil
    :documentation "Promise used to make `start-browser' synchronous.
Without it, `start-browser' would return before the `*browser*' is effectively usable.
Implementation detail.")
   (init-time
    0.0
    :type alex:non-negative-real
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
    :documentation "Whether to replace renderer specific dialog boxes with the
prompt buffer.")
   (theme
    (make-instance 'theme:theme)
    :type theme:theme
    :documentation "The theme to use for all the browser interface elements.")
   (glyph-logo
    (gethash "nyxt.svg" *static-data*)
    :documentation "The logo of Nyxt as an SVG.")
   (history-file
    (make-instance 'history-file)
    :type history-file
    :documentation "History file to read from when restoring session.
See `restore-session-on-startup-p' to control this behavior.
See also `history-file' in `context-buffer' for per-buffer history files.")
   (restore-session-on-startup-p
    t
    :type boolean
    :documentation "Whether to restore buffers from the previous session.
You can store and restore sessions manually to various files with
`store-history-by-name' and `restore-history-by-name'.")
   (default-cookie-policy
    :no-third-party
    :type cookie-policy
    :documentation "Cookie policy of new buffers.
Valid values are `:accept', `:never' and `:no-third-party'.")
   ;; Hooks follow:
   (after-init-hook
    (make-instance 'hook-browser)
    :documentation "The entry-point hook to configure everything in Nyxt.
The hook takes browser as the argument.

This hook is run after the `*browser*' is instantiated and before the
`startup' is run.

A handler can be added with:
\(define-configuration browser
  (after-init-hook (hooks:add-hook %slot-value% 'my-init-handler)))")
   (after-startup-hook
    (make-instance 'hook-browser)
    :documentation "Hook run when the browser is started and ready for interaction.
The handlers take browser as the argument.

A handler can be added with:
\(define-configuration browser
  (after-startup-hook (hooks:add-hook %slot-value% 'my-startup-handler)))")
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
   (external-editor-program
    (or (uiop:getenvp "VISUAL")
        (uiop:getenvp "EDITOR")
        (when (sera:resolve-executable "gio") "gio open"))
    :type (or string null)
    :reader nil
    :writer t
    :export t
    :documentation "The external editor to use for editing files.
The full command, including its arguments, may be specified as list of strings
or as a single string."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "The browser class defines the overall behavior of Nyxt, in
the sense that it manages the display of buffers.  For instance, it abstracts
the renderer, and lays the foundations to track and manipulate buffers and
windows.

A typical Nyxt session encompasses a single instance of this class, but nothing
prevents otherwise.")
  (:metaclass user-class))

(defmethod initialize-instance :after ((browser browser)
                                       &key (history-file (make-instance 'history-file :profile (profile browser)))
                                       &allow-other-keys)
  "Ensure `history-file' uses the browser profile."
  (setf (history-file browser) history-file))

(defmethod theme ((ignored (eql nil)))
  "Fallback theme in case `*browser*' is NIL."
  (declare (ignore ignored))
  (make-instance 'theme:theme))

(defmethod external-editor-program ((browser browser))
  "Specialized reader for `external-editor-program' slot.
A list of strings is returned, as to comply with `uiop:launch-program' or
`uiop:run-program'."
  (with-slots ((cmd external-editor-program)) browser
    (if (str:blank? cmd)
        (progn (echo-warning "Invalid value of `external-editor-program' browser slot.") nil)
        (str:split " " cmd))))

(defmethod get-containing-window-for-buffer ((buffer buffer) (browser browser))
  "Get the window containing a buffer."
  (find buffer (alex:hash-table-values (windows browser)) :key #'active-buffer))

(defmacro on-renderer-ready (thread-name &body body)
  "Run BODY from a new thread when renderer is ready.
`ffi-within-renderer-thread' runs its body on the renderer thread when it's
idle, so it should do the job."
  `(ffi-within-renderer-thread (lambda () (run-thread ,thread-name ,@body))))

(defmethod finalize-startup ((browser browser) urls startup-timestamp)
  "Run `after-init-hook' then BROWSER's `startup'."
  ;; `messages-appender' requires `*browser*' to be initialized.
  (unless (find-if (sera:eqs 'messages-appender) (log4cl:all-appenders)
                   :key #'sera:class-name-of)
    (log4cl:add-appender log4cl:*root-logger* (make-instance 'messages-appender)))
  (ignore-errors
   (handler-bind ((error (lambda (c) (log:error "In after-init-hook: ~a" c))))
     (hooks:run-hook (after-init-hook browser) browser))) ; TODO: Run outside the main loop?
  ;; `startup' must be run _after_ this function returns; It's not enough since
  ;; the `startup' may invoke the prompt buffer, which cannot be invoked from
  ;; the renderer thread: this is why we run the `startup' in a new thread from
  ;; there.
  (on-renderer-ready "finalize-window"
    ;; Restart on init error, in case `*config-file*' broke the state.
    ;; We only `handler-case' when there is an init file, this way we avoid
    ;; looping indefinitely.
    (let ((restart-on-error? (not (or (getf *options* :no-config)
                                      (not (uiop:file-exists-p (files:expand *config-file*)))))))
      ;; Set `*restart-on-error*' globally instead of let-binding it to
      ;; make it visible from all threads.
      (setf *restart-on-error* restart-on-error?)
      (finalize-window browser urls)))
  ;; Set `init-time' at the end of finalize to take the complete startup time
  ;; into account.
  (setf (slot-value *browser* 'init-time)
        (time:timestamp-difference (time:now) startup-timestamp))
  (setf (slot-value *browser* 'ready-p) t))

(defmethod finalize-window ((browser browser) urls)
  "Startup finalization: Set up initial window.
This step is crucial to get Nyxt to reach a usable step and be able to handle
errors correctly from then on."
  ;; Remove existing windows.  This may happen if we invoked this function,
  ;; possibly with a different renderer.  To avoid mixing windows with
  ;; different renderers.  REVIEW: A better option would be to have
  ;; `update-instance-for-redefined-class' call `customize-instance', but this
  ;; is tricky to get right, in particular `ffi-buffer-make' seems to hang on
  ;; `web-buffer's.
  (mapcar #'window-delete (window-list))
  (window-make browser)
  ;; History restoration and subsequent tasks are error-prone, thus they should
  ;; be done once the browser is ready to handle errors ,that is ,once the
  ;; renderer has displayed the window and its initial buffer.
  (on-renderer-ready "finalize-buffer"
    (finalize-first-buffer browser urls)))

(defmethod finalize-first-buffer ((browser browser) urls)
  "Startup finalization: Set up initial buffer."
  (switch-buffer :buffer (make-buffer :url (quri:uri (nyxt-url 'new)) :no-history-p t))
  (on-renderer-ready "finalize-history"
    ;; If we've reached here browser should be functional, no need to restart on error.
    (setf *restart-on-error* nil)
    (finalize-history browser urls)))

(defmethod finalize-history ((browser browser) urls)
  "Startup finalization: Restore history, open URLs, display startup errors."
  (macrolet ((with-protected-history (&body body)
               `(with-protect ("Error restoring history ~a: ~a"
                               (files:expand (history-file *browser*))
                               :condition)
                  ,@body)))
    (labels ((clear-history-owners ()
               "Warning: We clear the previous owners here.
After this, buffers from a previous session are permanently lost, they cannot be
restored."
               (with-protected-history
                   (files:with-file-content (history (history-file *browser*))
                     (when history
                       (clrhash (htree:owners history)))))))
      ;; Must catch all history-related errors, otherwise subsequent code would
      ;; not be run.
      (handler-case
          (let ((init-buffer (current-buffer)))
            (if (restore-session-on-startup-p *browser*)
                (if (with-protected-history
                        (restore-history-buffers
                         (files:content (history-file *browser*))
                         (history-file *browser*)))
                    (open-urls urls)
                    (open-urls (or urls (list (default-new-buffer-url browser)))))
                (progn
                  (log:info "Not restoring session.")
                  (clear-history-owners)
                  (open-urls (or urls (list (default-new-buffer-url browser))))))
            (buffer-delete init-buffer))
        (error (c)
          ;; TODO: Clear buffers or back up history?
          (log:warn c)))
      (lpara:fulfill (slot-value browser 'startup-promise))
      (hooks:run-hook (after-startup-hook browser) browser)
      (funcall* (startup-error-reporter-function browser)))))

;; Catch a common case for a better error message.
(defmethod buffers :before ((browser t))
  (when (null browser)
    (error "There is no current *browser*. Is Nyxt started?")))

(-> set-window-title (&optional window) *)
(export-always 'set-window-title)
(defun set-window-title (&optional (window (current-window)))
  "Set current window title to the return value of (titler window). "
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

(defun get-keymap (buffer buffer-keyscheme-map)
  "Return the keymap in BUFFER-KEYSCHEME-MAP corresponding to BUFFER's `keyscheme'.
If none is found, fall back to `keyscheme:cua'."
  (keymaps:get-keymap (or (keyscheme buffer) keyscheme:cua) buffer-keyscheme-map))

(defun request-resource-open-url (&key url buffer &allow-other-keys)
  (make-buffer :url url :parent-buffer buffer))

(defun request-resource-open-url-focus (&key url buffer &allow-other-keys)
  (make-buffer-focus :url url :parent-buffer buffer))

(export-always 'renderer-request-data)
(defclass renderer-request-data ()
  ()
  (:metaclass interface-class)
  (:documentation "Renderer-specific request object.
Should be redefined by the renderer."))

(define-class request-data (renderer-request-data)
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
    :type (maybe string)
    :documentation "The HTTP method (GET, POST and friends) of the request.")
   (request-headers
    nil
    :type trivial-types:association-list
    :documentation "Dotted alist of headers for the request.")
   (response-headers
    nil
    :type trivial-types:association-list
    :documentation "Dotted alist of headers for the response to the given request.")
   (toplevel-p
    nil
    :documentation "Whether the request happens in a toplevel frame.")
   (resource-p
    nil
    :documentation "Whether the request is a resource request.
Resource requests cannot be redirected or blocked.")
   (mime-type
    nil
    :type (maybe string)
    :documentation "The MIME type of the resource at the other end of the request.")
   (known-type-p
    nil
    :documentation "Whether the request is for content with
supported MIME-type, such as a picture that can be displayed in the web
view.")
   (file-name
    nil
    :type (maybe string)
    :documentation "The name this file will be saved on disk with, if downloaded.")
   (keys
    '()
    :type list
    :documentation "The key sequence that generated the request."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Representation of HTTP(S) request.
Most important slots are:
- `buffer' request belongs to.
- `url' requested.
- `request-headers'/`response-headers' for headers it's requested with.
- and `toplevel-p'/`resource-p' for whether it's a new page or resource
  request (respectively)."))

(export-always 'url-dispatching-handler)
(-> url-dispatching-handler
    (symbol
     (function (quri:uri) boolean)
     (or string (function (quri:uri) (or quri:uri null))))
    *)
(defun url-dispatching-handler (name test action)
  "Return a `hook-request' handler apply its ACTION on the URLs conforming to TEST.
Fit for `request-resource-hook'.

TEST should be function of one argument, the requested URL.

ACTION can be either
- a shell command as a string,
- or a function taking a URL as argument.

In case ACTION returns nil (always the case for shell command), URL request is
aborted. If ACTION returns a URL, it's loaded.

`match-host', `match-scheme', `match-domain' and `match-file-extension' can be
used to create TEST-functions, but any other function of one argument would fit
the TEST slot as well.

The following example does a few things:
- Forward DOI links to the doi.org website.
- Open magnet links with Transmission.
- Open local files (file:// URIs) with Emacs.

\(define-configuration web-buffer
  (request-resource-hook
   (hooks:add-hook %slot-value%
                   (url-dispatching-handler
                    'doi-link-dispatcher
                    (match-scheme \"doi\")
                    (lambda (url)
                      (quri:uri (format nil \"https://doi.org/~a\"
                                        (quri:uri-path url))))))))

\(defmethod customize-instance ((buffer web-buffer))
  (hooks:add-hook
   (request-resource-hook buffer)
   (url-dispatching-handler
    'transmission-magnet-links
    (match-scheme \"magnet\")
    \"transmission-remote --add ~a\"))
  (hooks:add-hook
   (request-resource-hook buffer)
   (url-dispatching-handler
    'emacs-file
    (match-scheme \"file\")
    (lambda (url)
      (uiop:launch-program
       `(\"emacs\" ,(quri:uri-path url)))
      nil))))"
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

(defun print-message (html-body &optional (window (current-window)))
  (with-slots (message-buffer) window
    (when (and window message-buffer)
      (ffi-print-message window html-body))))

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
    (set-window-title)
    buffer))

(export-always 'current-prompt-buffer)
(defun current-prompt-buffer ()
  "Return the current prompt-buffer."
  (first (active-prompt-buffers (current-window))))

(export-always 'focused-buffer)
(defun focused-buffer (&optional (window (current-window)) )
  "Return the currently focused buffer."
  (find-if #'ffi-focused-p
           (list (first (active-prompt-buffers window))
                 (active-buffer window)
                 (status-buffer window)
                 (message-buffer window))))

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
                                          :actions-on-return #'identity
                                          :enable-marks-p t))))
    (unwind-protect
         (spinneret:with-html-string
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

(export-always 'render-menu)
(defun render-menu (mode-symbol &optional (buffer (current-buffer)))
  "Render a menu for a given mode symbol."
  (spinneret:with-html
    (:div :class "mode-menu"
          (loop for command in (nyxt::list-mode-commands mode-symbol)
                collect
                   (let ((name (string-downcase (closer-mop:generic-function-name command)))
                         (bindings (keymaps:pretty-binding-keys
                                    (name command)
                                    (current-keymaps buffer)
                                    :print-style (keymaps:name (keyscheme buffer)))))
                     (:nbutton
                       :class "button binding"
                       :text (if bindings (first bindings) "⏎")
                       `(nyxt::run-async ,command))
                     (:nbutton
                       :class "button command"
                       :text name
                       `(nyxt::run-async ,command)))))))
