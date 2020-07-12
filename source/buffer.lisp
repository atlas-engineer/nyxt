(in-package :nyxt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'buffer)
  (export
   '(id
     url
     url-at-point
     title
     load-status
     modes
     default-modes
     enable-mode-hook
     disable-mode-hook
     keymap-scheme-name
     current-keymaps-hook
     override-map
     forward-input-events-p
     request-resource-scheme
     request-resource-hook
     default-new-buffer-url
     scroll-distance
     horizontal-scroll-distance
     current-zoom-ratio
     zoom-ratio-step
     zoom-ratio-min
     zoom-ratio-max
     zoom-ratio-default
     page-scroll-ratio
     cookies-path
     box-style
     highlighted-box-style
     proxy
     certificate-whitelist
     buffer-delete-hook
     default-cookie-policy)))
(defclass buffer ()
  ((id :accessor id
       :initarg :id
       :type string
       :initform ""
       :documentation "Unique identifier for a buffer.
Dead buffers or placeholder buffers (i.e. those not associated with a web view)
have an empty ID.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (url :accessor url :initarg :url :type quri:uri :initform (quri:uri ""))
   (url-at-point :accessor url-at-point :type quri:uri :initform (quri:uri ""))
   (title :accessor title :initarg :title :type string :initform "")
   (load-status ;; :accessor load-status ; TODO: Need to decide if we want progress / errors before exposing to the user.
                :initarg :load-status
                :type (or (eql :loading)
                          (eql :finished)
                          (eql :unloaded))
                :initform :unloaded
                :documentation "The status of the buffer.
- `:loading' when loading a web resource.
- `:finished' when done loading a web resource.
- `:unloaded' for buffers that have not been loaded yet, like
  session-restored buffers, dead buffers or new buffers that haven't started the
  loading process yet..")
   (last-access :accessor last-access
                :initform (local-time:now)
                :type local-time:timestamp
                :documentation "Timestamp when the buffer was last switched to.")
   (modes :accessor modes
          :initform '()
          :documentation "The list of mode instances.
Modes are instantiated after the `default-modes' slot, with `initialize-modes'
and not in the initform so that the instantiation form can access the
initialized buffer.")
   (default-modes :accessor default-modes
                  :initarg :default-modes
                  :type list-of-symbols
                  :initform '(certificate-whitelist-mode web-mode base-mode)
                  :documentation "The symbols of the modes to instantiate on buffer creation.
The mode instances are stored in the `modes' slot.")
   (enable-mode-hook :accessor enable-mode-hook
                     :initarg :enable-mode-hook
                     :initform (make-hook-mode)
                     :type hook-mode
                     :documentation "Hook run on every mode activation,
after the mode-specific hook.")
   (disable-mode-hook :accessor disable-mode-hook
                      :initarg :disable-mode-hook
                      :initform (make-hook-mode)
                      :type hook-mode
                      :documentation "Hook run on every mode deactivation,
after the mode-specific hook.")
   (keymap-scheme-name
    :accessor keymap-scheme-name
    :initarg :keymap-scheme-name
    :initform scheme:cua
    :type keymap:scheme-name
    :documentation "The keymap scheme that will be used for all modes in the current buffer.")
   (current-keymaps-hook
    :accessor current-keymaps-hook
    :initarg :current-keymaps-hook
    :type hook-keymaps-buffer
    :initform (make-hook-keymaps-buffer
               :combination #'hooks:combine-composed-hook)
    :documentation "Hook run as a return value of `current-keymaps'.")
   (override-map :accessor override-map
                 :initarg :override-map
                 :initform (let ((map (make-keymap "overide-map")))
                             (define-key map
                               "C-space" 'execute-command))
                 :documentation "Keymap that overrides all other bindings.
No libraries should ever touch the override-map, this is left for the user to
customize to their needs.

Example:

\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"overide-map\")))
                             (define-key map
                               \"M-x\" 'execute-command
                               \"C-q\" 'quit)
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
                                         "C-button1" 'request-resource-open-url-focus
                                         "button2" 'request-resource-open-url-focus
                                         "C-shift-button1" 'request-resource-open-url))
                            :documentation "This keymap can be looked up when
`request-resource-hook' handlers run.
The functions are expected to take key arguments like `:url'.")
   (request-resource-hook :accessor request-resource-hook
                          :initarg :request-resource-hook
                          :initform (make-hook-resource
                                     :combination #'combine-composed-hook-until-nil
                                     :handlers (list #'request-resource))
                          :documentation "Hook run on every resource load.
The handlers are composed, passing a `request-data'
until one of them returns nil or all handlers apply successfully.

Newest hook is run first.
If a `request-data' object is returned, it gets passed to other handlers
or right to the renderer if there are no more handlers.
If nil is returned, stop the hook and cancel the resource load.

The current buffer URL should not be relied upon.  With WebKitGTK, it is the same
as (url REQUEST-DATA).
If you need to access the URL before this request, inspect the web-mode history.

There's no more ability to pass the results to the renderer with :FORWARD.

Example:

\(define-configuration buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource (list #'old-reddit-handler #'auto-proxy-handler))
            :initial-value %slot-default))))")
   (default-new-buffer-url :accessor default-new-buffer-url
                           :initform (quri:uri "https://nyxt.atlas.engineer/start")
                           :documentation "The URL set to a new blank buffer opened by Nyxt.")
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
                 :initform (make-instance 'cookies-data-path :basename "cookies.txt")
                 :documentation "The path where cookies are stored.  Not all
renderers might support this.")
   (box-style :accessor box-style
              :initform (cl-css:css
                         '((".nyxt-hint"
                            :background "linear-gradient(#fcff9e, #efcc00)"
                            :color "black"
                            :border "1px black solid"
                            :padding "1px 3px 1px 3px"
                            :border-radius "2px"
                            :z-index #.(1- (expt 2 31)))))
              :documentation "The style of the boxes, e.g. link hints.")
   (highlighted-box-style :accessor highlighted-box-style
                          :initform (cl-css:css
                                     '((".nyxt-hint.nyxt-highlight-hint"
                                        :font-weight "500"
                                        :background "#fcff9e")))

                          :documentation "The style of highlighted boxes, e.g. link hints.")
   (proxy :initform nil
          :type (or proxy null)
          :documentation "Proxy for buffer.")
   (certificate-whitelist :accessor certificate-whitelist
                          :initform '()
                          :type list-of-strings
                          :documentation  "A list of hostnames for which certificate errors shall be ignored.")
   (buffer-load-hook ;; :accessor buffer-load-hook ; TODO: Export?  Maybe not since `request-resource-hook' mostly supersedes it.
                 :initform (make-hook-uri->uri
                            :combination #'hooks:combine-composed-hook)
                 :type hook-uri->uri
                 :documentation "Hook run in `buffer-load' after `parse-url' was processed.
The handlers take the URL going to be loaded as argument
and must return a (possibly new) URL.")
   (buffer-delete-hook :accessor buffer-delete-hook
                       :initform (make-hook-buffer)
                       :type hook-buffer
                       :documentation "Hook run before `buffer-delete' takes effect.
The handlers take the buffer as argument.")
   (default-cookie-policy :accessor default-cookie-policy
                          :initarg :default-cookie-policy
                          :type cookie-policy
                          :initform :no-third-party
                          :documentation "Cookie policy of new buffers.
Must be one of `:always' (accept all cookies), `:never' (reject all cookies),
`:no-third-party' (accept cookies for current website only).")))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (ffi-buffer-set-proxy buffer
                            (server-address proxy)
                            (whitelist proxy))
      (ffi-buffer-set-proxy buffer
                            (quri:uri "")
                            nil)))

(declaim (ftype (function (buffer &key (:downloads-only boolean))) proxy-adress))
(defun proxy-address (buffer &key (downloads-only nil))
  "Return the proxy address, nil if not set.
If DOWNLOADS-ONLY is non-nil, then it only returns the proxy address (if any)
when `proxied-downloads-p' is true."
  (let* ((proxy (and buffer (proxy buffer)))
         (proxied-downloads (and proxy (proxied-downloads-p proxy))))
    (when (or (not downloads-only)
              proxied-downloads)
      (server-address proxy))))

(defmethod initialize-modes ((buffer buffer))
  "Initialize BUFFER modes.
This must be called after BUFFER has been created by the renderer.
See `buffer-make'."
  (dolist (mode-symbol (reverse (default-modes buffer)))
    (make-mode mode-symbol buffer)))

(export-always 'on-signal-notify-uri)
(defmethod on-signal-notify-uri ((buffer buffer) no-uri)
  "Set BUFFER's `url' slot, then dispatch `on-signal-notify-uri' over the
BUFFER's modes."
  (declare (ignore no-uri))
  (setf (url buffer) (ffi-buffer-uri buffer))
  (dolist (mode (modes buffer))
    (on-signal-notify-uri mode (url buffer)))
  (url buffer))

(export-always 'on-signal-notify-title)
(defmethod on-signal-notify-title ((buffer buffer) no-title)
  "Set BUFFER's `title' slot, then dispatch `on-signal-notify-title' over the
BUFFER's modes."
  (declare (ignore no-title))
  (setf (title buffer) (ffi-buffer-title buffer))
  (dolist (mode (modes buffer))
    (on-signal-notify-title mode (url buffer)))
  (title buffer))

(export-always 'on-signal-load-committed)
(defmethod on-signal-load-committed ((buffer buffer) url)
  (declare (ignore buffer url))
  nil)

(export-always 'on-signal-load-finished)
(defmethod on-signal-load-finished ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-finished mode url)))

(defun default-mode-symbols ()
  "Return default mode symbols (with package prefix)."
  (let ((default-modes
          (second (getf (mopu:slot-properties (find-class 'buffer) 'default-modes)
                        :initform))))
    (mapcar (alex:compose #'sym #'mode-command) default-modes)))

(hooks:define-hook-type buffer (function (buffer)))

(defclass-export buffer-description ()
  ((url :initarg :url
        :type (or quri:uri string)
        :initform (quri:uri "")
        :documentation "URL should be a `quri:uri'.
We also support the `string' type only for serialization purposes.  The URL is
automatically turned into a `quri:uri' in the accessor.")
   (title :accessor title
          :initarg :title
          :type string
          :initform "")))

(defmethod url ((bd buffer-description))
  "This accessor ensures we always return a `quri:uri'.
This is useful in cases the URL is originally stored as a string (for instance
when deserializing a `buffer-description').

We can't use `initialize-instance :after' to convert the URL because
`s-serialization:deserialize-sexp' sets the slots manually after making the
class."
  (unless (quri:uri-p (slot-value bd 'url))
    (setf (slot-value bd 'url) (ensure-url (slot-value bd 'url))))
  (slot-value bd 'url))

(defmethod (setf url) (value (bd buffer-description))
  (setf (slot-value bd 'url) value))

(defmethod s-serialization::serialize-sexp-internal ((bd buffer-description)
                                                     stream
                                                     serialization-state)
  "Serialize `buffer-description' by turning the URL into a string."
  (let ((new-bd (make-instance 'buffer-description
                               :title (title bd))))
    (setf (url new-bd) (object-string (url bd)))
    (call-next-method new-bd stream serialization-state)))

(defmethod object-string ((buffer-description buffer-description))
  (object-string (url buffer-description)))

(defmethod object-display ((buffer-description buffer-description))
  (format nil "~a  ~a"
          (title buffer-description)
          (object-display (url buffer-description))))

(export-always 'equals)
(defmethod equals ((bd1 buffer-description) (bd2 buffer-description))
  "Comparison function for buffer history entries.
An entry is uniquely identified from its URL.  We do not take the
title into accound as it may vary from one load to the next."
  (quri:uri= (url bd1) (url bd2)))

(defmethod object-string ((buffer buffer))
  (object-string (url buffer)))

(defmethod object-display ((buffer buffer))
  (format nil "~a  ~a" (title buffer) (object-display (url buffer))))

(define-command make-buffer (&key (title "") modes (url ""))
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'."
  (let* ((buffer (buffer-make *browser* :title title :default-modes modes))
         (url (if (eq url :default)
                  (default-new-buffer-url buffer)
                  url)))
    (unless (url-empty-p url)
      (buffer-load url :buffer buffer))
    buffer))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
(export-always '*buffer-class*)
(defvar *buffer-class* 'buffer)

(declaim (ftype (function (browser &key (:title string) (:default-modes list) (:dead-buffer buffer))) buffer-make))
(defun buffer-make (browser &key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
Run `*browser*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let* ((buffer (if dead-buffer
                     (progn
                       ;; Dead buffer ID must be renewed before calling `ffi-buffer-make'.
                       (setf (id dead-buffer) (get-unique-buffer-identifier *browser*))
                       (ffi-buffer-make dead-buffer))
                     (apply #'make-instance *buffer-class*
                            :id (get-unique-buffer-identifier *browser*)
                            (append (when title `(:title ,title))
                                    (when default-modes `(:default-modes ,default-modes)))))))
    (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after the view has been created.
    (initialize-modes buffer)
    (when dead-buffer
      (setf (url buffer) (url dead-buffer)))
    (when (expand-path (cookies-path buffer))
      (ensure-parent-exists (expand-path (cookies-path buffer))))
    (buffers-set (id buffer) buffer)
    (unless (last-active-buffer browser)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer browser) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (hooks:run-hook (buffer-make-hook browser) buffer)
    buffer))

(declaim (ftype (function (buffer)) buffer-delete))
(defun buffer-delete (buffer)
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find buffer (window-list) :key 'active-buffer)))
    (when parent-window
      (let ((replacement-buffer (or (first (get-inactive-buffers))
                                    (make-buffer :url :default))))
        (window-set-active-buffer parent-window
                                  replacement-buffer)))
    (ffi-buffer-delete buffer)
    (buffers-delete (id buffer))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *browser*)
      ((guard f f)
       (funcall-safely f)))))

(export-always 'buffer-list)
(defun buffer-list (&key sort-by-time domain)
  (let* ((buffer-list (alex:hash-table-values (slot-value *browser* 'buffers)))
         (buffer-list (if sort-by-time (sort
                                        buffer-list #'local-time:timestamp> :key #'last-access)
                          buffer-list))
         (buffer-list (if domain (remove-if-not
                                  (lambda (i) (equal domain (quri:uri-domain (url i)))) buffer-list)
                          buffer-list)))
    buffer-list))

(defun buffers-get (id)
  (gethash id (slot-value *browser* 'buffers)))

(defun buffers-set (id buffer)
  (setf (gethash id (slot-value *browser* 'buffers)) buffer)
  (print-status))

(defun buffers-delete (id)
  (remhash id (slot-value *browser* 'buffers))
  (print-status))

(export-always 'window-list)
(defun window-list ()
  (alex:hash-table-values (windows *browser*)))

(declaim (ftype (function (window buffer)) window-set-active-buffer))
(export-always 'window-set-active-buffer)
(defun window-set-active-buffer (window buffer) ; TODO: Rename window-set-buffer.
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (window-set-active-buffer-hook window) window buffer)
  ;; The current buffer last-access time is set to now to ensure it becomes the
  ;; second newest buffer.  If we didn't update the access time, the buffer
  ;; last-access time could be older than, say, buffers opened in the
  ;; background.
  (setf (last-access (active-buffer window)) (local-time:now))
  (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                       :key #'active-buffer)))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (make-instance *buffer-class*))
              (old-buffer (active-buffer window)))
          (log:debug "Swapping old buffer ~a with other window ~a to switch to ~a"
                     (object-string (url old-buffer))
                     (object-string (url (active-buffer window-with-same-buffer)))
                     (object-string (url buffer)))
          (ffi-window-set-active-buffer window-with-same-buffer temp-buffer)
          (ffi-window-set-active-buffer window buffer)
          (setf (active-buffer window) buffer)
          (window-set-active-buffer window-with-same-buffer old-buffer)
          (ffi-buffer-delete temp-buffer))
        (progn
          (ffi-window-set-active-buffer window buffer)
          (setf (active-buffer window) buffer)))
    (setf (last-access buffer) (local-time:now))
    (setf (last-active-buffer *browser*) buffer)
    (set-window-title window buffer)
    (print-status nil window)
    (when (and (eq (slot-value buffer 'load-status) :unloaded)
               ;; TODO: Find a better way to filter out non-webpages ("internal
               ;; buffers" like the REPL or help pages).  Could use a marker
               ;; slot or a specialized class.
               (not (url-empty-p (url buffer))))
      (reload-current-buffer buffer))))

(defun get-inactive-buffers ()
  "Return inactive buffers sorted by last-access timestamp, or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer (window-list)))
        (buffers (buffer-list)))
    (match (set-difference buffers active-buffers)
      ((guard diff diff)
       ;; Display the most recent inactive buffer.
       (sort diff #'local-time:timestamp> :key #'last-access)))))

(export-always 'buffer-suggestion-filter)
(defun buffer-suggestion-filter (&key current-is-last-p domain)
  (let ((buffers (buffer-list :sort-by-time t :domain domain)))
    (when current-is-last-p
      (setf buffers (alex:rotate buffers -1)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) buffers))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (copy-to-clipboard (object-string (url (current-buffer))))
  (echo "~a copied to clipboard." (object-string (url (current-buffer)))))

(define-command copy-title ()
  "Save current page title to clipboard."
  (copy-to-clipboard (title (current-buffer)))
  (echo "~a copied to clipboard." (title (current-buffer))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Switch to buffer"
                         ;; For commodity, the current buffer shouldn't be the first one on the list.
                         :suggestion-function (buffer-suggestion-filter :current-is-last-p t))))
    (set-current-buffer buffer)))

(define-command switch-buffer-domain (&optional (buffer (current-buffer)))
  "Switch the active buffer in the current window from the current domain."
  (let ((domain (quri:uri-domain (url buffer))))
    (with-result (buffer (read-from-minibuffer
                          (make-minibuffer
                           :input-prompt "Switch to buffer in current domain:"
                           :suggestion-function (buffer-suggestion-filter
                                                 :domain domain
                                                 :current-is-last-p t))))
      (set-current-buffer buffer))))

(define-command make-buffer-focus (&key (url :default))
  "Switch to a new buffer.
See `make-buffer'."
  (let ((buffer (make-buffer :url url)))
    (set-current-buffer buffer)
    buffer))

(define-command delete-buffer ()
  "Delete the buffer(s) via minibuffer input."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (mapcar #'buffer-delete buffers)))

(defun delete-buffers ()
  "Delete all buffers."
  (mapcar #'buffer-delete (buffer-list)))

(define-command delete-all-buffers ()
  "Delete all buffers, with confirmation."
  (let ((count (length (buffer-list))))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (delete-buffers))))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (buffer-delete buffer))

(define-command delete-other-buffers (&optional (buffer (current-buffer)))
  "Delete all other buffers but `buffer` which if not explicitly set defaults
to the currently active buffer."
  (let* ((all-buffers (buffer-list))
         (buffers-to-delete (remove buffer all-buffers))
         (count (list-length buffers-to-delete)))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (mapcar #'buffer-delete buffers-to-delete))))

;; WARNING: Don't use this parenscript, use the TITLE buffer slot instead.
(define-parenscript %%buffer-get-title () ; TODO: `on-signal-load-finished' should
                                          ; pass the title so that we don't have
                                          ; to call this.
  (ps:chain document title))

(export-always 'buffer-load)
(declaim (ftype (function ((or quri:uri string) &key (:buffer buffer)) t) buffer-load))
(defun buffer-load (input-url &key (buffer (current-buffer)))
  "Load INPUT-URL in BUFFER.
If INPUT-URL is a string, it's transformed to a `quri:uri' by `parse-url'.
URL is then transformed by BUFFER's `buffer-load-hook'."
  (let ((url (if (stringp input-url)
                 (parse-url input-url)
                 input-url)))
    (handler-case
        (progn
          (let ((new-url (hooks:run-hook (slot-value buffer 'buffer-load-hook) url)))
            (check-type new-url quri:uri)
            (setf url new-url)
            (ffi-buffer-load buffer url)))
      (error (c)
        (log:error "In `buffer-load-hook': ~a" c)))))

(define-command set-url (&key new-buffer-p prefill-current-url-p)
  "Set the URL for the current buffer, completing with history."
  (let ((history (minibuffer-set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (with-result (url (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt (format nil "Open URL in ~A buffer"
                                              (if new-buffer-p
                                                  "new"
                                                  "current"))
                        :input-buffer (if prefill-current-url-p
                                          (object-string (url (current-buffer))) "")
                        :default-modes '(set-url-mode minibuffer-mode)
                        :suggestion-function (history-suggestion-filter
                                              :prefix-urls (list (object-string
                                                                  (url (current-buffer)))))
                        :history history
                        :must-match-p nil)))
      (when (typep url 'history-entry)
        ;; In case read-from-minibuffer returned a string upon
        ;; must-match-p.
        (setf url (url url)))
      (buffer-load url :buffer (if new-buffer-p
                                   (make-buffer-focus)
                                   (current-buffer))))))

(define-command set-url-from-current-url ()
  "Set the URL for the current buffer, pre-filling in the current URL."
  (set-url :prefill-current-url-p t))

(define-command set-url-new-buffer ()
  "Prompt for a URL and set it in a new focused buffer."
  (set-url :new-buffer-p t))

(define-command reload-current-buffer (&optional (buffer (current-buffer)))
  "Reload of BUFFER or current buffer if unspecified."
  (buffer-load (url buffer) :buffer buffer))

(define-command reload-buffer ()
  "Reload queried buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Reload buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (mapcar #'reload-current-buffer buffers)))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers.
That is to say, the one with the most recent access time after the current buffer.
The current buffer access time is set to be the last so that if we keep calling
this command it cycles through all buffers."
  (let* ((buffers (buffer-list :sort-by-time t))
         (last-buffer (alex:last-elt buffers))
         (current-buffer (current-buffer)))
    (when (second buffers)
      (set-current-buffer (second buffers))
      ;; Set the last-access time after switching buffer, since switching
      ;; buffers already sets the slot.
      (setf (last-access current-buffer)
            (local-time:timestamp- (last-access last-buffer) 1 :sec)))))

(define-command switch-buffer-next ()   ; TODO: Rename switch-buffer-oldest
  "Switch to the oldest buffer in the list of buffers."
  (let* ((buffers (buffer-list :sort-by-time t))
         (oldest-buffer (alex:last-elt buffers)))
    (when (eq oldest-buffer (current-buffer))
      ;; Current buffer may already be the oldest, e.g. if other buffer was
      ;; opened in the background.
      (setf oldest-buffer (or (second (nreverse buffers))
                              oldest-buffer)))
    (set-current-buffer oldest-buffer)))

(export-always 'mode-name)
(defun mode-name (mode)
  (class-name (class-of mode)))

(declaim (ftype (function (list-of-symbols &optional buffer)) disable-modes enable-modes))
(export-always 'disable-modes)
(defun disable-modes (modes &optional (buffer (current-buffer)))
  "Disable MODES for BUFFER.
MODES should be a list symbols, each possibly returned by `mode-name'."
  (dolist (mode modes)
    (funcall-safely (sym (mode-command mode))
                    :buffer buffer :activate nil)))

(export-always 'enable-modes)
(defun enable-modes (modes &optional (buffer (current-buffer)))
  "Enable MODES for BUFFER.
MODES should be a list of symbols, each possibly returned by `mode-name'."
  (dolist (mode modes)
    (funcall-safely (sym (mode-command mode))
                    :buffer buffer :activate t)))

(defun active-mode-suggestion-filter (buffers)
  "Return the union of the active modes in BUFFERS."
  (let ((modes (delete-duplicates (mapcar #'mode-name
                                          (alex:mappend #'modes buffers)))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) modes))))

(defun inactive-mode-suggestion-filter (buffers)
  "Return the list of all modes minus those present in all BUFFERS."
  (let ((all-non-minibuffer-modes
         (delete-if (lambda (m)
                      (closer-mop:subclassp (find-class m)
                                            (find-class 'nyxt/minibuffer-mode:minibuffer-mode)))
                    (mode-list)))
        (common-modes (reduce #'intersection
                              (mapcar (lambda (b)
                                        (mapcar #'mode-name (modes b)))
                                      buffers))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) (set-difference all-non-minibuffer-modes common-modes)))))

(define-command disable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Disable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Disable mode(s)"
                        :multi-selection-p t
                        :suggestion-function (active-mode-suggestion-filter buffers))))
    (dolist (buffer buffers)
      (disable-modes modes buffer))))

(define-command disable-mode-for-buffer ()
  "Disable queried mode(s) for select buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Disable mode(s) for buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (disable-mode-for-current-buffer :buffers buffers)))

(define-command enable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Enable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Enable mode(s)"
                        :multi-selection-p t
                        :suggestion-function (inactive-mode-suggestion-filter buffers))))
    (dolist (buffer buffers)
      (enable-modes modes buffer))))

(define-command enable-mode-for-buffer ()
  "Enable queried mode(s) for select buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Enable mode(s) for buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (enable-mode-for-current-buffer :buffers buffers)))

(define-command open-inspector ()
  "Open the inspector, a graphical tool to inspect and change the content of the buffer."
  (ffi-inspector-show (current-buffer)))
