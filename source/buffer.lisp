;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type keymaps-buffer (function (list-of-keymaps buffer)
                                                 (values &optional list-of-keymaps buffer)))
(export-always '(make-hook-keymaps-buffer make-handler-keymaps-buffer))
(hooks:define-hook-type url->url (function (quri:uri) quri:uri))

(define-class buffer ()
  ((id
    ""
    :documentation "Unique identifier for a buffer.
Dead buffers or placeholder buffers (i.e. those not associated with a web view)
have an empty ID.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (data-profile
    (make-instance (or (find-data-profile (getf *options* :data-profile))
                       'default-data-profile))
    :type data-profile
    :documentation "Profile to use for all persisted files.
See the `data-path' class and the `expand-path' function.")
   (document-model-delta-threshold
    10
    :documentation "Update the document model when the amount of elements on the
    page change greater than this amount."
    :export nil)
   (document-model
    nil
    :type (or null plump:node)
    :documentation "A parsed representation of the page currently opened.
Created from the page code with the help of `plump:parse'. See `update-document-model'.")
   (url (quri:uri ""))
   (url-at-point (quri:uri ""))
   (title "")
   (last-access
    (local-time:now)
    :export nil
    :documentation "Timestamp when the buffer was last switched to.")
   (modes
    '()
    :documentation "The list of mode instances.
Modes are instantiated over the result of the `default-modes' method, with
`initialize-modes' and not in the initform so that the instantiation form can
access the initialized buffer.")
   (enable-mode-hook
    (make-hook-mode)
    :type hook-mode
    :documentation "Hook run on every mode activation,
after the mode-specific hook.")
   (disable-mode-hook
    (make-hook-mode)
    :type hook-mode
    :documentation "Hook run on every mode deactivation,
after the mode-specific hook.")
   (keymap-scheme-name
    scheme:cua
    :documentation "The keymap scheme that will be used for all modes in the current buffer.")
   (search-auto-complete-p
    t
    :type boolean
    :documentation "Whether search suggestions are requested and displayed.")
   (search-always-auto-complete-p
    t
    :type boolean
    :documentation "Whether auto-completion works even for non-prefixed search.
Auto-completions come from the default search engine.")
   (search-engines
    (list (make-instance 'search-engine
                         :shortcut "wiki"
                         :search-url "https://en.wikipedia.org/w/index.php?search=~a"
                         :fallback-url (quri:uri "https://en.wikipedia.org/")
                         :completion-function
                         (make-search-completion-function
                          :base-url "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
                          :processing-function
                          #'(lambda (results)
                              (alex:when-let* ((results results)
                                               (results (decode-json results)))
                                (mapcar #'list (second results) (fourth results))))))
          (make-instance 'search-engine
                         :shortcut "ddg"
                         :search-url "https://duckduckgo.com/?q=~a"
                         :fallback-url (quri:uri "https://duckduckgo.com/")
                         :completion-function
                         (make-search-completion-function
                          :base-url "https://duckduckgo.com/ac/?q=~a"
                          :processing-function
                          #'(lambda (results)
                              (when results
                                (mapcar (lambda (hash-table)
                                          (first (alex:hash-table-values hash-table)))
                                        (decode-json results)))))))
    :type (cons search-engine *)
    :documentation "A list of the `search-engine' objects.
You can invoke them from the prompt-buffer by prefixing your query with SHORTCUT.
If the query is empty, FALLBACK-URL is loaded instead.  If
FALLBACK-URL is empty, SEARCH-URL is used on an empty search.

The default search engine (as per `default-search-engine') is used when the
query is not a valid URL, or the first keyword is not recognized.")
   (current-keymaps-hook
    (make-hook-keymaps-buffer
     :combination #'hooks:combine-composed-hook)
    :type hook-keymaps-buffer
    :documentation "Hook run as a return value of `current-keymaps'.")
   (conservative-word-move
    nil
    :documentation "If non-nil, the cursor moves to the end
(resp. beginning) of the word when `move-forward-word'
(resp. `move-backward-word') is called.")
   (override-map
    (let ((map (make-keymap "override-map")))
      (define-key map
        "C-space" 'execute-command))
    :documentation "Keymap that overrides all other bindings.

`override-map` takes priority over everything, including text insertion, and is
therefore better used with modifier-prefixed bindings.

No libraries should ever touch the override-map, this is left for the user to
customize to their needs.

Example:

\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                             (define-key map
                               \"M-x\" 'execute-command
                               \"C-q\" 'quit)
                   map))))")
   (forward-input-events-p
    t
    :documentation "When non-nil, keyboard events are
forwarded to the renderer when no binding is found.  Pointer
events (e.g. mouse events) are not affected by this, they are always
forwarded when no binding is found.")
   (last-event
    nil
    :type t
    :export nil
    ;; TODO: Store multiple events?  Maybe when implementing keyboard macros.
    :documentation "The last event received in the current buffer.")
   (pre-request-hook
    (make-hook-resource
     :combination #'combine-composed-hook-until-nil)
    :type hook-resource
    :documentation "Hook run before the `request-resource-hook'.
One example of its application is `auto-mode' that changes mode setup. Any
action on modes that can possibly change the handlers in `request-resource-hook'
should find its place there.")
   (request-resource-scheme
    (define-scheme "request-resource"
      scheme:cua
      (list
       "C-button1" 'request-resource-open-url-focus
       "button2" 'request-resource-open-url-focus
       "C-shift-button1" 'request-resource-open-url))
    :documentation "This keymap can be looked up when
`request-resource-hook' handlers run.
The functions are expected to take key arguments like `:url'.")
   (request-resource-hook
    (make-hook-resource
     :combination #'combine-composed-hook-until-nil)
    :type hook-resource
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
            :initial-value %slot-default%))))")
   (scroll-distance
    50
    :type integer
    :documentation "The distance scroll-down or scroll-up will scroll.")
   (smooth-scrolling
    nil
    :documentation "Whether to scroll smoothly with the mouse.")
   (horizontal-scroll-distance
    50
    :type integer
    :documentation "Horizontal scroll distance. The
distance scroll-left or scroll-right will scroll.")
   (current-zoom-ratio
    1.0
    :type float
    :documentation "The current zoom relative to the default zoom.")
   (zoom-ratio-step
    0.2
    :type float
    :documentation "The step size for zooming in and out.")
   (zoom-ratio-min
    0.2
    :type float
    :documentation "The minimum zoom ratio relative to the default.")
   (zoom-ratio-max
    5.0
    :type float
    :documentation "The maximum zoom ratio relative to the default.")
   (zoom-ratio-default
    1.0
    :type float
    :documentation "The default zoom ratio.")
   (page-scroll-ratio
    0.90
    :type float
    :documentation "The ratio of the page to scroll.
A value of 0.95 means that the bottom 5% will be the top 5% when scrolling
down.")
   (style
    (theme:themed-css
        (theme *browser*)
      (body
       :color theme:text
       :background-color theme:background
       :margin-left "20px"
       :margin-top "20px")
      ("h1,h2,h3,h4,h5,h6"
       :color theme:primary
       :font-family theme:font-family)
      (hr
       :height "3px"
       :border-radius "2px"
       :border-width "0"
       :color theme:secondary
       :background-color theme:secondary)
      (.button
       :display "inline-block"
       :background-color theme:secondary
       :color theme:background
       :text-decoration "none"
       :border-radius "2px"
       :padding "6px"
       :margin-left "2px"
       :margin-right "2px")
      (|.button:hover|
       :color theme:text)
      (|.button:visited|
       :color theme:background)
      (|.button:active|
       :color theme:background)
      (a
       :color theme:primary)
      (pre
       :color theme:text
       :background-color theme:quaternary
       :border-radius "2px"
       :padding-bottom "10px")))
   (buffer-load-hook
    (make-hook-url->url
     :combination #'hooks:combine-composed-hook)
    :type hook-url->url
    :accessor nil
    :export nil
    :documentation "Hook run in `buffer-load' before loading.
The handlers take the URL going to be loaded as argument
and must return a (possibly new) URL.")
   (buffer-loaded-hook
    (make-hook-buffer)
    :type hook-buffer
    :documentation "Hook run on `on-signal-load-finished'.
The handlers take the buffer as argument.")
   (buffer-delete-hook
    (make-hook-buffer)
    :type hook-buffer
    :documentation "Hook run before `buffer-delete' takes effect.
The handlers take the buffer as argument.")
   (password-interface
    (make-password-interface)
    :type (or null password::password-interface)
    :documentation "The current password interface.
See `password:*interfaces*' for the list of all currently registered interfaces.
To use, say, KeepassXC, set this slot to

  (make-instance 'password:user-keepassxc-interface)

Password interfaces may have user classes (that is, prefixed with 'user-' as in
the above example), in which case you can use `define-configuration' on them.")
   (download-path
    (make-instance 'download-data-path)
    :type data-path
    :documentation "Path of directory where downloads will be
stored.  Nil means use system default.
Downloads are kept in browser's `user-data', keyed by the expanded `download-path'.")
   (download-engine
    :initform :renderer
    :type symbol
    :documentation "Select a download engine to use,
such as :lisp or :renderer.")
   (history-path
    (make-instance 'history-data-path)
    :type data-path
    :documentation "
The path where the system will create/save the global history.
History data is kept in browser's `user-data', keyed by the expanded `history-path'.")
   (bookmarks-path
    (make-instance 'bookmarks-data-path)
    :type data-path
    :documentation "
The path where the system will create/save the bookmarks.
Bookmarks' data is kept in browser's `user-data', keyed by the expanded `bookmarks-path'.")
   (annotations-path
    (make-instance 'annotations-data-path)
    :type data-path
    :documentation "
The path where the system will create/save annotations.
Annotation' data is kept in browser's `user-data', keyed by the expanded
`annotations-path'.")
   (inputs-path
    (make-instance 'inputs-data-path)
    :type data-path
    :documentation "
The path where the system will create/save the input data.
Inputs' data is kept in browser's `user-data', keyed by the expanded `save-inputs-path'.")
   (auto-mode-rules-path
    (make-instance 'auto-mode-rules-data-path)
    :type data-path
    :documentation "The path where the auto-mode rules are saved.
Rules are kept in browser's `user-data', keyed by the expanded `auto-mode-rules-path'.")
   (standard-output-path
    (make-instance 'standard-output-data-path)
    :type data-path
    :documentation "Path where `*standard-output*' can be written to.")
   (error-output-path
    (make-instance 'error-output-data-path)
    :type data-path
    :documentation "Path where `*error-output*' can be written to."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A buffer is the fundamental unit of displayed content.
Buffers result from the computations of a web renderer, which generates a visual
representation of HTML documents.

Rendered URLs or the Nyxt's manual qualify as examples.  Buffers are fully
separated from one another, so that each has its own behaviour and settings."))

(define-user-class buffer)

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~a" (id buffer))))

(defvar %default-modes '(web-mode base-mode)
  "The default modes for unspecialized buffers.
This is useful when there is no current buffer.")

(export-always 'default-modes)
(defgeneric default-modes (buffer)
  (:method-combination append)
  (:method append ((buffer buffer))
    %default-modes)
  (:documentation "The symbols of the modes to instantiate on buffer creation.
The mode instances are stored in the `modes' BUFFER slot.

The default modes returned by this method are appended to the default modes
inherited from the superclasses."))

(defmethod default-modes :around ((buffer buffer))
  "Remove the duplicates from the `default-modes'."
  (remove-duplicates (call-next-method)
                     ;; Mode at the beginning of the list have higher priorities.
                     :from-end t))

(define-class web-buffer (user-buffer)
  ((status
    :unloaded
    :type (member :loading
                  :finished
                  :unloaded
                  :failed)
    :accessor nil
    :export nil ; TODO: Need to decide if we want progress / errors before exposing to the user.
    :documentation "The status of the buffer.
- `:loading' when loading a web resource.
- `:finished' when done loading a web resource.
- `:unloaded' for buffers that have not been loaded yet, like
  session-restored buffers, dead buffers or new buffers that haven't started the
  loading process yet.")
   (keywords
    nil
    :accessor nil
    :documentation "The keywords parsed from the current web buffer.")
   (keywords-document-model
    nil
    :export nil
    :documentation "The document model used to calculate the keywords.")
   (proxy
    nil
    :accessor nil
    :type (or proxy null)
    :documentation "Proxy for buffer.")
   (certificate-exceptions
    '()
    :type list-of-strings
    :documentation "A list of hostnames for which certificate errors shall be ignored.")
   (cookies-path
    (make-instance 'cookies-data-path)
    :type data-path
    :documentation "The path where cookies are stored.  Not all
renderers might support this.")
   (default-cookie-policy
    :no-third-party
    :type cookie-policy
    :documentation "Cookie policy of new buffers.
Must be one of `:always' (accept all cookies), `:never' (reject all cookies),
`:no-third-party' (accept cookies for current website only)."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-user-class web-buffer)

(defmethod default-modes append ((buffer web-buffer))
  '(certificate-exception-mode))

(defmethod initialize-instance :after ((buffer web-buffer) &key)
  (when (expand-path (cookies-path buffer))
    (ensure-parent-exists (expand-path (cookies-path buffer)))))

(define-class nosave-buffer (user-web-buffer)
  ((data-profile (make-instance 'nosave-data-profile)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-user-class nosave-buffer)

(defmethod default-modes append ((buffer nosave-buffer))
  '(certificate-exception-mode))

(define-class background-buffer (user-web-buffer)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A non-user-facing buffer to run background processes in.
Examples of the processes to run in background buffers are:
- WebExtensions background pages.
- Page scraping processes.
- Anything else requiring a renderer running invisible to the user.

These buffers are not referenced by `browser', so the only way to control these is to
store them somewhere and `ffi-buffer-delete' them once done."))

(define-user-class background-buffer)

(define-class internal-buffer (user-buffer)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-user-class internal-buffer)

(define-class panel-buffer (user-web-buffer)
  ((width 250 :documentation "The width in pixels.")
   (style (theme:themed-css (theme *browser*)
            (body
             :background-color theme:background
             :color theme:text
             :margin "0"
             :padding "10px"
             :border-style "solid"
             :border-width "0px 1px"
             :border-color theme:tertiary)
            ("h1,h2,h3,h4,h5,h6"
             :font-family theme:font-family
             :font-weight 500)
            (a
             :color theme:primary)
            (.button
             :display "inline-block"
             :background-color "darkgray"
             :color theme:background
             :text-decoration "none"
             :border-radius "2px"
             :padding "6px"
             :margin-left "2px"
             :margin-right "2px")
            (|.button:hover|
             :color theme:text)
            (|.button:visited|
             :color theme:background)
            (|.button:active|
             :color theme:background))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-user-class panel-buffer)

(define-class editor-buffer (internal-buffer)
  ((file :documentation "The file being edited.")
   (url (quri:uri "editor-buffer"))
   (title "editor-buffer"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Each editor buffer matches a file. Each editor buffer
  contains an editor mode instance."))

(defmethod default-modes append ((buffer editor-buffer))
  '(plaintext-editor-mode))
(defmethod default-modes :around ((buffer editor-buffer))
  ;; REVIEW: Really remove web-mode from editor-buffer?
  (remove 'web-mode (call-next-method)))

(define-user-class editor-buffer)

(defun make-dummy-buffer ()
  "Internal buffers are lighter than full-blown buffers which can have a
WebKit context, etc.
Delete it with `ffi-buffer-delete'."
  (make-instance 'user-internal-buffer))

(define-class status-buffer (user-internal-buffer)
  ((height
    24
    :type integer
    :documentation "The height of the status buffer in pixels.")
   (glyph-mode-presentation-p
    nil
    :documentation "Display the modes as a list of glyphs.")
   (style
    (theme:themed-css (theme *browser*)
      (body
       :color theme:text
       :background theme:tertiary
       :font-size "14px"
       :color theme:text
       :padding 0
       :margin 0
       :line-height "24px")
      (.loader
       :border-width "2px"
       :border-style "solid"
       :border-color "transparent"
       :border-top-color theme:accent
       :border-left-color theme:accent
       :border-radius "50%"
       :display "inline-block"
       :width "7px"
       :height "7px"
       :animation "spin 1s linear infinite")
      ("@keyframes spin"
       ("0%" :transform "rotate(0deg)")
       ("100%" :transform "rotate(360deg)"))
      (".arrow-right"
       :clip-path "polygon(0 0, calc(100% - 10px) 0, 100% 50%, calc(100% - 10px) 100%, 0 100%)"
       :margin-right "-10px")
      (".arrow-left"
       :clip-path "polygon(10px 0, 100% 0, 100% 100%, 10px 100%, 0% 50%)"
       :margin-left "-10px")
      ("#container"
       :display "grid"
       ;; Columns: controls, url, tabs, modes
       :grid-template-columns "90px minmax(auto, 30ch) 1fr 220px"
       :overflow-y "hidden")
      ("#container-vi"
       :display "grid"
       ;; Columns: controls, vi-status, url, tabs, modes
       :grid-template-columns "90px 30px minmax(auto, 30ch) 1fr 220px"
       :overflow-y "hidden")
      ("#controls"
       :font-size "16px"
       :font-weight "700"
       :background-color theme:primary
       :color theme:background
       :padding-left "5px"
       :overflow "hidden"
       :white-space "nowrap"
       :z-index "4")
      ("#vi-mode"
       :padding-right "10px"
       :padding-left "10px"
       :text-align "center"
       :z-index "3")
      (".vi-normal-mode"
       :color theme:text
       :background-color theme:secondary)
      (".vi-insert-mode"
       :color theme:text
       :background-color theme:accent)
      ("#url"
       :color theme:background
       :background-color theme:secondary
       :min-width "100px"
       :text-overflow "ellipsis"
       :overflow-x "hidden"
       :white-space "nowrap"
       :padding-right "10px"
       :padding-left "15px"
       :z-index "2")
      ("#tabs"
       :color theme:text
       :background-color theme:tertiary
       :min-width "100px"
       :white-space "nowrap"
       :overflow-x "scroll"
       :text-align "left"
       :padding-left "15px"
       :padding-right "10px"
       :z-index "1")
      ("#tabs::-webkit-scrollbar"
       :display "none")
      (.tab
       :color theme:background
       :white-space "nowrap"
       :text-decoration "none"
       :padding-left "5px"
       :padding-right "5px")
      (".tab:hover"
       :color theme:text)
      ("#modes"
       :background-color theme:secondary
       :color theme:background
       :text-align "right"
       :padding-left "10px"
       :padding-right "5px"
       :overflow-x "scroll"
       :white-space "nowrap"
       :z-index "2")
      ("#modes::-webkit-scrollbar"
       :display "none")
      (.button
       :color theme:background
       :text-decoration "none"
       :padding-left "2px"
       :padding-right "2px"
       :margin-left "2px"
       :margin-right "2px")
      (|.button:hover|
       :color theme:text))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-user-class status-buffer)

(define-command update-document-model (&key (buffer (current-buffer)))
  "Update the buffer's `dom' with the page source augmented with Nyxt identifiers."
  (ffi-buffer-evaluate-javascript
   buffer
   (ps:ps
    (defvar nyxt-identifier-counter 0)
    (defun add-nyxt-identifiers (node)
      (unless (ps:chain node (has-attribute "nyxt-identifier"))
        (ps:chain node (set-attribute "nyxt-identifier" (ps:stringify nyxt-identifier-counter))))
      (incf nyxt-identifier-counter)
      (dolist (child (ps:chain node children))
        (add-nyxt-identifiers child))
      nyxt-identifier-counter)
    (setf nyxt-identifier-counter (add-nyxt-identifiers (ps:chain document body)))))
  (alex:when-let ((body-json (nyxt/dom::get-document-body-json)))
                 (setf (document-model buffer)
                       (nyxt/dom::named-json-parse body-json))))

(defun dead-buffer-p (buffer) ; TODO: Use this wherever needed.
  (str:empty? (id buffer)))

(defmethod document-model ((buffer buffer))
  (pflet ((%count-dom-elements
           ()
           (defvar dom-counter 0)
           (defun count-dom-elements (node)
             (incf dom-counter)
             (dolist (child (ps:chain node children))
               (count-dom-elements child))
             dom-counter)
           (setf dom-counter 0)
           (count-dom-elements (nyxt/ps:qs document "html"))))
         (if (dead-buffer-p buffer)
             (slot-value buffer 'document-model)
             (with-current-buffer buffer
               (let ((value (slot-value buffer 'document-model))
                     (element-count (%count-dom-elements)))
                 (if (and value element-count
                          ;; Check whether the difference in element count is significant.
                          (< (abs (- (length (clss:select "*" value)) (truncate element-count)))
                             (document-model-delta-threshold buffer)))
                     value
                     (update-document-model :buffer buffer)))))))

(export-always 'get-nyxt-id)
(defmethod get-nyxt-id ((element plump:element))
  (plump:get-attribute element "nyxt-identifier"))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (ffi-buffer-set-proxy buffer
                            (url proxy)
                            (allowlist proxy))
      (ffi-buffer-set-proxy buffer
                            (quri:uri "")
                            nil)))

(defmethod keywords ((buffer web-buffer))
  "Calculate the keywords for a given buffer."
  (ignore-errors
   (if (not (eq (document-model buffer)
                (keywords-document-model buffer)))
       (let ((contents (serapeum:string-join
                        (map 'list (lambda (e) (plump:text e))
                             (clss:select "p" (document-model buffer))) " ")))
         (setf (keywords-document-model buffer)
               (document-model buffer)
               (slot-value buffer 'keywords)
               (analysis:extract-keywords contents)))
       (slot-value buffer 'keywords))))

(-> proxy-adress (buffer &key (:downloads-only boolean)) *)
(defun proxy-url (buffer &key (downloads-only nil))
  "Return the proxy address, nil if not set.
If DOWNLOADS-ONLY is non-nil, then it only returns the proxy address (if any)
when `proxied-downloads-p' is true."
  (let* ((proxy (and buffer (proxy buffer)))
         (proxied-downloads (and proxy (proxied-downloads-p proxy))))
    (when (or (and (not downloads-only) proxy)
              proxied-downloads)
      (url proxy))))

(defmethod initialize-modes ((buffer buffer))
  "Initialize BUFFER modes.
This is called after BUFFER has been created by the renderer.
See `buffer-make'."
  (dolist (mode-symbol (reverse (default-modes buffer)))
    (make-mode mode-symbol buffer)))

(defun load-failed-p (buffer)
  "Only web-buffer loads can fail."
  (and (web-buffer-p buffer)
       (eq (slot-value buffer 'status) :failed)))

(export-always 'on-signal-notify-uri)
(defmethod on-signal-notify-uri ((buffer buffer) no-url)
  "Set BUFFER's `url' slot, then dispatch `on-signal-notify-uri' over the
BUFFER's modes."
  (declare (ignore no-url))
  (let ((view-url (ffi-buffer-url buffer)))
    (unless (or (load-failed-p buffer)
                (url-empty-p view-url))
      ;; When a buffer fails to load and `ffi-buffer-url' returns an empty
      ;; URL, we don't set (url buffer) to keep access to the old value.
      (setf (url buffer) (ffi-buffer-url buffer))))
  (dolist (mode (modes buffer))
    (on-signal-notify-uri mode (url buffer)))
  (url buffer))

(defmethod on-signal-notify-uri ((buffer internal-buffer) no-url)
  "Internal buffers don't load external resources and as such don't need URL
change notifications.
In particular, we don't want to register a URL in the history via the `web-mode'
notification."
  ;; TODO: We should not ignore this notification since it may be used by modes.
  ;; In particular, we receive a legit notify::uri when clicking on an anchor URL.
  (declare (ignore no-url))
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
  (dolist (mode (modes buffer))
    (on-signal-load-committed mode url)))

(export-always 'on-signal-load-redirected)
(defmethod on-signal-load-redirected ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-redirected mode url)))

(export-always 'on-signal-load-finished)
(defmethod on-signal-load-finished ((buffer buffer) url)
  (update-document-model :buffer buffer)
  (dolist (mode (modes buffer))
    (on-signal-load-finished mode url))
  (run-thread "buffer-loaded-hook" (hooks:run-hook (buffer-loaded-hook buffer) buffer)))

(hooks:define-hook-type buffer (function (buffer)))

(defmethod prompter:object-attributes ((buffer buffer))
  `(("URL" ,(render-url (url buffer)))
    ("Title" ,(title buffer))))

(defmethod prompter:object-attributes ((buffer web-buffer))
  `(("URL" ,(render-url (url buffer)))
    ("Title" ,(title buffer))
    ("Keywords" ,(lambda (buffer) (format nil "~:{~a~^ ~}" (keywords buffer))))))

(-> make-buffer
    (&key (:title string)
          (:modes (or null (cons symbol *)))
          (:url quri:uri)
          (:parent-buffer (or null buffer))
          (:no-history-p boolean)
          (:load-url-p boolean)
          (:buffer-class (or null symbol)))
    *)
(define-command make-buffer (&key (title "") modes (url (quri:uri "")) parent-buffer
                                  no-history-p (load-url-p t) buffer-class)
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is empty, the `default-new-buffer-url' browser slot is used instead.
To load nothing, set it to 'about:blank'.
PARENT-BUFFER is useful when we want to record buffer- and history relationships.
LOAD-URL-P controls whether to load URL right at buffer creation."
  (let* ((buffer (apply #'buffer-make *browser*
                        :title title
                        :extra-modes modes
                        :parent-buffer parent-buffer
                        :no-history-p no-history-p
                        (when buffer-class
                          (list :buffer-class buffer-class))))
         (url (if (url-empty-p url)
                  (default-new-buffer-url *browser*)
                  url)))
    (if load-url-p
        (buffer-load url :buffer buffer)
        (setf (url buffer) (quri:uri url)))
    buffer))

(-> make-nosave-buffer
    (&key (:title string)
          (:modes (or null (cons symbol *)))
          (:url quri:uri)
          (:load-url-p boolean))
    *)
(define-command make-nosave-buffer (&rest args
                                          &key title modes url load-url-p)
  "Create a new buffer that won't save anything to the filesystem.
See `make-buffer' for a description of the arguments."
  (declare (ignorable title modes url load-url-p))
  (apply #'make-buffer (append (list :buffer-class 'user-nosave-buffer) args)))

(-> make-buffer-focus
    (&key (:url quri:uri)
          (:parent-buffer (or null buffer))
          (:nosave-buffer-p boolean))
    *)
(define-command make-buffer-focus (&key (url (quri:uri "")) parent-buffer nosave-buffer-p)
  "Switch to a new buffer.
See `make-buffer'."
  (let ((buffer (if nosave-buffer-p
                    (make-nosave-buffer :url url)
                    (make-buffer :url url :parent-buffer parent-buffer))))
    (set-current-buffer buffer)
    buffer))

(declaim (ftype (function (&key (:title string)
                                (:modes (or null (cons symbol *)))
                                (:url quri:uri)))
                make-background-buffer))
(export-always 'make-background-buffer)
(defun make-background-buffer (&rest args &key title modes url)
  "Create a new web-aware buffer that won't be registered by the `browser'.
See `make-buffer' for a description of the arguments."
  (declare (ignorable title modes url))
  (apply #'make-buffer (append (list :buffer-class 'user-background-buffer :no-history-p t) args)))

(define-command make-internal-buffer (&key (url (quri:uri "")) (title "") modes no-history-p)
  "Create a new buffer.
MODES is a list of mode symbols.
URL is a URL to load into the buffer."
  (let ((buffer (buffer-make *browser* :title title
                                       :extra-modes modes
                                       :buffer-class 'user-internal-buffer
                                       :no-history-p no-history-p))
        (url (if (url-empty-p url)
                 (default-new-buffer-url *browser*)
                 url)))
    (buffer-load url :buffer buffer)))

(define-command make-editor-buffer (&key (title "") modes)
  "Create a new editor buffer."
  (buffer-make *browser* :title title
                         :extra-modes modes
                         :buffer-class 'user-editor-buffer))

(-> buffer-make
    (browser &key
             (:title string)
             (:data-profile data-profile)
             (:extra-modes list)
             (:dead-buffer buffer)
             (:nosave-buffer-p boolean)
             (:buffer-class symbol)
             (:parent-buffer buffer)
             (:no-history-p boolean))
    *)
(defun buffer-make (browser &key data-profile title extra-modes
                                 dead-buffer (buffer-class 'user-web-buffer)
                                 parent-buffer no-history-p
                                 (nosave-buffer-p (nosave-buffer-p parent-buffer)))
  "Make buffer with title TITLE and with EXTRA-MODES.
Run `*browser*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let ((buffer (if dead-buffer
                    (progn
                      ;; Dead buffer ID must be renewed before calling `ffi-buffer-make'.
                      (setf (id dead-buffer) (get-unique-identifier *browser*))
                      (ffi-buffer-make dead-buffer))
                    (apply #'make-instance buffer-class
                           :id (get-unique-identifier *browser*)
                           (append (when title `(:title ,title))
                                   (when data-profile `(:data-profile ,data-profile)))))))
    (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after the view has been created.
    (initialize-modes buffer)
    (mapc (alex:rcurry #'make-mode buffer) extra-modes)
    (when dead-buffer                   ; TODO: URL should be already set.  Useless?
      (setf (url buffer) (url dead-buffer)))
    ;; Background buffers are invisible to the browser.
    (unless (eq buffer-class 'user-background-buffer)
      (buffers-set (id buffer) buffer))
    (unless no-history-p
      ;; When we create buffer, current one can override the
      ;; data-profile of the created buffer. This is dangerous,
      ;; especially for nosave buffers.
      (with-current-buffer buffer
        ;; Register buffer in global history:
        (with-data-access (history (history-path buffer)
                                   :default (make-history-tree buffer))
          ;; Owner may already exist if history was just created with the above
          ;; default value.
          (unless (htree:owner history (id buffer))
            (htree:add-owner history (id buffer)
                             :creator-id (when (and parent-buffer
                                                    (not nosave-buffer-p)
                                                    (not (nosave-buffer-p parent-buffer)))
                                           (id parent-buffer)))))))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (hooks:run-hook (buffer-make-hook browser) buffer)
    buffer))

(-> add-to-recent-buffers (buffer) *)
(defun add-to-recent-buffers (buffer)
  "Create a recent-buffer from given buffer and add it to `recent-buffers'."
  ;; Make sure it's a dead buffer:
  (setf (id buffer) "")
  (containers:delete-item-if (recent-buffers *browser*) (buffer-match-predicate buffer))
  (containers:insert-item (recent-buffers *browser*) buffer))

(-> buffer-delete (buffer) *)
(defun buffer-delete (buffer)
  "For dummy buffers, use `ffi-buffer-delete' instead."
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (with-data-access (history (history-path buffer))
    (sera:and-let* ((owner (htree:owner history (id buffer)))
                    (current (htree:current owner))
                    (data (htree:data current)))
      (setf (nyxt::scroll-position data) (nyxt:document-scroll-position buffer))
      (htree:delete-owner history (id buffer))))
  (ffi-buffer-delete buffer))

(defun buffer-hide (buffer)
  "Stop showing the buffer in Nyxt.
Should be called from/instead of `ffi-buffer-delete' when the renderer view
associated to the buffer is already killed."
  (let ((parent-window (find buffer (window-list) :key 'active-buffer)))
    (when parent-window
      (let ((replacement-buffer (or (first (get-inactive-buffers))
                                    (make-buffer :load-url-p nil
                                                 :url (quri:uri "about:blank")))))
        (window-set-buffer parent-window replacement-buffer)))
    (buffers-delete (id buffer))
    ;; (setf (id buffer) "") ; TODO: Reset ID?
    (add-to-recent-buffers buffer)))

(export-always 'buffer-list)
(defun buffer-list ()
  "Order is stable."
  (sort
   (alex:hash-table-values (buffers *browser*))
   #'string>
   :key #'id))

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

(export-always 'window-set-buffer)
(defun window-set-buffer (window buffer &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (window-set-buffer-hook window) window buffer)
  ;; When not focusing, that is, when previewing we don't update the
  ;; `last-access' so as to not disturb the ordering.
  (when focus
    ;; The current buffer last-access time is set to now to ensure it becomes the
    ;; second newest buffer.  If we didn't update the access time, the buffer
    ;; last-access time could be older than, say, buffers opened in the
    ;; background.
    (setf (last-access (active-buffer window)) (local-time:now)))
  (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                       :key #'active-buffer)))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (make-dummy-buffer))
              (old-buffer (active-buffer window)))
          (log:debug "Swapping old buffer ~a with other window ~a to switch to ~a"
                     (render-url (url old-buffer))
                     (render-url (url (active-buffer window-with-same-buffer)))
                     (render-url (url buffer)))
          (ffi-window-set-buffer window-with-same-buffer temp-buffer)
          (ffi-window-set-buffer window buffer :focus focus)
          (setf (active-buffer window) buffer)
          (window-set-buffer window-with-same-buffer old-buffer)
          (ffi-buffer-delete temp-buffer))
        (progn
          (ffi-window-set-buffer window buffer :focus focus)
          (setf (active-buffer window) buffer)))
    (when focus
      (setf (last-access buffer) (local-time:now)))
    ;; So that `current-buffer' returns the new value if buffer was
    ;; switched inside a `with-current-buffer':
    (setf %buffer nil)
    (set-window-title window)
    (print-status nil window)
    (when (and (web-buffer-p buffer)
               (eq (slot-value buffer 'status) :unloaded))
      (reload-buffers (list buffer)))))

(defun last-active-buffer ()
  "Return buffer with most recent `last-access'."
  (first (sort-by-time (buffer-list))))

(defun get-inactive-buffers ()
  "Return inactive buffers sorted by last-access timestamp, or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer (window-list)))
        (buffers (buffer-list)))
    (alex:when-let ((diff (set-difference buffers active-buffers)))
                   ;; Display the most recent inactive buffer.
                   (sort-by-time diff))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (copy-to-clipboard (render-url (url (current-buffer))))
  (echo "~a copied to clipboard." (render-url (url (current-buffer)))))

(define-command copy-title ()
  "Save current page title to clipboard."
  (copy-to-clipboard (title (current-buffer)))
  (echo "~a copied to clipboard." (title (current-buffer))))

(defun buffer-initial-suggestions (&key current-is-last-p domain)
  (let ((buffers (sera:filter (if domain
                                  (match-domain domain)
                                  #'identity)
                              (sort-by-time (buffer-list)))))
    (when (and buffers current-is-last-p)
      (setf buffers (alex:rotate buffers -1)))
    buffers))

(define-class buffer-source (prompter:source)
  ((prompter:name "Buffer list")
   (prompter:constructor (buffer-initial-suggestions :current-is-last-p nil))
   (prompter:multi-selection-p t)
   (prompter:actions (list (make-unmapped-command set-current-buffer)
                           (make-mapped-command buffer-delete)))
   (prompter:follow-p t)
   (prompter:follow-delay 0.1)
   (prompter:follow-mode-functions #'(lambda (buffer)
                                       (set-current-buffer buffer :focus nil)))
   (prompter:destructor (let ((buffer (current-buffer)))
                          (lambda (prompter source)
                            (declare (ignore source))
                            (unless (or (prompter:returned-p prompter)
                                        (eq buffer (current-buffer)))
                              (set-current-buffer buffer))))))
  (:export-class-name-p t))
(define-user-class buffer-source)

(-> switch-buffer (&key (:id string) (:current-is-last-p boolean)) *)
(define-command switch-buffer (&key id (current-is-last-p nil))
  "Switch buffer using fuzzy completion to quickly find whatever buffer you are looking for.
Buffers are ordered by last access.
With CURRENT-IS-LAST-P, the current buffer is listed last so as to list the
second latest buffer first."
  (if id
      (set-current-buffer (buffers-get id))
      (prompt
       :prompt "Switch to buffer"
       :sources (list (make-instance 'user-buffer-source
                                     :constructor (buffer-initial-suggestions
                                                   :current-is-last-p current-is-last-p))))))

(define-command switch-buffer-domain (&key domain (buffer (current-buffer)))
  "Switch the active buffer in the current window from the current domain."
  (let ((domain (or domain (quri:uri-domain (url buffer)))))
    (prompt
     :prompt "Switch to buffer in current domain:"
     :sources (make-instance 'user-buffer-source
                             :constructor (sera:filter (match-domain domain)
                                                       (sort-by-time (buffer-list)))))))

(defun switch-buffer-or-query-domain (domain)
  "Switch to a buffer if it exists for a given DOMAIN, otherwise query
  the user."
  (let ((matching-buffers (sera:filter (match-domain domain) (buffer-list))))
    (if (eql 1 (length matching-buffers))
        (set-current-buffer (first matching-buffers))
        (switch-buffer-domain :domain domain))))

(define-command delete-buffer (&key id)
  "Query the buffer(s) to delete."
  (if id
      (buffer-delete (gethash id (slot-value *browser* 'buffers)))
      (prompt
       :prompt "Delete buffer(s)"
       :sources (make-instance 'user-buffer-source
                               :multi-selection-p t
                               :actions (list (make-mapped-command buffer-delete))))))

(define-internal-page-command reduce-to-buffer (&key (delete t))
    (reduced-buffer "*Reduced Buffers*" 'base-mode)
  "Query the buffer(s) to \"reduce \" by copying their titles/URLs to a
single buffer, optionally delete them. This function is useful for archiving a
set of useful URLs or preparing a list to send to a someone else."
  (let ((buffers (prompt
                  :prompt "Reduce buffer(s)"
                  :sources (make-instance 'user-buffer-source
                                          :actions '()
                                          :multi-selection-p t))))
    (spinneret:with-html-string
      (:style (style reduced-buffer))
      (:h1 "Reduced Buffers:")
      (:div
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
                (:hr ""))))))
    (when delete (mapcar #'buffer-delete buffers))))

(define-command delete-all-buffers (&key (confirmation-p t))
  "Delete all buffers, with confirmation."
  (let ((count (length (buffer-list))))
    (if confirmation-p
        (if-confirm ("Delete ~a buffer~p?" count count)
                    (mapcar #'buffer-delete (buffer-list)))
        (mapcar #'buffer-delete (buffer-list)))))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the current buffer, and make the next buffer the current one. If no
other buffers exist, set the url of the current buffer to the start page."
  (buffer-delete buffer))

(define-command delete-other-buffers (&optional (buffer (current-buffer)))
  "Delete all buffers except BUFFER.
When BUFFER is omitted, it defaults to the current one."
  (let* ((all-buffers (buffer-list))
         (buffers-to-delete (remove buffer all-buffers))
         (count (list-length buffers-to-delete)))
    (if-confirm ("Delete ~a buffer~p?" count count)
                (mapcar #'buffer-delete buffers-to-delete))))

(export-always 'buffer-load)
(declaim (ftype (function (url-designator &key (:buffer buffer)))
                buffer-load))
(defun buffer-load (url-designator &key (buffer (current-buffer)))
  "Load INPUT-URL in BUFFER.
URL is then transformed by BUFFER's `buffer-load-hook'."
  (let* ((url (url url-designator))
         (new-url
           (ignore-errors
            (handler-bind ((error (lambda (c) (log:error "In `buffer-load-hook': ~a" c))))
              (hooks:run-hook (slot-value buffer 'buffer-load-hook) url)))))
    (when new-url
      (check-type new-url quri:uri)
      (setf url new-url)
      ;; TODO: This condition can be a source of inefficiency.  Besides, it
      ;; partly duplicates the code in `preprocess-request'.  Can we factor this
      ;; out?
      (cond
        ((equal "javascript" (quri:uri-scheme url))
         (ffi-buffer-evaluate-javascript buffer (quri:url-decode (quri:uri-path url))))
        (t (ffi-buffer-load buffer url))))))

(define-class global-history-source (prompter:source)
  ((prompter:name "Global history")
   ;; REVIEW: Collect history suggestions asynchronously or not?  It's fast
   ;; enough with <10,000 entries on @ambrevar's laptop.
   ;; (prompter:initial-suggestions (history-initial-suggestions))
   (prompter:constructor (lambda (source)
                           (declare (ignorable source))
                           (history-initial-suggestions)))
   (prompter:multi-selection-p t)
   (prompter:filter-preprocessor nil)   ; Don't remove non-exact results.
   (prompter:actions '(buffer-load)))
  (:export-class-name-p t))
(define-user-class global-history-source)

(define-class new-url-query ()
  ((query ""
          :documentation "Either a URL or a string query passed to `engine'.")
   (label nil
          :type (or null string)
          :documentation "The meaningful text for the query, if query is a URL.")
   (engine nil
           :type (or null search-engine)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Structure holding the new URL query generated from a user
 string input.
If `engine' is set, `query' is passed to it.  See the `url' method.
If `query' is a valid URL, use it as is.
If it points to an existing file, 'file://' is automatically prepended to it on
instantiation.
If prefixing with 'https://' results in a valid URL, set `query' to this result
on instantiation.
Finally, if nothing else, set the `engine' to the `default-search-engine'."))

(defmethod initialize-instance :after ((query new-url-query)
                                       &key check-dns-p &allow-other-keys)
  ;; Trim whitespace, in particular to detect URL properly.
  (setf (query query) (str:trim (query query)))
  (cond
    ((engine query)
     ;; First check engine: if set, no need to change anything.
     nil)
    ((valid-url-p (query query)
                  :check-dns-p check-dns-p)
     ;; Valid URLs should be passed forward.
     nil)
    ;; Rest is for invalid URLs:
    ((uiop:file-exists-p (query query))
     (setf (query query)
           (str:concat
            "file://"
            (uiop:native-namestring
             (uiop:ensure-absolute-pathname
              (query query) *default-pathname-defaults*)))))
    ((and check-dns-p
          (valid-url-p (str:concat "https://" (query query))
                       :check-dns-p check-dns-p))
     (setf (query query)
           (str:concat "https://" (query query))))
    (t
     (setf (engine query)
           (or (engine query)
               (default-search-engine))))))

(defun encode-url-char (c)
  (if (find c '("+" "&" "%") :test #'string=)
      (quri:url-encode c)
      c))

(defmethod url ((query new-url-query))
  (quri:uri
   (cond
     ((and (engine query)
           (not (uiop:emptyp (query query))))
      (format nil (search-url (engine query))
              (str:join ""
                        (mapcar #'encode-url-char
                                (map 'list #'string (query query))))))
     ((engine query)
      (fallback-url (engine query)))
     (t (query query)))))

(defmethod prompter:object-attributes ((query new-url-query))
  `(("URL or new query" ,(or (label query) (query query)))
    ("Search engine?" ,(if (engine query) (shortcut (engine query)) ""))))

(defun make-completion-query (completion &key engine (check-dns-p t))
  (typecase completion
    (string (make-instance 'new-url-query
                           :engine      engine
                           :check-dns-p check-dns-p
                           :query completion))
    (list (make-instance 'new-url-query
                         :check-dns-p check-dns-p
                         :query (second completion)
                         :label (first completion)))))

(defun input->queries (input &key (check-dns-p t)
                               (engine-completion-p))
  (let* ((terms (sera:tokens input))
         (engines (let ((all-prefixed-engines
                          (remove-if
                           (sera:partial (complement #'str:starts-with-p) (first terms))
                           (all-search-engines)
                           :key #'shortcut)))
                    (multiple-value-bind (matches non-matches)
                        (sera:partition (lambda (e)
                                          (string= (first terms) e))
                                        all-prefixed-engines :key #'shortcut)
                      (append matches non-matches)))))
    (append (unless (and engines (member (first terms)
                                         (mapcar #'shortcut engines)
                                         :test #'string=))
              (list (make-instance 'new-url-query
                                   :query       input
                                   :check-dns-p check-dns-p)))
            (or (alex:mappend (lambda (engine)
                                (append
                                 (list (make-instance 'new-url-query
                                                      :query       (str:join " " (rest terms))
                                                      :engine      engine
                                                      :check-dns-p check-dns-p))
                                 ;; Some engines (I'm looking at you, Wikipedia!)
                                 ;; return garbage in response to an empty request.
                                 (when (and engine-completion-p
                                            (search-auto-complete-p (current-buffer))
                                            (completion-function engine)
                                            (rest terms))
                                   (mapcar (alex:rcurry #'make-completion-query
                                                        :engine      engine
                                                        :check-dns-p check-dns-p)
                                           (with-protect ("Error while completing search: ~a" :condition)
                                             (funcall (completion-function engine)
                                                      (str:join " " (rest terms))))))))
                              engines)
                (sera:and-let* ((completion engine-completion-p)
                                (buffer (current-buffer))
                                (complete (search-auto-complete-p buffer))
                                (always-complete (search-always-auto-complete-p buffer))
                                (engine (default-search-engine))
                                (completion (completion-function engine))
                                (all-terms (str:join " " terms)))
                  (mapcar (alex:rcurry #'make-completion-query
                                       :engine      engine
                                       :check-dns-p check-dns-p)
                          (funcall (completion-function engine) all-terms)))))))

(define-class new-url-or-search-source (prompter:source)
  ((prompter:name "New URL or search query")
   (prompter:filter-preprocessor
    (lambda (suggestions source input)
      (declare (ignore suggestions source))
      (input->queries input
                      :check-dns-p nil
                      :engine-completion-p nil)))
   (prompter:filter nil)
   (prompter:filter-postprocessor
    (lambda (suggestions source input)
      (declare (ignore suggestions source))
      (input->queries input
                      :check-dns-p t
                      :engine-completion-p t)))
   (prompter:actions '(buffer-load)))
  (:export-class-name-p t)
  (:documentation "This prompter source tries to \"do the right thing\" to
generate a new URL query from user input.
- If the query is a URL, open it directly.
- If it's a file, prefix the query with 'file://'.
- If it's a search engine shortcut, include it in the suggestions.
- If it's none of the above, use the `default-search-engine'.

It runs in two passes.  The first pass does not check the DNS for domain
validity, nor does it return any search engine suggestions.  This guarantees
that a good-enough default suggestion is showed instantaneously.
(We really want this prompter source to be fast!)  The second pass checks the
DNS to precisely validate domains and returns the search engines suggestions, if
any."))
(define-user-class new-url-or-search-source)

(defun pushnew-url-history (history url)
  "URL is not pushed if empty."
  (when (and history (not (url-empty-p url)))
    (prompter::history-pushnew history (render-url url))))

(define-command set-url (&key (prefill-current-url-p t))
  "Set the URL for the current buffer, completing with history."
  (let ((history (set-url-history *browser*))
        (actions (list (make-command buffer-load* (suggestion-values)
                                     "Load first selected URL in current buffer and the rest in new buffer(s)."
                                     (mapc (lambda (suggestion) (make-buffer :url (url suggestion))) (rest suggestion-values))
                                     (buffer-load (url (first suggestion-values))))
                       (make-command new-buffer-load (suggestion-values)
                                     "Load URL(s) in new buffer(s)."
                                     (mapc (lambda (suggestion) (make-buffer :url (url suggestion))) (rest suggestion-values))
                                     (make-buffer-focus :url (url (first suggestion-values)))))))
    (pushnew-url-history history (url (current-buffer)))
    (prompt
     :prompt "Open URL"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :history history
     :sources (list (make-instance 'user-new-url-or-search-source :actions actions)
                    (make-instance 'user-global-history-source :actions actions)
                    (make-instance 'bookmark-source :actions actions)
                    (make-instance 'search-engine-url-source :actions actions)))))

(define-command set-url-new-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused buffer."
  (let ((history (set-url-history *browser*))
        (actions (list (make-command new-buffer-load (suggestion-values)
                                     "Load URL(s) in new buffer(s)"
                                     (mapc (lambda (suggestion) (make-buffer :url (url suggestion)))
                                           (rest suggestion-values))
                                     (make-buffer-focus :url (url (first suggestion-values)))))))
    (pushnew-url-history history (url (current-buffer)))
    (prompt
     :prompt "Open URL in new buffer"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :history history
     :sources (list (make-instance 'user-new-url-or-search-source :actions actions)
                    (make-instance 'user-global-history-source :actions actions)
                    (make-instance 'bookmark-source :actions actions)
                    (make-instance 'search-engine-url-source :actions actions)))))

(define-command set-url-new-nosave-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused nosave buffer."
  (let ((actions
          (list (make-command new-nosave-buffer-load (suggestion-values)
                              "Load URL(s) in new nosave buffer(s)"
                              (mapc (lambda (suggestion) (make-nosave-buffer :url (url suggestion)))
                                    (rest suggestion-values))
                              (make-buffer-focus :url (url (first suggestion-values))
                                                 :nosave-buffer-p t)))))
    (prompt
     :prompt "Open URL in new nosave buffer"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :sources (list (make-instance 'user-new-url-or-search-source :actions actions)
                    (make-instance 'user-global-history-source :actions actions)
                    (make-instance 'bookmark-source :actions actions)
                    (make-instance 'search-engine-url-source :actions actions)))))

(define-command reload-current-buffer ()
  "Reload current buffer."
  (reload-buffers (list (current-buffer))))

(define-command reload-buffers (&optional buffers)
  "Reload queried buffer(s)."
  (if buffers
      (mapcar (lambda (buffer) (buffer-load (url buffer) :buffer buffer)) buffers)
      (prompt
       :prompt "Reload buffer(s)"
       :sources (make-instance 'user-buffer-source
                               :multi-selection-p t
                               :actions (list 'reload-buffers)))))

(defun buffer-parent (&optional (buffer (current-buffer)))
  (with-data-unsafe (history (history-path buffer))
    (sera:and-let* ((owner (htree:owner history (id buffer)))
                    (parent-id (htree:creator-id owner)))
                   (gethash parent-id (buffers *browser*)))))

(defun buffers-with-history (history)
  "Return the list of buffers that have history HISTORY.
HISTORY may be NIL for buffers without history."
  (remove-if (complement (sera:eqs history))
             (buffer-list)
             :key (alex:compose #'get-data #'history-path)))

(defun buffer-children (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (let* ((current-history (get-data (history-path buffer)))
           (buffers (buffers-with-history current-history)))
      (with-data-unsafe (history (history-path buffer))
        (sort (sera:filter
               (sera:equals (id buffer))
               buffers
               :key (lambda (b) (alex:when-let ((owner (htree:owner history (id b))))
                                  (htree:creator-id owner))))
              #'string< :key #'id)))))

(defun buffer-siblings (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (let* ((current-history (get-data (history-path buffer)))
           (buffers (buffers-with-history current-history)))
      (with-data-unsafe (history (history-path buffer))
        (flet ((existing-creator-id (owner)
                 "If owner's creator does not exist anymore
(i.e. parent has been deleted), return NIL so has mimick top-level owners."
                 (if (htree:owner history (htree:creator-id owner))
                     (htree:creator-id owner)
                     nil)))
          (let* ((owner (htree:owner history (id buffer)))
                 (current-parent-id (when owner (existing-creator-id owner)))
                 (common-parent-buffers
                   (sera:filter
                    (sera:equals current-parent-id)
                    buffers
                    :key (lambda (b)
                           (alex:when-let ((owner (htree:owner history (id b))))
                             (existing-creator-id owner)))))
                 (common-parent-buffers
                   (sort common-parent-buffers #'string< :key #'id)))
            (sera:split-sequence-if (sera:equals (id buffer))
                                    common-parent-buffers
                                    :key #'id)))))))

(define-command switch-buffer-previous (&optional (buffer (current-buffer)))
  "Switch to the previous buffer in the buffer tree.
The tree is browsed in a depth-first fashion.
When there is no previous buffer, go to the last one so as to cycle."
  (labels ((buffer-last-child (&optional (buffer (current-buffer)))
             (alex:if-let ((next-siblings (second (buffer-siblings buffer))))
                          (buffer-last-child (alex:last-elt next-siblings))
                          (alex:if-let ((children (buffer-children buffer)))
                                       (buffer-last-child (alex:last-elt children))
                                       buffer)))
           (buffer-sibling-previous (&optional (buffer (current-buffer)))
             (alex:when-let ((previous-siblings (first (buffer-siblings buffer))))
                            (alex:last-elt previous-siblings))))
    (alex:when-let ((previous (or (alex:when-let ((previous-sibling (buffer-sibling-previous buffer)))
                                                 (alex:if-let ((children (buffer-children previous-sibling)))
                                                              (buffer-last-child (first children))
                                                              previous-sibling))
                                  (buffer-parent buffer)
                                  (buffer-last-child buffer))))
                   (set-current-buffer previous))))

(define-command switch-buffer-next (&optional (buffer (current-buffer)))
  "Switch to the next buffer in the buffer tree.
The tree is browsed in a depth-first fashion.
When there is no next buffer, go to the first one so as to cycle."
  (labels ((buffer-first-root (buffer)
             (alex:if-let ((parent (buffer-parent buffer)))
                          (buffer-first-root parent)
                          (first (first (buffer-siblings buffer)))))
           (buffer-next-parent-sibling (buffer)
             (alex:when-let ((parent (buffer-parent buffer)))
                            (alex:if-let ((next-siblings (second (buffer-siblings parent))))
                                         (first next-siblings)
                                         (buffer-next-parent-sibling parent))))
           (buffer-sibling-next (&optional (buffer (current-buffer)))
             (first (second (buffer-siblings buffer)))))
    (alex:when-let ((next (or (first (buffer-children buffer))
                              (buffer-sibling-next buffer)
                              (buffer-next-parent-sibling buffer)
                              (buffer-first-root buffer))))
                   (set-current-buffer next))))

(define-command switch-buffer-last ()
  "Switch to the last showing buffer in the list of buffers.
That is to say, the one with the most recent access time after the current buffer."
  (let* ((buffers (sort-by-time (buffer-list))))
    (when (second buffers)
      (set-current-buffer (second buffers)))))

(export-always 'mode-name)
(defun mode-name (mode)
  "Return the full MODE symbol (with package prefix).
For user modes, return the parent mode.
If MODE does not exist, return nil."
  (cond
    ((eq mode 'root-mode)
     mode)
    ((symbolp mode)
     (alex:when-let ((command (mode-command mode)))
                    (name command)))
    (t
     (mode-name (sera:class-name-of mode)))))

(export-always 'disable-modes)
(defun disable-modes (modes &optional (buffer (current-buffer)))
  "Disable MODES for BUFFER.
MODES should be a list symbols, each possibly returned by `mode-name'."
  (dolist (mode (uiop:ensure-list modes))
    (let ((command (mode-command mode)))
      (if command
          (funcall command :buffer buffer :activate nil)
          (log:warn "Mode command ~a not found." mode)))))

(export-always 'enable-modes)
(defun enable-modes (modes &optional (buffer (current-buffer)) args)
  "Enable MODES for BUFFER.
MODES should be a list of symbols, each possibly returned by `mode-name'.
ARGS are passed to the mode command."
  (dolist (mode (uiop:ensure-list modes))
    (let ((command (mode-command mode)))
      (if command
          (apply #'funcall command :buffer buffer :activate t args)
          (log:warn "Mode command ~a not found." mode)))))

(define-class active-mode-source (prompter:source)
  ((prompter:name "Active modes")
   (buffers :initarg :buffers :accessor buffers :initform nil)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (delete-duplicates
                            (alex:mappend
                             #'modes
                             (uiop:ensure-list (buffers source)))
                            :test (lambda (i y) (equal (mode-name i)
                                                       (mode-name y)))))))
  (:export-class-name-p t))
(define-user-class active-mode-source)

(define-class inactive-mode-source (prompter:source)
  ((prompter:name "Inactive modes")
   (buffers :initarg :buffers :accessor buffers :initform nil)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (let ((common-modes
                                   (reduce #'intersection
                                           (mapcar (lambda (b)
                                                     (mapcar #'mode-name (modes b)))
                                                   (uiop:ensure-list (buffers source))))))
                             (set-difference (mode-list) common-modes)))))
  (:export-class-name-p t))
(define-user-class inactive-mode-source)

(define-command disable-mode ()
  "Disable queried mode(s) for select buffer(s)."
  (let* ((buffers (prompt
                   :prompt "Disable mode(s) for buffer(s)"
                   :sources (make-instance 'user-buffer-source
                                           :multi-selection-p t
                                           :actions '())))
         (modes (prompt
                 :prompt "Disable mode(s)"
                 :sources (make-instance 'user-active-mode-source
                                         :buffers buffers))))
    (loop for buffer in buffers
          do (disable-modes (mapcar #'mode-name modes) buffer))))

(define-command enable-mode ()
  "Enable queried mode(s) for select buffer(s)."
  (let* ((buffers (prompt
                   :prompt "Enable mode(s) for buffer(s)"
                   :sources (make-instance 'user-buffer-source
                                           :multi-selection-p t
                                           :actions '())))
         (modes (prompt
                 :prompt "Enable mode(s)"
                 :sources (make-instance 'user-inactive-mode-source
                                         :buffers buffers))))
    (loop for buffer in buffers
          do (enable-modes (uiop:ensure-list modes) buffer))))

(defun all-mode-names ()
  (mapcar #'name (sera:filter #'mode-toggler-p (list-commands))))

(defun make-mode-suggestion (mode &optional source input)
  "Return a `suggestion' wrapping around ATTRIBUTE. "
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value mode
                 :attributes `(("Mode" ,(string-downcase (symbol-name mode)))
                               ("Documentation" ,(or (first (sera:lines (documentation mode 'function)))
                                                     "")))))

(define-class mode-source (prompter:source)
  ((prompter:name "Modes")
   (prompter:multi-selection-p t)
   (prompter:constructor (sort (all-mode-names) #'string< :key #'symbol-name))
   (prompter:suggestion-maker 'make-mode-suggestion))
  (:export-class-name-p t))
(define-user-class mode-source)

(define-command toggle-modes (&key (buffer (current-buffer)))
  "Enable marked modes, disable unmarked modes for BUFFER."
  (let* ((modes-to-enable (prompt
                           :prompt "Mark modes to enable, unmark to disable"
                           :sources (make-instance 'user-mode-source
                                                   :actions (list 'identity
                                                                  (make-command force-disable-auto-mode (modes)
                                                                                "Return selection but force disabling auto-mode.
This is convenient when you use auto-mode by default and you want to toggle a
mode permanently for this buffer."
                                                                                (delete (read-from-string "nyxt/auto-mode:auto-mode" )
                                                                                        modes)))
                                                   :marks (mapcar #'mode-name (modes buffer)))))
         (modes-to-disable (set-difference (all-mode-names) modes-to-enable
                                           :test #'string=)))
    (disable-modes (uiop:ensure-list modes-to-disable) buffer)
    (enable-modes (uiop:ensure-list modes-to-enable) buffer)))

(define-command open-inspector ()
  "Open the inspector, a graphical tool to inspect and change the content of the buffer."
  (ffi-inspector-show (current-buffer)))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (pflet ((print-buffer () (print)))
         (print-buffer)))
