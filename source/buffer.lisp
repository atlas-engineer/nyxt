;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type keymaps-buffer (function ((list-of keymaps:keymap) buffer)
                                                 (values &optional (list-of keymaps:keymap) buffer)))
(export-always '(hook-keymaps-buffer))
(hooks:define-hook-type url->url (function (quri:uri) quri:uri))

(export-always 'renderer-buffer)
(defclass renderer-buffer ()
  ()
  (:metaclass interface-class))

(defvar %default-modes '(base-mode)
  "The default modes for unspecialized buffers.
This is useful when there is no current buffer.")

(define-class buffer (renderer-buffer)
  ((default-modes
    %default-modes
    :accessor nil
    :type (list-of symbol)
    :documentation "The symbols of the modes to instantiate on buffer creation.
The mode instances are stored in the `modes' BUFFER slot.

The default modes returned by this method are appended to the default modes
inherited from the superclasses.")
   (id
    (new-id)
    :type unsigned-byte
    :documentation "Unique identifier for a buffer.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (profile
    *global-profile*
    :type nyxt-profile
    :documentation "Buffer profiles are used to specialize the behavior of
various parts, such as the path of all data files.")
   (url (quri:uri ""))
   (url-at-point (quri:uri ""))
   (title "")

   (style (theme:themed-css (theme *browser*)
            (body
             :background-color theme:background
             :color theme:on-background
             :margin-left "20px"
             :margin-top "20px")
            ("h1,h2,h3,h4,h5,h6"
             :color theme:primary
             :font-family theme:font-family)
            (hr
             :background-color theme:secondary
             :color theme:on-secondary
             :height "3px"
             :border-radius "2px"
             :border-width "0")
            (button
             :background "transparent"
             :color "inherit"
             :border "none"
             :padding 0
             :font "inherit"
             :outline "inherit")
            (.button
             :background-color theme:primary
             :color theme:on-primary
             :display "inline-block"
             :text-decoration "none"
             :border-radius "2px"
             :padding "6px"
             :margin "2px")
            (.link
             :all "unset"
             :text-decoration "underline"
             :display "inline"
             :color theme:primary)
            (".link:hover"
             :opacity 0.8)
            (.accent
             :color theme:accent)
            (|.button:hover|
             :opacity 0.8)
            (|.button:visited|
             :color theme:background)
            (|.button:active|
             :color theme:background)
            (a
             :color theme:primary)
            ("a:hover"
             :opacity 0.8)
            (pre
             :overflow "auto"
             :color theme:on-background
             :background-color theme:secondary
             :border-radius "2px"
             :padding "5px")
            ("table, th, td"
             :border-color theme:secondary
             :border-collapse "collapse"
             :border-width "1px"
             :border-style "solid"
             :background-color theme:background
             :color theme:on-background)
            (th
             :background-color theme:primary
             :color theme:on-primary
             :text-align "left")
            (dt
             :font-weight "bold")
            ("::selection"
             :color theme:on-accent
             :background-color theme:accent)))
   (buffer-delete-hook                  ; TODO: Should we move this to `context-buffer'?
    (make-instance 'hook-buffer)
    :type hook-buffer
    :documentation "Hook run before `buffer-delete' takes effect.
The handlers take the buffer as argument."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "A buffer is the fundamental unit of displayed content.
Buffers result from the computations of a web renderer, which generates a visual
representation of HTML documents.

Rendered URLs or the Nyxt's manual qualify as examples.  Buffers are fully
separated from one another, so that each has its own behavior and settings."))

(defmethod request-resource-hook ((buffer buffer))
  "A method to not error out if the buffer has no `request-resource-hook'.

Useful in FFI functions where we usually specialize things against
`renderer-buffer', not knowing the exact class of those."
  nil)

(defmethod initialize-instance :after ((buffer buffer) &key
                                       &allow-other-keys)
  "Dummy method to allow forwarding other key arguments."
  buffer)

(defmethod finalize-buffer ((buffer buffer) &key (browser *browser*) &allow-other-keys)
  "Finalize instantiation of BUFFER.
Nothing to do for the simplest `buffer' type."
  (declare (ignore browser))
  t)

(define-class modable-buffer (buffer)
  ((modes
    '()
    :writer t
    :export t
    :reader nil
    :documentation "The list of mode instances.
Modes are instantiated over the result of the `default-modes' method, with
`finalize-buffer' and not in the initform so that the instantiation form can
access the initialized buffer.")
   (enable-mode-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "Hook run on mode enabling, after the mode-specific hook.")
   (disable-mode-hook
    (make-instance 'hook-mode)
    :type hook-mode
    :documentation "Hook run on mode disabling, after the mode-specific hook."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "A buffer whose behavior can be modified with `mode's."))

(defmethod finalize-buffer ((buffer modable-buffer) &key (browser *browser*) no-hook-p extra-modes)
  "Finalize instantiation of modable BUFFER.
In particular,
- run `buffer-make-hook';
- `enable' the modes from the `modes' slot, the `default-modes' and the EXTRA-MODES,
- run `buffer-after-make-hook'.
This method should be called by the renderer after instantiating the web view
of BUFFER."
  (unless no-hook-p
    (hooks:run-hook (buffer-make-hook browser) buffer))
  (mapc #'enable (slot-value buffer 'modes))
  (enable-modes (append (reverse (default-modes buffer))
                        (uiop:ensure-list extra-modes))
                buffer)
  (unless no-hook-p
    (hooks:run-hook (buffer-after-make-hook browser) buffer)))

(defmethod modes ((buffer buffer))
  "Non-modable buffers never have modes.
This specialization is useful to be able to call the method regardless of the
buffer, with a meaningful result."
  '())

(defmethod modes ((buffer modable-buffer))
  "Only return enabled modes.
To access all modes, including disabled ones, use `slot-value'."
  (sera:filter #'enabled-p (slot-value buffer 'modes)))

(define-class input-buffer (buffer)
  ((keyscheme
    keyscheme:cua
    :documentation "The keyscheme that will be used for all modes in the current buffer.")
   (current-keymaps-hook
    (make-instance 'hook-keymaps-buffer
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

`override-map' takes priority over everything, including text insertion, and is
therefore better used with modifier-prefixed bindings.

No libraries should ever touch the override-map, this is left for the user to
customize to their needs.

Example:

\(defmethod customize-instance ((buffer buffer) &key)
  (setf (override-map buffer)
        (let ((map (make-keymap \"override-map\")))
          (define-key map
            \"M-x\" 'execute-command
            \"C-q\" 'quit)
          map)))")
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
   (lisp-url-callbacks
    (sera:dict)
    :type hash-table
    :export nil
    :documentation "The index of callbacks for `lisp://' URLs.
They are populated by the `nyxt/ps:lisp-eval' Parenscript macro.

It's part of `input-buffer' since any (even offline) buffer that can be clicked
on may want to have dynamic interactions."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "A buffer in which the user can input."))

(define-class document-buffer (buffer)
  ((document-model-delta-threshold
    10
    :documentation "The `document-model' is updated when the changed elements
exceed this amount."
    :export nil)
   (document-model
    nil
    :reader nil                         ; We use a custom reader.
    :writer t
    :export t
    :type (or null plump:node)
    :documentation "A parsed representation of the rendered buffer.
Computed by `plump:parse', see `update-document-model' for details.")
   (search-auto-complete-p
    t
    :type boolean
    :documentation "Whether search suggestions are requested and displayed.")
   (search-always-auto-complete-p
    t
    :type boolean
    :documentation "Whether auto-completion acts on non-prefixed searches.
Suggestions are computed by the default search engine.")
   (keep-search-hints-p
    t
    :type boolean
    :documentation "Whether to keep search hints when the search prompt for the
`search-buffer' command is closed.")
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
    :documentation "Horizontal scroll distance. The distance scroll-left or
scroll-right will scroll.")
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
down."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Buffers in which hold structured documents.
The user can scroll them, zoom, etc."))

(define-class context-buffer (buffer)
  ((last-access
    (local-time:now)
    :export nil
    :documentation "Timestamp when the buffer was last switched to.")
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
   (download-directory
    (make-instance 'download-directory)
    :type download-directory
    :documentation "Directory where downloads will be stored.")
   (download-engine
    :initform :renderer
    :type symbol
    :documentation "Select a download engine to use, such as `:lisp' or
`:renderer'.")
   (history-file
    (history-file *browser*)
    :type history-file
    :documentation "File where to save the global history used by this buffer.
See also `history-file' in `browser' for the global history restored on startup,
which is not necessarily the same."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "A buffer which focus sets the current context, that is, the
buffer-specific values of various settings like the various file paths, prompt
options, download options, etc.

Every setting that can be buffer-specific should be stored here; settings that
only make sense globally should be stored in `browser' instead.

It's similar to the \"private window\" in popular browser, but the scope here is
the buffer (which gives us more flexibility)."))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~a" (id buffer))))

(defmethod (setf url) :around (value (buffer document-buffer))
  (declare (ignore value))
  (call-next-method)
  (set-window-title))
(defmethod (setf title) :around (value (buffer document-buffer))
  (declare (ignore value))
  (call-next-method)
  (set-window-title))

(export-always 'default-modes)
(defgeneric default-modes (buffer)
  (:method-combination append)
  ;; TODO: Add a warning method when passing NIL to guard the current buffer not
  ;; bound errors?
  (:method append ((buffer t))
    %default-modes)
  (:method append ((buffer buffer))
    (slot-value buffer 'default-modes))
  (:method :around ((buffer buffer))
    "Remove the duplicates from the `default-modes'."
    (remove-duplicates (call-next-method)
                       ;; Modes at the beginning of the list have higher priority.
                       :from-end t))
  (:method append ((buffer context-buffer))
    (list
     ;; TODO: No need for `resolve-symbol' if we move `context-buffer'
     ;; declaration in a separate file, loaded after modes.
     (resolve-symbol :annotate-mode :mode)
     (resolve-symbol :bookmark-mode :mode)
     (resolve-symbol :history-mode :mode)
     (resolve-symbol :password-mode :mode)))
  (:method append ((buffer document-buffer))
    (list
     ;; TODO: No need for `resolve-symbol' if we move `document-buffer'
     ;; declaration in a separate file, loaded after modes.
     (resolve-symbol :hint-mode :mode)
     (resolve-symbol :document-mode :mode)
     (resolve-symbol :search-buffer-mode :mode)
     (resolve-symbol :autofill-mode :mode) ; TODO: Remove from default?
     (resolve-symbol :spell-check-mode :mode))))

(define-class network-buffer (buffer)
  ((status
    :unloaded
    :type (member :loading
                  :finished
                  :unloaded
                  :failed)
    :export nil ; TODO: Need to decide if we want progress / errors before exposing to the user.
    :documentation "The status of the buffer.
- `:loading' when loading a web resource.
- `:finished' when done loading a web resource.
- `:unloaded' for buffers that have not been loaded yet, like
  session-restored buffers, dead buffers or new buffers that haven't started the
  loading process yet.")
   (buffer-load-hook
    (make-instance 'hook-url->url
                   :combination #'hooks:combine-composed-hook)
    :type hook-url->url
    :accessor nil
    :export nil
    :documentation "Hook run in `buffer-load' before loading.
The handlers take the URL going to be loaded as argument and must return a
(possibly new) URL.")
   (buffer-loaded-hook
    (make-instance 'hook-buffer)
    :type hook-buffer
    :documentation "Hook run on `on-signal-load-finished'.
The handlers take the buffer as argument.")

   (pre-request-hook
    (make-instance 'hook-resource
                   :combination #'combine-composed-hook-until-nil)
    :type hook-resource
    :documentation "Hook run before the `request-resource-hook'.
One example of its application is `auto-mode' that changes mode setup. Any
action on modes that can possibly change the handlers in `request-resource-hook'
should find its place there.")
   (request-resource-keyscheme-map
    (define-keyscheme-map "request-resource" ()
      keyscheme:cua
      (list
       "C-button1" 'request-resource-open-url-focus
       "button2" 'request-resource-open-url-focus
       "C-shift-button1" 'request-resource-open-url))
    :documentation "Looked up when `request-resource-hook' handlers run.  The
keymap takes functions whose key arguments are `:url' and `:buffer'.")
   (request-resource-hook
    (make-instance 'hook-resource
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
If you need to access the URL before this request, inspect the document-mode history.

There's no more ability to pass the results to the renderer with :FORWARD.

Example:

\(defmethod configure-instance ((buffer buffer))
  (reduce #'hooks:add-hook
          '(old-reddit-handler auto-proxy-handler)
          :initial-value (request-resource-hook buffer)))")
   (proxy
    nil
    :accessor nil
    :type (or proxy null)
    :documentation "Proxy for buffer.")
   (certificate-exceptions
    '()
    :type (list-of string)
    :documentation "A list of hostnames for which certificate errors shall be ignored."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Buffers that must interact with resources over the network."))

(define-class web-buffer (context-buffer network-buffer modable-buffer document-buffer input-buffer)
  ((keywords
    nil
    :accessor nil
    :documentation "The keywords parsed from the current web buffer.")
   (keywords-document-model
    nil
    :export nil
    :documentation "The document model used to calculate the keywords."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Buffer for browsing the web."))

(define-class background-buffer (web-buffer)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "A non-user-facing buffer to run background processes in.
Examples of the processes to run in background buffers are:
- WebExtensions background pages.
- Page scraping processes.
- Anything else requiring a renderer running invisible to the user.

These buffers are not referenced by `browser', so the only way to control these is to
store them somewhere and `ffi-buffer-delete' them once done."))

(define-class nosave-buffer (web-buffer)
  ((profile (make-instance 'nosave-profile)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class)
  (:documentation "Like `web-buffer', but don't persist data to disk."))

(define-class panel-buffer (input-buffer modable-buffer document-buffer network-buffer)
  ((width 250 :documentation "The width in pixels.")
   (style (theme:themed-css (theme *browser*)
            (body
             :background-color theme:background
             :color theme:on-background
             :margin "0"
             :padding "10px"
             :border-style "solid"
             :border-width "0px 1px"
             :border-color theme:secondary)
            ("h1,h2,h3,h4,h5,h6"
             :font-family theme:font-family
             :font-weight 500)
            (a
             :color theme:primary)
            (button
             :background "transparent"
             :color "inherit"
             :border "none"
             :padding 0
             :font "inherit"
             :outline "inherit")
            (.button
             :background-color theme:primary
             :color theme:on-primary
             :display "inline-block"
             :text-decoration "none"
             :border-radius "2px"
             :padding "6px"
             :margin "2px")
            (|.button:hover|
             :opacity 0.8)
            (|.button:visited|
             :color theme:background)
            (|.button:active|
             :color theme:background))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(define-class status-buffer (input-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The `window' to which the status buffer is attached.")
   (height
    24
    :type integer
    :documentation "The height of the status buffer in pixels.")
   (glyph-mode-presentation-p
    nil
    :documentation "Display the modes as a list of glyphs.")
   (style (theme:themed-css (theme *browser*)
            (body
             :line-height "20px"
             :font-size "14px"
             :padding 0
             :margin 0)
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
             :display "flex"
             ;; Columns: controls, url, tabs, modes
             :justify-content "space-between"
             :overflow-y "hidden")
            ("#controls"
             :background-color theme:secondary
             :color theme:on-secondary
             ;; :font-size "16px"
             :font-weight "700"
             :padding-left "5px"
             :overflow "hidden"
             :white-space "nowrap"
             :z-index "3"
             :flex-basis "6em")
            ("#url"
             :background-color theme:primary
             :color theme:on-primary
             :min-width "100px"
             :text-overflow "ellipsis"
             :overflow-x "hidden"
             :white-space "nowrap"
             :padding-right "10px"
             :padding-left "15px"
             :z-index "2"
             :flex-grow "3"
             :flex-shrink "2"
             :flex-basis "10em")
            ("#tabs"
             :background-color theme:secondary
             :color theme:on-secondary
             :min-width "100px"
             :white-space "nowrap"
             :overflow-x "scroll"
             :text-align "left"
             :padding-left "15px"
             :padding-right "10px"
             :z-index "1"
             :flex-grow "10"
             :flex-shrink "4"
             :flex-basis "10em")
            ("#tabs::-webkit-scrollbar"
             :display "none")
            (.tab
             :color theme:background
             :white-space "nowrap"
             :text-decoration "none"
             :padding-left "5px"
             :padding-right "5px")
            (".tab:hover"
             :opacity 0.8)
            ("#modes"
             :background-color theme:primary
             :color theme:on-primary
             :text-align "right"
             :padding-left "10px"
             :padding-right "5px"
             :overflow-x "scroll"
             :white-space "nowrap"
             :z-index "2"
             :flex-grow "2"
             :flex-shrink "1"
             :flex-basis "10em")
            ("#modes::-webkit-scrollbar"
             :display "none")
            (button
             :background "transparent"
             :color "inherit"
             :text-decoration "transparent"
             :border "transparent"
             :padding 0
             :font "inherit"
             :outline "inherit")
            (|.button:hover|
             :opacity 0.8))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(defmethod customize-instance :after ((buffer buffer)
                                      &key (browser *browser*)
                                        no-hook-p
                                      &allow-other-keys)
  "Finalize buffer.
When NO-HOOK-P is nil, run `*browser*'s `buffer-before-make-hook'.
Return the created buffer."
  (let ((file-slot-names (remove-if (lambda (slot-name)
                                      (not (typep (slot-value buffer slot-name)
                                                  'nyxt-file)))
                                    (mopu:slot-names 'buffer))))
    (dolist (file-slot-name file-slot-names)
      (setf (files:profile (slot-value buffer file-slot-name))
            (profile buffer))))
  (unless (or no-hook-p
              (not browser))
    (hooks:run-hook (buffer-before-make-hook browser) buffer))
  ;; Background buffers are invisible to the browser.
  buffer)

(defmethod customize-instance :after ((buffer context-buffer)
                                      &key parent-buffer no-history-p
                                      &allow-other-keys)
  "Finalize buffer.
PARENT-BUFFER can we used to specify the parent in the history.
Return the created buffer."
  ;; Background buffers are invisible to the browser.
  (unless (background-buffer-p buffer)
    (buffers-set (id buffer) buffer))
  (unless no-history-p
    ;; Register buffer in global history:
    (files:with-file-content (history (history-file buffer)
                              :default (make-history-tree buffer))
      ;; Owner may already exist if history was just created with the above
      ;; default value.
      (unless (htree:owner history (id buffer))
        (htree:add-owner history (id buffer)
                         :creator-id (when (and parent-buffer
                                                (not (nosave-buffer-p buffer))
                                                (not (nosave-buffer-p parent-buffer)))
                                       (id parent-buffer))))))
  buffer)

(define-command update-document-model (&key (buffer (current-buffer)))
  "Update BUFFER's `document-model' with the page source augmented with Nyxt
identifiers."
  (ps-eval :buffer buffer
    (defvar nyxt-identifier-counter 0)
    (defun add-nyxt-identifiers (node)
      (unless (ps:chain node (has-attribute "nyxt-identifier"))
        (ps:chain node (set-attribute "nyxt-identifier"
                                      (ps:stringify nyxt-identifier-counter))))
      (incf nyxt-identifier-counter)
      (dolist (child (ps:chain node children)) (add-nyxt-identifiers child))
      nyxt-identifier-counter)
    (setf nyxt-identifier-counter (add-nyxt-identifiers (ps:chain document body))))
  (alex:when-let ((body-json (with-current-buffer buffer
                               (nyxt/dom::get-document-body-json))))
    (let ((dom (nyxt/dom::named-json-parse body-json)))
      (unless (uiop:emptyp (plump:text dom))
        (setf (document-model buffer) dom)))))

(defun dead-buffer-p (buffer)           ; TODO: Use this wherever needed.
  (not (buffers-get (id buffer))))

(-> resurrect-buffer (buffer) (values &optional buffer))
(defun resurrect-buffer (dead-buffer)
  ;; (setf (id dead-buffer) (new-id))      ; TODO: Shall we reset the ID?
  (ffi-buffer-make dead-buffer)
  dead-buffer)

(defmethod document-model ((buffer buffer))
  (ps-labels :buffer buffer
    ((%count-dom-elements
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
        (let ((value (slot-value buffer 'document-model))
              (element-count (%count-dom-elements)))
          (if (and value element-count
                   ;; Check whether the difference in element count is significant.
                   (< (abs (- (length (clss:select "*" value)) (truncate element-count)))
                      (document-model-delta-threshold buffer)))
              value
              (progn
                (update-document-model :buffer buffer)
                (slot-value buffer 'document-model)))))))

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (setf (ffi-buffer-proxy buffer)
            (list (url proxy)
                  (allowlist proxy)))
      (setf (ffi-buffer-proxy buffer)
            (quri:uri ""))))

(defmethod keywords ((buffer web-buffer))
  "Calculate the keywords for a given buffer."
  (if (not (eq (document-model buffer)
               (keywords-document-model buffer)))
      (let ((contents (serapeum:string-join
                       (map 'list (lambda (e) (plump:text e))
                            (clss:select "p" (document-model buffer))) " ")))
        (setf (keywords-document-model buffer)
              (document-model buffer)
              (slot-value buffer 'keywords)
              (ignore-errors (analysis:extract-keywords contents))))
      (slot-value buffer 'keywords)))

(define-class keyword-source (prompter:source)
  ((prompter:name "Keywords")
   (buffer
    (current-buffer)
    :type buffer)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (mapcar #'first (nyxt::keywords (buffer source))))))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t))

(-> proxy-url (buffer &key (:downloads-only boolean)) *)
(defun proxy-url (buffer &key (downloads-only nil))
  "Return the proxy address, nil if not set.
If DOWNLOADS-ONLY is non-nil, then it only returns the proxy address (if any)
when `proxied-downloads-p' is true."
  (let* ((proxy (and buffer (proxy buffer)))
         (proxied-downloads (and proxy (proxied-downloads-p proxy))))
    (when (or (and (not downloads-only) proxy)
              proxied-downloads)
      (url proxy))))

(defun load-failed-p (buffer)
  "Only `network-buffer' loads can fail."
  (and (network-buffer-p buffer)
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

(export-always 'on-signal-notify-title)
(defmethod on-signal-notify-title ((buffer buffer) no-title)
  "Set BUFFER's `title' slot, then dispatch `on-signal-notify-title' over the
BUFFER's modes."
  (declare (ignore no-title))
  (setf (title buffer) (ffi-buffer-title buffer))
  (dolist (mode (modes buffer))
    (on-signal-notify-title mode (url buffer)))
  (title buffer))

;; See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#WebKitLoadEvent
(export-always 'on-signal-load-started)
(defmethod on-signal-load-started ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-started mode url)))

(export-always 'on-signal-load-redirected)
(defmethod on-signal-load-redirected ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-redirected mode url)))

(export-always 'on-signal-load-canceled)
(defmethod on-signal-load-canceled ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-redirected mode url)))

(export-always 'on-signal-load-committed)
(defmethod on-signal-load-committed ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-committed mode url)))

(export-always 'on-signal-load-finished)
(defmethod on-signal-load-finished ((buffer buffer) url)
  (update-document-model :buffer buffer)
  (dolist (mode (modes buffer))
    (on-signal-load-finished mode url))
  (run-thread "buffer-loaded-hook" (hooks:run-hook (buffer-loaded-hook buffer) buffer)))

(export-always 'on-signal-load-failed)
(defmethod on-signal-load-failed ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-failed mode url)))

(export-always 'on-signal-button-press)
(defmethod on-signal-button-press ((buffer buffer) button-key)
  (dolist (mode (modes buffer))
    (on-signal-button-press mode button-key)))

(hooks:define-hook-type buffer (function (buffer)))

(define-command make-buffer (&rest args &key (title "") modes (url (default-new-buffer-url *browser*)) parent-buffer
                             no-history-p (load-url-p t) (buffer-class 'web-buffer)
                             &allow-other-keys)
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is empty, the `default-new-buffer-url' browser slot is used instead.
To load nothing, set it to 'about:blank'.
PARENT-BUFFER is useful when we want to record buffer- and history relationships.
LOAD-URL-P controls whether to load URL right at buffer creation."
  (let* ((buffer (apply #'make-instance buffer-class
                        :title title
                        :extra-modes modes
                        :parent-buffer parent-buffer
                        :no-history-p no-history-p
                        (append
                         (unless (url-empty-p url)
                           (list :url url))
                         (uiop:remove-plist-keys '(:title :modes :url :parent-buffer
                                                   :no-history-p :load-url-p)
                                                 args)))))
    (when load-url-p
      (buffer-load url :buffer buffer))
    buffer))

(define-command make-nosave-buffer (&rest args
                                    &key title modes url load-url-p)
  "Create a new buffer that won't save anything to the filesystem.
See `make-buffer' for a description of the arguments."
  (declare (ignorable title modes url load-url-p))
  (apply #'make-buffer (append (list :buffer-class 'nosave-buffer) args)))

(define-command make-buffer-focus (&key (url (default-new-buffer-url *browser*)) parent-buffer nosave-buffer-p)
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
  (apply #'make-buffer (append (list :buffer-class 'background-buffer :no-history-p t) args)))

(define-command duplicate-buffer-with-current-modes (&key (modes nil) parent-buffer)
  "Duplicate current buffer in a new buffer with current modes as well."
  (let* ((curr-buffer (current-buffer))
         (buffer (make-buffer :title (title curr-buffer)
                              :url (url curr-buffer)
                              :modes (or modes
                                         (mapcar #'sera:class-name-of
                                                 (modes curr-buffer)))
                              :parent-buffer parent-buffer)))
    (set-current-buffer buffer)
    buffer))

(define-command duplicate-buffer (&key parent-buffer)
  "Duplicate current buffer in a new buffer."
  (duplicate-buffer-with-current-modes :modes (list (resolve-symbol :document-mode :mode) 'base-mode)
                                       :parent-buffer parent-buffer))

(-> add-to-recent-buffers (buffer) *)
(defun add-to-recent-buffers (buffer)
  "Create a recent-buffer from given buffer and add it to `recent-buffers'."
  (when (web-buffer-p buffer)
    (containers:delete-item-if (recent-buffers *browser*) (buffer-match-predicate buffer))
    (containers:insert-item (recent-buffers *browser*) buffer)))


(defmethod buffer-delete ((buffer buffer))
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (ffi-buffer-delete buffer))

(defmethod buffer-delete ((buffer context-buffer))
  (files:with-file-content (history (history-file buffer))
    (sera:and-let* ((owner (htree:owner history (id buffer)))
                    (current (htree:current owner))
                    (data (htree:data current)))
      (setf (nyxt::scroll-position data) (nyxt:document-scroll-position buffer))
      (htree:delete-owner history (id buffer))))
  (call-next-method))

(defun buffer-hide (buffer)
  "Stop showing the buffer in Nyxt.
Should be called from/instead of `ffi-buffer-delete' when the renderer view
associated to the buffer is already killed."
  (let ((parent-window (find buffer (window-list) :key 'active-buffer)))
    (when parent-window
      (let ((replacement-buffer (or (first (get-inactive-buffers))
                                    (make-buffer :load-url-p nil
                                                 :url (default-new-buffer-url *browser*)))))
        (window-set-buffer parent-window replacement-buffer)))
    (buffers-delete (id buffer))
    (add-to-recent-buffers buffer)))

(export-always 'buffer-list)
(defun buffer-list ()
  "Order is stable."
  (sort
   (alex:hash-table-values (buffers *browser*))
   #'>
   ;; TODO: Sort by creation time instead?
   :key #'id))

(defun buffers-get (id)
  (gethash id (slot-value *browser* 'buffers)))

(defun buffers-set (id buffer)
  (when *browser*
    (setf (gethash id (slot-value *browser* 'buffers)) buffer)
    ;; Force setf call so that slot is seen as changed, e.g. by status buffer watcher.
    (setf (slot-value *browser* 'buffers) (slot-value *browser* 'buffers))))

(defun buffers-delete (id)
  (remhash id (slot-value *browser* 'buffers))
  ;; Force setf call so that slot is seen as changed, e.g. by status buffer watcher.
  (setf (slot-value *browser* 'buffers) (slot-value *browser* 'buffers)))

(export-always 'window-list)
(defun window-list ()
  (alex:hash-table-values (windows *browser*)))

(defun dummy-buffer-p (buffer)
  (eq 'buffer (type-of buffer)))

(export-always 'window-set-buffer)
(defun window-set-buffer (window buffer &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (window-set-buffer-hook window) window buffer)
  ;; When not focusing, that is, when previewing we don't update the
  ;; `last-access' so as to not disturb the ordering.
  (when (and focus
             (context-buffer-p (active-buffer window)))
    ;; The current buffer last-access time is set to now to ensure it becomes the
    ;; second newest buffer.  If we didn't update the access time, the buffer
    ;; last-access time could be older than, say, buffers opened in the
    ;; background.
    (setf (last-access (active-buffer window)) (local-time:now)))
  ;; So that `current-buffer' returns the new value if buffer was
  ;; switched inside a `with-current-buffer':
  (setf %buffer nil)
  (if (dummy-buffer-p (active-buffer window))
      (let ((dummy (active-buffer window)))
        (ffi-window-set-buffer window buffer :focus focus)
        (setf (active-buffer window) buffer)
        (ffi-buffer-delete dummy))

      (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                           :key #'active-buffer)))
        (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
            (let ((temp-buffer (make-instance 'buffer))
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
              (setf (active-buffer window) buffer)))))
  (when (and focus
             (context-buffer-p buffer))
    (setf (last-access buffer) (local-time:now)))
  (when (and (network-buffer-p buffer)
             (eq (slot-value buffer 'status) :unloaded))
    (reload-buffer buffer)))

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
   (prompter:return-actions (list (lambda-unmapped-command set-current-buffer)
                                  (lambda-mapped-command buffer-delete)
                                  'reload-buffers))
   (prompter:selection-actions-enabled-p t)
   (prompter:selection-actions-delay 0.1)
   (prompter:selection-actions #'(lambda (buffer)
                                   (set-current-buffer buffer :focus nil)))
   (prompter:destructor (let ((buffer (current-buffer)))
                          (lambda (prompter source)
                            (declare (ignore source))
                            (unless (or (prompter:returned-p prompter)
                                        (eq buffer (current-buffer)))
                              (set-current-buffer buffer))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((buffer buffer) (source prompter:source))
  (declare (ignore source))
  `(("URL" ,(render-url (url buffer)))
    ("Title" ,(title buffer))
    ("ID" ,(id buffer))))

(defmethod prompter:object-attributes ((buffer web-buffer) (source buffer-source))
  (declare (ignore source))
  `(("URL" ,(render-url (url buffer)))
    ("Title" ,(title buffer))
    ("ID" ,(id buffer))
    ("Keywords" ,(lambda (buffer) (format nil "~:{~a~^ ~}" (keywords buffer))))))

(define-command switch-buffer (&key buffer (current-is-last-p nil))
  "Switch buffer using fuzzy completion.
Buffers are ordered by last access.
With CURRENT-IS-LAST-P, the current buffer is listed last so as to list the
second latest buffer first."
  (if buffer
      (set-current-buffer buffer)
      (prompt
       :prompt "Switch to buffer"
       :sources (make-instance 'buffer-source
                               :constructor (buffer-initial-suggestions
                                             :current-is-last-p current-is-last-p)))))

(define-command switch-buffer-domain (&key domain (buffer (current-buffer)))
  "Switch the active buffer in the current window from the current domain."
  (let ((domain (or domain (quri:uri-domain (url buffer)))))
    (prompt
     :prompt "Switch to buffer in current domain"
     :sources (make-instance 'buffer-source
                             :constructor (sera:filter (match-domain domain)
                                                       (sort-by-time (buffer-list)))))))

(defun switch-buffer-or-query-domain (domain)
  "Switch to a buffer if it exists for a given DOMAIN, otherwise query
  the user."
  (let ((matching-buffers (sera:filter (match-domain domain) (buffer-list))))
    (if (eql 1 (length matching-buffers))
        (set-current-buffer (first matching-buffers))
        (switch-buffer-domain :domain domain))))

(define-command delete-buffer
    (&key (buffers
           (prompt
            :prompt "Delete buffer(s)"
            :sources (make-instance 'buffer-source
                                    :multi-selection-p t
                                    :return-actions (list 'identity)))))
  "Query the buffer(s) to delete.

BUFFERS should be a list of `buffer's."
  (mapcar #'buffer-delete (alex:ensure-list buffers)))

(define-command delete-all-buffers (&key (confirmation-p t))
  "Delete all buffers, with confirmation."
  (let ((count (length (buffer-list))))
    (if confirmation-p
        (if-confirm ((format nil "Delete ~a buffer~p?" count count))
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
    (if-confirm ((format nil "Delete ~a buffer~p?" count count))
                (mapcar #'buffer-delete buffers-to-delete))))

(export-always 'buffer-load)
(declaim (ftype (function (url-designator &key (:buffer buffer)))
                buffer-load))
(defun buffer-load (url-designator &key (buffer (current-buffer)))
  "Load URL-DESIGNATOR in BUFFER.
URL-DESIGNATOR is then transformed by BUFFER's `buffer-load-hook'."
  ;; TODO: Move all most of this code to `ffi-buffer-load :around'?
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

      ;; We could have `on-url-load' and `on-url-unload' methods instead.
      ;; `on-url-unload' could be used to perform some clean up, while
      ;; `on-url-load' would perform the actual loading.
      ;; Then subclass `quri:uri' with uri-js, uri-nyxt, uri-lisp, etc.
      ;; Finally, specialize against these URLs.
      (cond
        ((equal "javascript" (quri:uri-scheme url))
         (ffi-buffer-evaluate-javascript buffer (quri:url-decode (quri:uri-path url))))
        (t
         (clrhash (lisp-url-callbacks buffer)) ; REVIEW: Is it the only spot where to clear the Lisp URL callbacks?
         (alex:when-let ((page (find-url-internal-page url)))
           (disable-modes (page-mode page) buffer))
         (ffi-buffer-load buffer url))))
    buffer))

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
   (prompter:return-actions '(buffer-load)))
  (:export-class-name-p t)
  (:metaclass user-class))

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
           (not (uiop:emptyp (query query)))
           (not (uiop:emptyp (label query))))
      (query query))
     ((and (engine query)
           (not (uiop:emptyp (query query)))
           (uiop:emptyp (label query)))
      (format nil (search-url (engine query))
              (str:join ""
                        (mapcar #'encode-url-char
                                (map 'list #'string (query query))))))
     ((engine query)
      (fallback-url (engine query)))
     (t (query query)))))

(defun make-completion-query (completion &key engine (check-dns-p t))
  (typecase completion
    (string (make-instance 'new-url-query
                           :engine      engine
                           :check-dns-p check-dns-p
                           :query completion))
    (list (make-instance 'new-url-query
                         :engine engine
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
            (or (mappend (lambda (engine)
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
                              (mapcar (rcurry #'make-completion-query
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
                  (mapcar (rcurry #'make-completion-query
                                  :engine      engine
                                  :check-dns-p check-dns-p)
                          (with-protect ("Error while completing default search: ~a" :condition)
                            (funcall (completion-function engine) all-terms))))))))

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
   (prompter:return-actions '(buffer-load)))
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
any.")
  (:metaclass user-class))

(defmethod prompter:object-attributes ((query new-url-query) (source new-url-or-search-source))
  (declare (ignore source))
  `(("URL or new query" ,(or (label query) (query query)))
    ("Search engine?" ,(if (engine query) (shortcut (engine query)) ""))))

(defun pushnew-url-history (history url)
  "URL is not pushed if empty."
  (when (and history (not (url-empty-p url)))
    (prompter::history-pushnew history (render-url url))))

(export-always 'url-sources)
(defmethod url-sources ((buffer buffer) return-actions)
  (append
   (list (make-instance 'new-url-or-search-source :return-actions return-actions)
         (make-instance 'global-history-source :return-actions return-actions)
         (make-instance 'search-engine-url-source :return-actions return-actions))
   (mappend (rcurry #'url-sources return-actions) (modes buffer))))

(define-command set-url (&key (prefill-current-url-p t))
  "Set the URL for the current buffer, completing with history."
  (let ((history (set-url-history *browser*))
        (return-actions
          (list (lambda-command buffer-load* (suggestion-values)
                  "Load first selected URL in current buffer and the rest in new buffer(s)."
                  (mapc (lambda (suggestion) (make-buffer :url (url suggestion))) (rest suggestion-values))
                  (buffer-load (url (first suggestion-values))))
                (lambda-command new-buffer-load* (suggestion-values)
                  "Load URL(s) in new buffer(s)."
                  (mapc (lambda (suggestion) (make-buffer :url (url suggestion))) (rest suggestion-values))
                  (make-buffer-focus :url (url (first suggestion-values))))
                (lambda-command new-nosave-buffer-load* (suggestion-values)
                  "Load URL(s) in new buffer(s)."
                  (mapc (lambda (suggestion) (make-nosave-buffer :url (url suggestion))) (rest suggestion-values))
                  (set-current-buffer
                   (make-nosave-buffer :url (url (first suggestion-values))) :focus t))
                (lambda-command copy-url* (suggestions)
                  "Copy the URL of the chosen suggestion."
                  (trivial-clipboard:text (render-url (url (first suggestions))))))))
    (pushnew-url-history history (url (current-buffer)))
    (prompt
     :prompt "Open URL"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :history history
     :sources (url-sources (current-buffer) return-actions))
    (current-buffer)))

(define-command set-url-new-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused buffer."
  (let ((history (set-url-history *browser*))
        (return-actions (list (lambda-command new-buffer-load (suggestion-values)
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
     :sources (url-sources (current-buffer) return-actions))
    (current-buffer)))

(define-command set-url-new-nosave-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused nosave buffer."
  (let ((return-actions
          (list (lambda-command new-nosave-buffer-load (suggestion-values)
                  "Load URL(s) in new nosave buffer(s)"
                  (mapc (lambda (suggestion) (make-nosave-buffer :url (url suggestion)))
                        (rest suggestion-values))
                  (make-buffer-focus :url (url (first suggestion-values))
                                     :nosave-buffer-p t)))))
    (prompt
     :prompt "Open URL in new nosave buffer"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :sources (url-sources (current-buffer) return-actions))
    (current-buffer)))

(define-command reload-buffer
    (&optional (buffer
                (prompt
                 :prompt "Reload buffer(s)"
                 :sources (make-instance 'buffer-source))))
  "Reload BUFFER.
Return it."
  (when buffer
    (buffer-load (url buffer) :buffer buffer)))

(define-command reload-current-buffer ()
  "Reload current buffer.
Return it."
  (reload-buffer (current-buffer)))

(define-command reload-buffers
    (&optional (buffers
                (prompt
                 :prompt "Reload buffer(s)"
                 :sources (make-instance 'buffer-source :multi-selection-p t))))
  "Prompt for BUFFERS to be reloaded.
Return BUFFERS."
  (when buffers
    (mapcar #'reload-buffer (alex:ensure-list buffers)))
  buffers)

(defun buffer-parent (&optional (buffer (current-buffer)))
  (let ((history (buffer-history buffer)))
    (sera:and-let* ((owner (htree:owner history (id buffer)))
                    (parent-id (htree:creator-id owner)))
      (gethash parent-id (buffers *browser*)))))

(defun buffers-with-history (history)
  "Return the list of buffers that have history HISTORY.
HISTORY may be NIL for buffers without history."
  (remove-if (complement (sera:eqs history))
             (buffer-list)
             :key #'buffer-history))

(defun buffer-children (&optional (buffer (current-buffer)))
  (let* ((history (buffer-history buffer))
         (buffers (buffers-with-history history)))
    (sort (sera:filter
           (sera:eqs (id buffer))
           buffers
           :key (lambda (b) (alex:when-let ((owner (htree:owner history (id b))))
                              (htree:creator-id owner))))
          #'< :key #'id)))

(defun buffer-siblings (&optional (buffer (current-buffer)))
  (let* ((history (buffer-history buffer))
         (buffers (buffers-with-history history)))
    (flet ((existing-creator-id (owner)
             "If owner's creator does not exist anymore
(that is, parent has been deleted), return NIL so it can mimic top-level owners."
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
               (sort common-parent-buffers #'< :key #'id)))
        (sera:split-sequence-if (sera:eqs (id buffer))
                                common-parent-buffers
                                :key #'id)))))

(define-command switch-buffer-previous (&optional (buffer (current-buffer)))
  "Switch to the previous buffer in the buffer tree.
Return it.

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
Return it.

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
  "Switch to the last visited buffer.
That is, the one with the most recent access time.

Return this last buffer."
  (let* ((buffers (sort-by-time (buffer-list))))
    (when (second buffers)
      (set-current-buffer (second buffers)))))

(define-command open-inspector ()
  "Open the inspector, a graphical tool to inspect and change the buffer's content."
  (ffi-inspector-show (current-buffer))
  (current-buffer))
