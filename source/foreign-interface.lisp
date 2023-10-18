;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-ffi-generic (name arguments &body (documentation . options))
  "Like `defgeneric' but export NAME and define default dummy method if none is provided.
If the `:setter-p' option is non-nil, then a dummy setf method is defined."
  (flet ((option-first (option)
           (first (uiop:ensure-list option))))
    (let* ((methods (sera:filter (sera:eqs :method) options :key #'option-first))
           (setter? (alex:assoc-value options :setter-p))
           ;; Instead of `set-difference', which mangles the option order.
           (normalized-options (remove-if (lambda (option)
                                            (member (option-first option) methods :key #'option-first))
                                          options))
           (normalized-options (remove :setter-p normalized-options :key #'option-first)))
      `(progn
         (export-always ',name)
         (prog1
             (define-generic ,name (,@arguments)
               ,documentation
               ,@(if methods
                     methods
                     `((declare (ignorable ,@(set-difference
                                              (mapcar (compose #'first #'uiop:ensure-list) arguments)
                                              lambda-list-keywords)))))
               ,@normalized-options)
           ,(when setter?
              `(define-generic (setf ,name) (value ,@arguments)
                 ;; Ignore here, because setter generic doesn't have any body,
                 ;; unlike the generic above.
                 (declare (ignore value ,@arguments)))))))))

(define-ffi-generic ffi-window-delete (window)
  "Delete WINDOW, possibly freeing the associated widgets.
After this call, the window should not be displayed.")

(define-ffi-generic ffi-window-fullscreen (window)
  "Make WINDOW fullscreen.
Removes all the Nyxt interface element and leaves the open page as the only
thing on the screen.")
(define-ffi-generic ffi-window-unfullscreen (window)
  "Un-fullscreen the WINDOW.")

(define-ffi-generic ffi-window-maximize (window)
  "Make the WINDOW to fill the screen.
Usually possible via desktop environment interface too.")
(define-ffi-generic ffi-window-unmaximize (window)
  "Switch the WINDOW to \"windowed\" display.
May or may not fill the screen.")

(define-ffi-generic ffi-buffer-url (buffer)
  "Return the `quri:uri' associated with the BUFFER.
This is used to set the BUFFER `url' slot.")
(define-ffi-generic ffi-buffer-title (buffer)
  "Return as a string the title of the document (or web page) showing in BUFFER.")

(define-ffi-generic ffi-window-make (browser)
  "Return a `window' object, ready for display.
The renderer specialization must handle the widget initialization."
  (make-instance 'window))

(define-ffi-generic ffi-window-to-foreground (window)
  "Show WINDOW in the foreground.
The specialized method may call `call-next-method' to set
WINDOW as the `last-active-window'."
  (setf (slot-value *browser* 'last-active-window) window))

(define-ffi-generic ffi-window-title (window)
  "Return as a string the title of the window.
It is the title that's often used by the window manager to decorate the window.
Setf-able."
  (:setter-p t))

(define-ffi-generic ffi-window-active (browser)
  "The primary method returns the focused window as per the
renderer.

The `:around' method automatically ensures that the result is set to
`last-active-window'.

The specialized method may call `call-next-method' to return a sensible fallback window."
  (:method ((browser t))
    (or (slot-value browser 'last-active-window)
        (first (window-list))))
  (:method :around ((browser t))
    (setf (slot-value browser 'last-active-window)
          (call-next-method))))

(define-ffi-generic ffi-window-set-buffer (window buffer &key focus)
  "Set the BUFFER's widget to display in WINDOW.")

(define-ffi-generic ffi-focus-prompt-buffer (window prompt-buffer)
  "Focus PROMPT-BUFFER in WINDOW.")

(define-ffi-generic ffi-window-add-panel-buffer (window buffer side)
  "Make widget for panel BUFFER and add it to the WINDOW widget.
SIDE is one of `:left' or `:right'.")
(define-ffi-generic ffi-window-delete-panel-buffer (window buffer)
  "Unbind the panel BUFFER widget from WINDOW.")

(define-ffi-generic ffi-height (object)
  "Return the OBJECT height in pixels as a number.
Dispatches over `window' and classes inheriting from `buffer'.
Usually setf-able."
  (:setter-p t))
(define-ffi-generic ffi-width (object)
  "Return the OBJECT width in pixels as a number.
Dispatches over `window' and classes inheriting from `buffer'.
Usually setf-able."
  (:setter-p t))

(define-ffi-generic ffi-buffer-make (buffer)
  "Make BUFFER widget.")
(define-ffi-generic ffi-buffer-delete (buffer)
  "Delete BUFFER widget.")

(define-ffi-generic ffi-buffer-load (buffer url)
  "Load URL into BUFFER through the renderer.")

(define-ffi-generic ffi-buffer-load-html (buffer html-content url)
  "Load HTML-CONTENT into BUFFER through the renderer.
If URL is not nil, relative URLs are resolved against it.")
(define-ffi-generic ffi-buffer-load-alternate-html (buffer html-content content-url url)
  "Load HTML-CONTENT for CONTENT-URL into BUFFER through the renderer.
Like `ffi-buffer-load-html', except that it doesn't influence the BUFFER history
or CSS/HTML cache.")

(define-ffi-generic ffi-buffer-evaluate-javascript (buffer javascript &optional world-name)
  "Evaluate JAVASCRIPT in the BUFFER web view.
See also `ffi-buffer-evaluate-javascript-async'.")
(define-ffi-generic ffi-buffer-evaluate-javascript-async (buffer javascript &optional world-name)
  "Same as `ffi-buffer-evaluate-javascript' but don't wait for
the termination of the JavaScript execution.")

(define-ffi-generic ffi-buffer-add-user-style (buffer style)
  "Apply the CSS style to the BUFFER web view.")
(define-ffi-generic ffi-buffer-remove-user-style (buffer style)
  "Remove the STYLE installed with `ffi-buffer-add-user-style'.")

(define-ffi-generic ffi-buffer-add-user-script (buffer user-script)
  "Install the JAVASCRIPT  into the BUFFER web view.")
(define-ffi-generic ffi-buffer-remove-user-script (buffer script)
  "Remove the SCRIPT installed with `ffi-buffer-add-user-script'.")

(define-ffi-generic ffi-buffer-javascript-enabled-p (buffer)
  "Return whether JavaScript can run, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-javascript-markup-enabled-p (buffer)
  "Return whether JavaScript can alter the page contents, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-smooth-scrolling-enabled-p (buffer)
  "Return whether the smooth scrolling is enabled, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-media-enabled-p (buffer)
  "Return whether video and audio playback is enabled, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-webgl-enabled-p (buffer)
  "Return whether WebGL is enabled, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-auto-load-image-enabled-p (buffer)
  "Return whether images are displayed, as boolean.
Setf-able."
  (:setter-p t))
(define-ffi-generic ffi-buffer-sound-enabled-p (buffer)
  "Return whether the BUFFER has sound enabled, as boolean.
Setf-able."
  (:setter-p t))

(define-ffi-generic ffi-buffer-user-agent (buffer)
  "Return the user agent as a string.
Setf-able."
  (:setter-p t))

(define-ffi-generic ffi-buffer-proxy (buffer)
  "Return the proxy URL as a `quri:uri'.
Return the list of ignored hosts (list of strings) as a second value.

Setf-able.  The value is either a PROXY-URL or a pair of (PROXY-URL IGNORE-HOSTS).
PROXY-URL is a `quri:uri' and IGNORE-HOSTS a list of strings."
  (:setter-p t))

(define-ffi-generic ffi-buffer-download (buffer url)
  "Download URL using the BUFFER web view.")

(define-ffi-generic ffi-buffer-zoom-level (buffer)
  "Return the zoom level of the document.
Setf-able."
  (:method (buffer)
    (ps-eval :buffer buffer (ps:chain document body style zoom)))
  (:setter-p t))
(defmethod (setf ffi-buffer-zoom-level) (value (buffer buffer))
  "Use JavaScript, if the renderer does not allow zooming natively."
  (ps-eval :buffer buffer
    (ps:let ((style (ps:chain document body style)))
      (setf (ps:@ style zoom)
            (ps:lisp value)))))

(define-ffi-generic ffi-buffer-get-document (buffer)
  "Return the BUFFER raw HTML as a string."
  (:method ((buffer t))
    (ps-labels :buffer buffer
      ((get-html
        (start end)
        (ps:chain document document-element |innerHTML| (slice (ps:lisp start)
                                                               (ps:lisp end))))
       (get-html-length
        ()
        (ps:chain document document-element |innerHTML| length)))
      (let ((slice-size 10000))
        (reduce #'str:concat
                (loop for i from 0 to (truncate (get-html-length)) by slice-size
                      collect (get-html i (+ i slice-size))))))))

(define-ffi-generic ffi-generate-input-event (window event)
  "Send input EVENT to renderer for WINDOW.
This allows to programmatically generate events on demand.
EVENT are renderer-specific objects.

The resulting should somehow be marked as generated, to allow Nyxt to tell
spontaneous events from programmed ones.
See also `ffi-generated-input-event-p'.")

(define-ffi-generic ffi-generated-input-event-p (window event)
  "Return non-nil if EVENT was generated by `ffi-generate-input-event'.")

(define-ffi-generic ffi-within-renderer-thread (thunk)
  "Run THUNK (a lambda of no argument) in the renderer's thread.
It is particularly useful for renderer procedures required to be executed in
specific threads."
  (funcall thunk))

(define-ffi-generic ffi-kill-browser (browser)
  "Terminate the renderer.
This often translates in the termination of the \"main loop\" associated to the widget engine.")

(define-ffi-generic ffi-initialize (browser urls startup-timestamp)
  "Renderer-specific initialization.
A specialization of this method must call `call-next-method' to conclude the
startup routine."
  (finalize-startup browser urls startup-timestamp))

(define-ffi-generic ffi-inspector-show (buffer)
  "Show the renderer built-in inspector.")

(define-ffi-generic ffi-print-status (window html-body)
  "Display status buffer in WINDOW according to HTML-BODY.
The `style' of the `status-buffer' is honored."
  (with-slots (status-buffer) window
    (html-write (spinneret:with-html-string
                  (:head (:nstyle (style status-buffer)))
                  (:body (:raw html-body)))
                status-buffer)))

(define-ffi-generic ffi-print-message (window html-body)
  "Print HTML-BODY in the WINDOW's message buffer.
The `style' of the `message-buffer' is honored."
  (with-slots (message-buffer) window
    (html-write (spinneret:with-html-string
                  (:head (:nstyle (style message-buffer)))
                  (:body (:raw html-body)))
                message-buffer)))

(define-ffi-generic ffi-display-url (browser url)
  "Return URL as a human-readable string.
In particular, this should understand Punycode.")

(define-ffi-generic ffi-buffer-cookie-policy (buffer)
  "Return the cookie policy.
Setf-able.  Valid values are determined by the `cookie-policy' type."
  (:setter-p t))

(define-ffi-generic ffi-preferred-languages (buffer)
  "Set the list of preferred languages in the HTTP header \"Accept-Language:\".
Setf-able, where the languages value is a list of strings like '(\"en_US\"
\"fr_FR\")."
  (:setter-p t))

(define-ffi-generic ffi-focused-p (buffer)
  "Return non-nil if the BUFFER widget is the one with focus (currently displayed).")

(define-ffi-generic ffi-tracking-prevention (buffer)
  "Return if WebKit-specific Intelligent Tracking Prevention (ITP) is enabled.
Setf-able."
  (:setter-p t))

(define-ffi-generic ffi-buffer-copy (buffer &optional text)
  "Copy selected text in BUFFER to the system clipboard.
If TEXT is provided, add it to system clipboard instead of selected text.
Should return the copied text or NIL, if something goes wrong."
  (:method :around ((buffer t) &optional text)
    (declare (ignore text))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t) &optional (text nil text-provided-p))
    (ps-labels :buffer buffer ((copy () (ps:chain window (get-selection) (to-string))))
      ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
      ;; copy something from the context menu 'Copy' action, upon pasting we will
      ;; retrieve the text from the GTK thread.  This is prone to create
      ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
      ;;
      ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
      ;; is present the clipboard and need not be retrieved from the GTK thread.
      ;; TODO: Do we still need to flush now that we have multiple threads?
      ;; (trivial-clipboard:text (trivial-clipboard:text))

      (sera:lret ((input (if text-provided-p text (copy))))
        (copy-to-clipboard input)
        (echo "Text copied: ~s" input)))))

(define-ffi-generic ffi-buffer-paste (buffer &optional text)
  "Paste the last clipboard entry into BUFFER.
If TEXT is provided, paste it instead."
  ;; While it may sound counterintuitive, it helps to keep track of the system
  ;; clipboard, both in Nyxt->OS and OS->Nyxt directions.
  (:method :around ((buffer t) &optional text)
    (declare (ignore text))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t) &optional (text nil text-provided-p))
    (ps-labels :buffer buffer
      ((paste
        (&optional (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
        (let ((active-element (nyxt/ps:active-element document))
              (tag (ps:@ (nyxt/ps:active-element document) tag-name))
              (text-to-paste (or (ps:lisp input-text)
                                 (ps:chain navigator clipboard (read-text)))))
          (when (nyxt/ps:element-editable-p active-element)
            (nyxt/ps:insert-at active-element text-to-paste))
          text-to-paste)))
      (if text-provided-p
          (paste text)
          (paste)))))

(define-ffi-generic ffi-buffer-cut (buffer)
  "Cut selected text in BUFFER to the system clipboard.
Return the text cut."
  (:method :around ((buffer t))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t))
    (ps-labels :buffer buffer
      ((cut
        ()
        (let ((active-element (nyxt/ps:active-element document)))
          (when (nyxt/ps:element-editable-p active-element)
            (let ((selection-text (ps:chain window (get-selection) (to-string))))
              (nyxt/ps:insert-at active-element "")
              selection-text)))))
      (sera:lret ((input (cut)))
        (when input
          (copy-to-clipboard input)
          (echo "Text cut: ~s" input))))))

(define-ffi-generic ffi-buffer-select-all (buffer)
  "Select all text in BUFFER web view."
  (:method ((buffer t))
    (ps-eval :async t :buffer buffer
      (let ((active-element (nyxt/ps:active-element document)))
        (when (nyxt/ps:element-editable-p active-element)
          (ps:chain active-element (set-selection-range 0 (ps:@ active-element value length))))))))

(define-ffi-generic ffi-buffer-undo (buffer)
  "Undo the last text edit performed in BUFFER's web view."
  (echo-warning "Undoing edits is not yet implemented for this renderer."))

(define-ffi-generic ffi-buffer-redo (buffer)
  "Redo the last undone text edit performed in BUFFER's web view."
  (echo-warning "Redoing edits is not yet implemented for this renderer."))

;; TODO: Move to alists for arbitrary number of params?
(defvar *context-menu-commands* (make-hash-table :test #'equal)
  "A hash table from labels to context menu commands.
Once a context menu appears, those commands will be added to it as actions with
the labels they have as hash keys.")

;; TODO: Add TEST arg to decide on whether to display?
(define-ffi-generic ffi-add-context-menu-command (command label)
  "Add COMMAND as accessible in context menus with LABEL displayed for it.
COMMAND can be a:
- `command',
- `function',
- symbol naming either a command or function,
- or an alist (dotted or undotted) of COMMAND (any of above types, but not list)
  to LABEL pairs.

In case COMMAND is an alist, every command in this alist is bound to its own
label, and all of those are available under LABEL-named submenu.

Example:

\(ffi-add-context-menu-command
 (lambda ()
   (when (url-at-point (current-buffer))
     (make-nosave-buffer :url (url-at-point (current-buffer)))))
 \"Open Link in New Nosave Buffer\")

\(ffi-add-context-menu-command
 (list (list 'reload-current-buffer \"Reload it\")
       (list #'(lambda () (delete-buffer :buffers (current-buffer))) \"Delete it\"))
 \"Buffer actions\")"
  (:method ((command command) (label string))
    (setf (gethash label *context-menu-commands*)
          command))
  (:method ((command list) (label string))
    (flet ((thing->function (thing)
             (typecase thing
               (symbol (symbol-function thing))
               (function thing))))
      (setf (gethash label *context-menu-commands*)
            (mapcar (lambda (pair)
                      ;; Convert to an undotted alist.
                      (match pair
                        ((cons command (list label))
                         (list (thing->function command) label))
                        ((cons command label)
                         (list (thing->function command) label))))
                    command))))
  (:method ((command function) (label string))
    (setf (gethash label *context-menu-commands*)
          command))
  (:method ((command symbol) (label string))
    (ffi-add-context-menu-command (symbol-function command) label)))


;;; Signals

(export-always 'on-signal-notify-uri)
(define-generic on-signal-notify-uri (object url)
  "Invoked when URL changes in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) no-url)
    (declare (ignore no-url))
    ;; Need to run the mode-specific actions first so that modes can modify the
    ;; behavior of buffer.
    (dolist (mode (modes buffer))
      (on-signal-notify-uri mode (url buffer)))
    (let ((view-url (ffi-buffer-url buffer)))
      (unless (or (load-failed-p buffer)
                  (url-empty-p view-url))
        ;; When a buffer fails to load and `ffi-buffer-url' returns an empty
        ;; URL, we don't set (url buffer) to keep access to the old value.
        (setf (url buffer) (ffi-buffer-url buffer))))
    (url buffer))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-notify-title)
(define-generic on-signal-notify-title (object title)
  "Invoked when page TITLE is set in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) no-title)
    (declare (ignore no-title))
    (setf (title buffer) (ffi-buffer-title buffer))
    (dolist (mode (modes buffer))
      (on-signal-notify-title mode (url buffer)))
    (title buffer))
  (:method ((mode mode) title)
    (on-signal-notify-uri mode (url (buffer mode)))
    title))

;; See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#WebKitLoadEvent
(export-always 'on-signal-load-started)
(define-generic on-signal-load-started (object url)
  "Invoked when URL starts loading in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-started mode url)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-load-redirected)
(define-generic on-signal-load-redirected (object url)
  "Invoked when the request gets redirected to URL in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-redirected mode url)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-load-canceled)
(define-generic on-signal-load-canceled (object url)
  "Invoked when URL loading is canceled in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-canceled mode url)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-load-committed)
(define-generic on-signal-load-committed (object url)
  "Invoked when URL loading is approved in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-committed mode url)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-load-finished)
(define-generic on-signal-load-finished (object url)
  "Invoked when done loading URL in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (update-document-model :buffer buffer)
    (dolist (mode (modes buffer))
      (on-signal-load-finished mode url))
    (run-thread "buffer-loaded-hook" (hooks:run-hook (buffer-loaded-hook buffer) buffer)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-load-failed)
(define-generic on-signal-load-failed (object url)
  "Invoked when URL loading has failed in OBJECT.
Dispatches on buffers and modes."
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-failed mode url)))
  (:method ((mode mode) url)
    url))

(export-always 'on-signal-button-press)
(define-generic on-signal-button-press (object button-key)
  "Invoked on BUTTON-KEY press.
Dispatches on buffers and modes."
  (:method ((buffer buffer) button-key)
    (dolist (mode (modes buffer))
      (on-signal-button-press mode button-key)))
  (:method ((mode mode) button-key)
    (declare (ignorable button-key))
    nil))

(export-always 'on-signal-key-press)
(define-generic on-signal-key-press (object key)
  "Invoked on KEY press.
Dispatches on buffers and modes."
  (:method ((buffer buffer) key)
    (dolist (mode (modes buffer))
      (on-signal-key-press mode key)))
  (:method ((mode mode) key)
    (declare (ignorable key))
    nil))
