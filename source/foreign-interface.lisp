;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-ffi-generic (name arguments &body options)
  "Like `defgeneric' but export NAME and define default dummy method if none is
provided.
If the `:setter-p' option is non-nil, then a dummy setf method is defined."
  (let* ((methods (sera:filter (sera:eqs :method) options :key #'first))
         (setter? (alex:assoc-value options :setter-p))
         (normalized-options (set-difference options methods :key #'first))
         (normalized-options (setf (alex:assoc-value normalized-options :setter-p) nil)))
    `(progn
       (export-always ',name)
       (prog1
           (defgeneric ,name (,@arguments)
             ,@(if methods
                   methods
                   `((:method (,@arguments)
                       (declare (ignore ,@(set-difference arguments lambda-list-keywords))))))
             ,@normalized-options)
         ,(when setter?
            `(defmethod (setf ,name) (value ,@arguments)
               (declare (ignore value ,@arguments))))))))

(define-ffi-generic ffi-window-delete (window)
  (:documentation "Delete WINDOW, possibly freeing the associated widgets.
After this call, the window should not be displayed."))

(define-ffi-generic ffi-window-fullscreen (window))
(define-ffi-generic ffi-window-unfullscreen (window))

(define-ffi-generic ffi-buffer-url (buffer)
  (:documentation "Return the `quri:uri' associated with the BUFFER.
This is used to set the `buffer' `url' slot."))
(define-ffi-generic ffi-buffer-title (buffer)
  (:documentation "Return as a string the title of the document (or web page)
showing in BUFFER."))

(define-ffi-generic ffi-window-make (browser)
  (:method ((browser t))
    (declare (ignore browser))
    (make-instance 'window))
  (:documentation "Return a `window' object, ready for display.
The renderer specialization must handle the widget initialization."))

(define-ffi-generic ffi-window-to-foreground (window)
  (:method ((window t))
    (setf (slot-value *browser* 'last-active-window) window))
  (:documentation "Show WINDOW in the foreground.
The specialized method may call `call-next-method' to set
WINDOW as the `last-active-window'."))

(define-ffi-generic ffi-window-title (window)
  (:setter-p t)
  (:documentation "Return as a string the title of the window.
It is the title that's often used by the window manager to decorate the window.
Setf-able."))

(define-ffi-generic ffi-window-active (browser)
  (:method ((browser t))
    (or (slot-value browser 'last-active-window)
        (first (window-list))))
  (:method :around ((browser t))
    (setf (slot-value browser 'last-active-window)
          (call-next-method)))
  (:documentation "The primary method returns the focused window as per the
renderer.

The `:around' method automatically ensures that the result is set to
`last-active-window'.

The specialized method may call `call-next-method' to return a sensible fallback window."))

(define-ffi-generic ffi-window-set-buffer (window buffer &key focus)
  (:documentation "Set the BUFFER's widget to display in WINDOW.
If FOCUS is non-nil, "))

(define-ffi-generic ffi-window-add-panel-buffer (window buffer side)
  (:documentation "Make widget for pannel BUFFER and add it to the WINDOW widget.
SIDE is one of `:left' or `:right'."))
(define-ffi-generic ffi-window-delete-panel-buffer (window buffer)
  (:documentation "Unbind the pannel BUFFER widget from WINDOW."))

(define-ffi-generic ffi-window-panel-buffer-width (window buffer)
  (:setter-p t)
  (:documentation "Return the panel BUFFER width as a number.
Setf-able."))
(define-ffi-generic ffi-window-prompt-buffer-height (window)
  (:setter-p t)
  (:documentation "Return the WINDOW prompt buffer height as a number.
Setf-able."))
(define-ffi-generic ffi-window-status-buffer-height (window)
  (:setter-p t)
  (:documentation "Return the WINDOW status buffer height as a number.
Setf-able."))
(define-ffi-generic ffi-window-message-buffer-height (window)
  (:setter-p t)
  (:documentation "Return the WINDOW message buffer height as a number.
Setf-able."))

(define-ffi-generic ffi-buffer-make (buffer)
  (:documentation "Make BUFFER widget."))
(define-ffi-generic ffi-buffer-delete (buffer)
  (:documentation "Delete BUFFER widget."))

(define-ffi-generic ffi-buffer-load (buffer url)
  (:documentation "Load URL into BUFFER through the renderer."))

(define-ffi-generic ffi-buffer-load-html (buffer html-content url)
  (:documentation "Load HTML-CONTENT into BUFFER through the renderer.
If URL is not nil, relative URLs are resolved against it."))
(define-ffi-generic ffi-buffer-load-alternate-html (buffer html-content content-url url)
  (:documentation "Load HTML-CONTENT for CONTENT-URL into BUFFER through the renderer.
This is like `ffi-buffer-load-html' except that it's meant to display
page-loading errors and it should try to maintain the history consistent."))

(define-ffi-generic ffi-buffer-evaluate-javascript (buffer javascript &optional world-name)
  (:documentation "Evaluate JAVASCRIPT in the BUFFER web view.
See also `ffi-buffer-evaluate-javascript-async'."))
(define-ffi-generic ffi-buffer-evaluate-javascript-async (buffer javascript &optional world-name)
  (:documentation "Same as `ffi-buffer-evaluate-javascript' but don't wait for
the termination of the JavaScript execution."))

(define-ffi-generic ffi-buffer-add-user-style (buffer css &key
                                                      world-name all-frames-p inject-as-author-p
                                                      allow-list block-list)
  ;; TODO: Document the options!
  (:documentation "Apply the CSS style to the BUFFER web view."))
(define-ffi-generic ffi-buffer-remove-user-style (buffer style-sheet)
  (:documentation "Remove the STYLE-SHEET installed with `ffi-buffer-add-user-style'."))

(define-ffi-generic ffi-buffer-add-user-script (buffer user-script)
  (:documentation "Install the JAVASCRIPT  into the BUFFER web view."))
(define-ffi-generic ffi-buffer-remove-user-script (buffer script)
  (:documentation "Remove the SCRIPT installed with `ffi-buffer-add-user-script'."))

(define-ffi-generic ffi-buffer-javascript-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-javascript-markup-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-smooth-scrolling-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-media-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-webgl-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-auto-load-image-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))
(define-ffi-generic ffi-buffer-sound-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return setting as boolean.
Setf-able."))

(define-ffi-generic ffi-buffer-user-agent (buffer)
  (:setter-p t)
  (:documentation "Return the user agent as a string.
Setf-able."))

(define-ffi-generic ffi-buffer-proxy (buffer)
  (:setter-p t)
  (:documentation "Return the proxy URL as a `quri:uri'.
Return the list of ignored hosts (list of strings) as a second value.

Setf-able.  The value is either a PROXY-URL or a pair of (PROXY-URL IGNORE-HOSTS).
PROXY-URL is a `quri:uri' and IGNORE-HOSTS a list of strings."))

(define-ffi-generic ffi-buffer-download (buffer url)
  (:documentation "Download URL using the BUFFER web view."))

(define-ffi-generic ffi-buffer-zoom-level (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (peval (ps:chain document body style zoom))))
  (:setter-p t)
  (:documentation "Return the zoom level of the document.
Setf-able."))
(defmethod (setf ffi-buffer-zoom-level) (value (buffer buffer))
  (with-current-buffer buffer
      (peval (ps:let ((style (ps:chain document body style)))
               (setf (ps:@ style zoom)
                     (ps:lisp value))))))

(define-ffi-generic ffi-buffer-get-document (buffer)
  (:method ((buffer t))
    (pflet ((get-html (start end)
                      (ps:chain document document-element |innerHTML| (slice (ps:lisp start)
                                                                             (ps:lisp end))))
            (get-html-length ()
                             (ps:chain document document-element |innerHTML| length)))
      (with-current-buffer buffer
        (let ((slice-size 10000))
          (reduce #'str:concat
                  (loop for i from 0 to (truncate (get-html-length)) by slice-size
                        collect (get-html i (+ i slice-size))))))))
  (:documentation "Return the BUFFER raw HTML as a string."))

(define-ffi-generic ffi-generate-input-event (window event)
  (:documentation "Send input EVENT to renderer for WINDOW.
This allows to programmatically generate events on demand.
EVENT are renderer-specific objects.

The resulting should somehow be marked as generated, to allow Nyxt to tell
spontaneous events from programmed ones.
See also `ffi-generated-input-event-p'."))

(define-ffi-generic ffi-generated-input-event-p (window event)
  (:documentation "Return non-nil if EVENT was generated by `ffi-generated-input-event'."))

(define-ffi-generic ffi-within-renderer-thread (browser thunk)
  (:method ((browser t) thunk)
    (declare (ignore browser))
    (funcall thunk))
  (:documentation "Run THUNK (a lambda of no argument) from the renderer's thread.
This is useful in particular for renderer-specific functions that cannot be run on random threads."))

(define-ffi-generic ffi-kill-browser (browser)
  (:documentation "Terminate the renderer.
This often translates in the termination of the \"main loop\" associated to the widget engine."))

(define-ffi-generic ffi-initialize (browser urls startup-timestamp)
  (:method ((browser t) urls startup-timestamp)
    (finalize browser urls startup-timestamp))
  (:documentation "Renderer-specific initialization.
When done, call `call-next-method' to finalize the startup."))

(define-ffi-generic ffi-inspector-show (buffer)
  (:documentation "Show the renderer built-in inspector."))

(define-ffi-generic ffi-print-status (window text)
  (:documentation "Display TEST in the WINDOW status buffer."))

(define-ffi-generic ffi-print-message (window message)
  (:documentation "Print MESSAGE (an HTML string) in the WINDOW message buffer."))

(define-ffi-generic ffi-display-url (browser url)
  (:documentation "Return URL as a human-readable string.
In particular, this should understand Punycode."))

(define-ffi-generic ffi-buffer-cookie-policy (buffer)
  (:setter-p t)
  (:documentation "Return the cookie 'accept' policy, one of of`:always',
`:never' or `:no-third-party'.

Setf-able with the same aforementioned values."))

(define-ffi-generic ffi-preferred-languages (buffer)
  (:setter-p t)
  (:documentation "Set the list of preferred languages in the HTTP header \"Accept-Language:\".
Setf-able, where the languages value is a list of strings like '(\"en_US\"
\"fr_FR\")."))

(define-ffi-generic ffi-focused-p (buffer)
  (:documentation "Return non-nil if the BUFFER widget is the one with focus."))

(define-ffi-generic ffi-tracking-prevention (buffer)
  (:setter-p t)
  (:documentation "Return if Intelligent Tracking Prevention (ITP) is enabled.
Setf-able."))

(define-ffi-generic ffi-buffer-copy (buffer)
  (:method :after ((buffer t))
    (ring-insert-clipboard (clipboard-ring *browser*)))
  (:method ((buffer t))
    (with-current-buffer buffer
      ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
      ;; copy something from the context menu 'Copy' action, upon pasting we will
      ;; retrieve the text from the GTK thread.  This is prone to create
      ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
      ;;
      ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
      ;; is present the clipboard and need not be retrieved from the GTK thread.
      ;; TODO: Do we still need to flush now that we have multiple threads?
      ;; (trivial-clipboard:text (trivial-clipboard:text))
      (let ((input (%copy)))
        (copy-to-clipboard input)
        (echo "Text copied: ~s" input))))
  (:documentation "Copy selected text in BUFFER to the system clipboard."))

(define-ffi-generic ffi-buffer-paste (buffer)
  ;; While it may sound unintuitive, it helps keeping track of system clipboard,
  ;; both in Nyxt->OS and OS->Nyxt directions.
  (:method :after ((buffer t))
    (ring-insert-clipboard (clipboard-ring *browser*)))
  (:method ((buffer t))
    (with-current-buffer buffer
      (%paste)))
  (:documentation "Paste the last clipboard entry into BUFFER."))

(define-ffi-generic ffi-buffer-cut (buffer)
  (:method :after ((buffer t))
    (ring-insert-clipboard (clipboard-ring *browser*)))
  (:method ((buffer t))
    (with-current-buffer buffer
      (let ((input (%cut)))
        (when input
          (copy-to-clipboard input)
          (echo "Text cut: ~s" input)))))
  (:documentation "Cut selected text in BUFFER to the system clipboard."))

(define-ffi-generic ffi-buffer-select-all (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (%select-all)))
  (:documentation "Select all text in BUFFER web view."))

(define-ffi-generic ffi-buffer-undo (buffer)
  (:method ((buffer t))
    (echo-warning "Undoing edits is not yet implemented for this renderer."))
  (:documentation "Undo the last text edit performed in BUFFER's web view."))

(define-ffi-generic ffi-buffer-redo (buffer)
  (:method ((buffer t))
    (echo-warning "Redoing edits is not yet implemented for this renderer."))
  (:documentation "Redo the last undone text edit performed in BUFFER's web view."))
