;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/gtk
    (:documentation "GTK renderer using direct CFFI bindings."))
(in-package :nyxt/renderer/gtk)

(define-class gtk-renderer (renderer)
  ((name "GTK"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "WebKit renderer class."))

(setf nyxt::*renderer* (make-instance 'gtk-renderer))
(pushnew :nyxt-gtk *features*)

(defvar renderer-thread-name "Nyxt renderer thread")

(export-always 'renderer-thread-p)
(defmethod renderer-thread-p ((renderer gtk-renderer) &optional (thread (bt:current-thread)))
  (string= renderer-thread-name (bt:thread-name thread)))

(defmethod install ((renderer gtk-renderer))
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser gtk-browser)
                               (renderer-window gtk-window)
                               (renderer-buffer gtk-buffer)
                               (nyxt/mode/download:renderer-download gtk-download)
                               (renderer-request-data gtk-request-data)
                               (renderer-scheme gtk-scheme)
                               (nyxt/mode/user-script:renderer-user-style gtk-user-style)
                               (nyxt/mode/user-script:renderer-user-script gtk-user-script)))))

(defmethod uninstall ((renderer gtk-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-window
                                  renderer-buffer
                                  nyxt/mode/download:renderer-download
                                  renderer-request-data
                                  renderer-scheme
                                  nyxt/mode/user-script:renderer-user-style
                                  nyxt/mode/user-script:renderer-user-script))))

(define-class gtk-browser ()
  ((web-contexts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "A table mapping strings to `webkit-web-context' objects."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "WebKit browser class."))

(defmethod get-web-context ((browser gtk-browser) name)
  (alexandria:ensure-gethash name
                             (web-contexts browser)
                             (make-web-context)))

(defmethod browser-schemes append ((browser gtk-browser))
  '("webkit" "webkit-pdfjs-viewer"))

(define-class gtk-window ()
  ((gtk-object
    :export nil)
   (handler-ids
    :documentation "Store all GObject signal handler IDs so that we can
disconnect the signal handler when the object is finalized.")
   (root-box-layout)
   (horizontal-box-layout)
   (left-panel-container)
   (main-buffer-container)
   (right-panel-container)
   (prompt-buffer-container)
   (prompt-buffer-view
    :documentation "A web view shared by all prompt buffers of this window.
This is done so that the UI is computed efficiently.")
   (prompt-view-handler-ids
    nil
    :export nil
    :documentation "Signal handler IDs connected to `prompt-buffer-view'.")
   (prompt-view-warm-p
    nil
    :type boolean
    :export nil
    :documentation "Whether `prompt-buffer-view' has loaded its fonts.")
   (status-initialized-p
    nil
    :type boolean
    :export nil
    :documentation "Whether initial status and message content have been rendered.")
   (status-container)
   (message-container)
   (key-string-buffer))
  (:export-class-name-p t)
  (:export-accessor-names-p nil)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "WebKit window class."))

(define-class gtk-buffer ()
  ((gtk-object)
   (modifier-plist
    '(:control-mask "control"
      :mod1-mask "meta"
      :mod5-mask nil
      :shift-mask "shift"
      :super-mask "super"
      :hyper-mask "hyper"
      :meta-mask nil
      :lock-mask nil)
    :type list
    :documentation "A map between GTK's and Nyxt's terminology for modifier keys.
Note that by changing the default value, modifier keys can be remapped.")
   (handler-ids
    :export nil
    :documentation "Store all GObject signal handler IDs so that we can
disconnect the signal handler when the object is finalized.")
   (handle-permission-requests-p
    nil
    :documentation "Whether permission requests are handled.
When non-nil, they are handled by `process-permission-request'.  Otherwise, all
requests are denied.")
   (provisional-load-p
    nil
    :type boolean
    :export nil
    :documentation "Whether a navigation requested on this buffer has not yet committed.")
   (blocked-hosts
    nil
    :export nil
    :documentation "Normalized host names this buffer refuses to load.")
   (blocked-host-set
    nil
    :export nil
    :documentation "Equal table of `blocked-hosts' for an exact match."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "WebKit buffer class."))

(defmethod input-modifier-translator ((buffer gtk-buffer) input-event-modifier-state)
  "Return a list of modifier keys understood by `keymaps:make-key'."
  (when-let ((state input-event-modifier-state))
    (delete nil
            (mapcar (lambda (modifier) (getf (modifier-plist buffer) modifier)) state))))

(defclass webkit-website-data-manager (webkit:webkit-website-data-manager) ()
  (:metaclass gobject:gobject-class))

(defvar gtk-running-p nil
  "Non-nil if the GTK main loop is running.
See `ffi-initialize' and `ffi-kill-browser'.

Restarting GTK within the same Lisp image breaks WebKitGTK.
As a workaround, we never leave the GTK main loop when running from a REPL.

See https://github.com/atlas-engineer/nyxt/issues/740")

(defmacro within-gtk-thread (&body body)
  "Protected `gtk:within-gtk-thread'."
  `(gtk:within-gtk-thread
     (with-protect ("Error on GTK thread: ~a" :condition)
       ,@body)))

(defmethod ffi-within-renderer-thread (thunk)
  (within-gtk-thread (funcall thunk)))

(defun %within-renderer-thread (thunk)
  "If the current thread is the renderer thread, execute THUNK with `funcall'.
Otherwise run the THUNK on the renderer thread by passing it a channel and wait on the channel's result."
  (if (renderer-thread-p nyxt::*renderer*)
      (funcall thunk)
      (let ((channel (nyxt::make-channel 1)))
        (within-gtk-thread
          (funcall thunk channel))
        (lparallel.queue:pop-queue channel))))

(defun %within-renderer-thread-async (thunk)
  "Same as `%within-renderer-thread' but THUNK is not blocking and does
not return."
  (if (renderer-thread-p nyxt::*renderer*)
      (funcall thunk)
      (within-gtk-thread
        (funcall thunk))))

(export-always 'define-ffi-method)
(defmacro define-ffi-method (name args &body body)
  "Define an FFI method to run in the renderer thread.

Return the value or forward the condition retrieved from the renderer thread,
using a channel if the current thread is not the renderer one.

It's a `defmethod' wrapper. If you don't need the body of the method to execute in
the renderer thread, use `defmethod' instead."
  (multiple-value-bind (forms declares docstring)
      (alex:parse-body body :documentation t)
    (alex:with-gensyms (ffi-body)
      `(defmethod ,name ,args
         ,@(sera:unsplice docstring)
         ,@declares
         (flet ((,ffi-body ()
                  ,@forms))
           (if (renderer-thread-p nyxt::*renderer*)
               (,ffi-body)
               (let ((channel (nyxt::make-channel 1)))
                 (within-gtk-thread
                   ;; TODO: Abstract this into `with-protect-from-thread'?
                   (if (or nyxt::*run-from-repl-p* nyxt::*restart-on-error*)
                       (let ((current-condition nil))
                         (restart-case
                             (handler-bind ((condition (lambda (c) (setf current-condition c))))
                               (lparallel.queue:push-queue (cons t (,ffi-body)) channel))
                           (abort-ffi-method ()
                             :report "Pass condition to calling thread."
                             (lparallel.queue:push-queue (cons nil current-condition) channel))))
                       (handler-case (lparallel.queue:push-queue (cons t (,ffi-body)) channel)
                         (condition (c)
                           (lparallel.queue:push-queue (cons nil c) channel)))))
                 (destructuring-bind (success . value) (lparallel.queue:pop-queue channel)
                   (if success
                       value
                       (with-protect ("Error in FFI method: ~a" :condition)
                         (error value)))))))))))

(-> parse-srgb-color (string) (or null gdk:gdk-rgba))
(defun parse-srgb-color (color)
  "Parse a CSS `color(srgb R G B ...)' string into a `gdk:gdk-rgba', or NIL."
  (let ((trimmed (str:trim color)))
    (when (and (str:starts-with-p "color(srgb " trimmed :ignore-case t)
               (str:ends-with-p ")" trimmed))
      (let* ((body (subseq trimmed (length "color(srgb ") (1- (length trimmed))))
             (parts (str:split "/" body))
             (tokens (sera:tokens (first parts)))
             (alpha-token (and (second parts) (str:trim (second parts))))
             (alpha (if (and alpha-token (plusp (length alpha-token)))
                        (uiop:safe-read-from-string alpha-token)
                        1.0)))
        (when (and (= (length tokens) 3) (realp alpha))
          (let ((channels (mapcar #'uiop:safe-read-from-string tokens)))
            (when (every #'realp channels)
              (gdk:make-gdk-rgba
               :red (alex:clamp (coerce (first channels) 'double-float) 0.0d0 1.0d0)
               :green (alex:clamp (coerce (second channels) 'double-float) 0.0d0 1.0d0)
               :blue (alex:clamp (coerce (third channels) 'double-float) 0.0d0 1.0d0)
               :alpha (alex:clamp (coerce alpha 'double-float) 0.0d0 1.0d0)))))))))

(-> css-color->gdk-rgba (string) (or null gdk:gdk-rgba))
(defun css-color->gdk-rgba (color)
  "Return COLOR as a `gdk:gdk-rgba', or NIL when COLOR cannot be parsed."
  (when (and (stringp color) (plusp (length color)))
    (or (when-let ((rgb (ignore-errors (cl-colors-ng:as-rgb color))))
          (gdk:make-gdk-rgba
           :red (alex:clamp (coerce (cl-colors-ng:rgb-red rgb) 'double-float) 0.0d0 1.0d0)
           :green (alex:clamp (coerce (cl-colors-ng:rgb-green rgb) 'double-float) 0.0d0 1.0d0)
           :blue (alex:clamp (coerce (cl-colors-ng:rgb-blue rgb) 'double-float) 0.0d0 1.0d0)
           :alpha 1.0d0))
        (parse-srgb-color color)
        (ignore-errors (gdk:gdk-rgba-parse color)))))

(-> canvas-color (t) (or null gdk:gdk-rgba))
(defun canvas-color (buffer)
  "Return the color shown before BUFFER's document paints.
A web buffer with `nyxt/mode/style:dark-mode' uses #222222, which is the color
that mode paints. Every other buffer uses the theme background."
  (when (and *browser* (theme *browser*))
    (css-color->gdk-rgba
     (if (and (web-buffer-p buffer)
              (find "DARK-MODE" (default-modes buffer)
                    :key #'symbol-name :test #'string=))
         "#222222"
         (theme:background-color (theme *browser*))))))

(defun pixbuf-from-octets (octets)
  "Decode PNG OCTETS into a `gdk-pixbuf:gdk-pixbuf', or NIL."
  (ignore-errors
    (let ((loader (gdk-pixbuf:gdk-pixbuf-loader-new)))
      (unwind-protect
           (when (gdk-pixbuf:gdk-pixbuf-loader-write loader octets (length octets))
             (gdk-pixbuf:gdk-pixbuf-loader-close loader)
             (gdk-pixbuf:gdk-pixbuf-loader-get-pixbuf loader))
        (ignore-errors (gdk-pixbuf:gdk-pixbuf-loader-close loader))))))

(-> install-window-icon () (values boolean &optional))
(defun install-window-icon ()
  "Set Nyxt's icons as the default GTK window icon list."
  (ignore-errors (gtk:gtk-window-set-default-icon-name "nyxt"))
  (let ((icons (loop for name in '("nyxt_16x16.png"
                                   "nyxt_32x32.png"
                                   "nyxt_128x128.png"
                                   "nyxt_256x256.png"
                                   "nyxt_512x512.png")
                     for png = (gethash name *static-data*)
                     for pixbuf = (and png (plusp (length png)) (pixbuf-from-octets png))
                     when pixbuf collect pixbuf)))
    (when icons
      (ignore-errors (gtk:gtk-window-set-default-icon-list icons))
      t)))

(defvar *exit-lock* (bt:make-lock "nyxt-gtk-exit"))
(defvar *exit-requested* nil)

(defun request-exit ()
  "Quit Nyxt from a dedicated thread outside the signal handler."
  (bt:with-lock-held (*exit-lock*)
    (when *exit-requested*
      (return-from request-exit nil))
    (setf *exit-requested* t))
  (bt:make-thread
   (lambda ()
     (ignore-errors (nyxt:quit))
     (unless nyxt::*run-from-repl-p*
       (uiop:quit 0 nil)))
   :name "nyxt shutdown"))

#+sbcl
(defun install-exit-signals ()
  "Exit through `quit' when the process receives SIGINT or SIGTERM."
  (flet ((shutdown (&rest ignored)
           (declare (ignore ignored))
           (request-exit)))
    (sb-sys:enable-interrupt sb-unix:sigint #'shutdown)
    (sb-sys:enable-interrupt sb-unix:sigterm #'shutdown)))

#-sbcl
(defun install-exit-signals ()
  nil)

(defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
  "Initialize GTK on its dedicated renderer thread and finalize startup."
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing GTK Interface")
  (labels ((mask-float-traps ()
             #+sbcl
             (sb-int:set-floating-point-modes :traps nil))
           (start-in-renderer ()
             (mask-float-traps)
             (install-window-icon)
             (call-next-method))
           (main-func ()
             (mask-float-traps)
             (with-protect ("Error on GTK thread: ~a" :condition)
               (glib:g-set-prgname "nyxt")
               (gdk:gdk-set-program-class "Nyxt")
               (cffi:foreign-funcall "gtk_main" :void))))
    (mask-float-traps)
    (unless nyxt::*run-from-repl-p*
      (install-exit-signals))
    (if gtk-running-p
        (within-gtk-thread (start-in-renderer))
        (progn
          (setf gtk-running-p t)
          (within-gtk-thread (start-in-renderer))
          (let ((main-thread (bt:make-thread #'main-func :name renderer-thread-name)))
            (unless nyxt::*run-from-repl-p*
              (bt:join-thread main-thread)
              (uiop:quit (nyxt:exit-code browser) nil)))))))

(define-ffi-method ffi-kill-browser ((browser gtk-browser))
  (dolist (buffer (alex:hash-table-values (buffers browser)))
    (ignore-errors (ffi-buffer-delete buffer)))
  (unless nyxt::*run-from-repl-p*
    (gtk:gtk-main-quit)))

(define-ffi-method ffi-clear-cache ((browser gtk-browser))
  (maphash (lambda (name context)
             (declare (ignore name))
             (webkit:webkit-web-context-clear-cache context))
           (web-contexts browser))
  t)

(define-class gtk-extensions-directory (nyxt-file)
  ((files:name "gtk-extensions")
   (files:base-path (uiop:merge-pathnames* "nyxt/" nasdf:*libdir*)))
  (:export-class-name-p t)
  (:documentation "Directory to load WebKitWebExtensions from."))

(define-class gtk-download ()
  ((gtk-object)
   (handler-ids
    :export nil
    :documentation "See `gtk-buffer' slot of the same name."))
  (:documentation "WebKit download class."))

(defun make-web-context ()
  (let* ((context (make-instance 'webkit:webkit-web-context
                                 :website-data-manager
                                 (make-instance 'webkit-website-data-manager)))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context))
         (gtk-extensions-path (files:expand (make-instance 'gtk-extensions-directory))))
    (webkit:webkit-web-context-set-spell-checking-enabled context t)
    ;; Need to set the initial language list.
    (let ((pointer (cffi:foreign-alloc :string
                                       :initial-contents (list (or (uiop:getenv "LANG")
                                                                   (uiop:getenv "LANGUAGE")
                                                                   (uiop:getenv "LC_CTYPE")
                                                                   "en_US"))
                                       :null-terminated-p t)))
      (webkit:webkit-web-context-set-spell-checking-languages context pointer)
      (cffi:foreign-free pointer))
    (when (and (not (nfiles:nil-pathname-p gtk-extensions-path))
               ;; Either the directory exists.
               (or (uiop:directory-exists-p gtk-extensions-path)
                   ;; Or try to create it.
                   (handler-case
                       (nth-value 1 (ensure-directories-exist gtk-extensions-path))
                     (file-error ()))))
      (log:info "GTK extensions directory: ~s" gtk-extensions-path)
      (gobject:g-signal-connect
       context "initialize-web-extensions"
       (lambda (context)
         (with-protect ("Error in \"initialize-web-extensions\" signal thread: ~a" :condition)
           ;; The following calls
           ;; `webkit:webkit-web-context-add-path-to-sandbox' for us, so no need
           ;; to add `gtk-extensions-path' to the sandbox manually.
           (webkit:webkit-web-context-set-web-extensions-directory
            context
            (uiop:native-namestring gtk-extensions-path))))))
    (gobject:g-signal-connect
     context "download-started"
     (lambda (context download)
       (declare (ignore context))
       (with-protect ("Error in \"download-started\" signal thread: ~a" :condition)
         (wrap-download download))))
    (maphash (lambda (scheme-name callbacks)
               (ffi-register-custom-scheme (make-instance 'scheme
                                                          :name scheme-name
                                                          :web-context context
                                                          :callback (first callbacks)
                                                          :error-callback (second callbacks))))
             nyxt::*schemes*)
    (webkit:webkit-cookie-manager-set-persistent-storage
     cookie-manager
     (uiop:native-namestring (files:expand (make-instance 'nyxt-data-directory
                                                          :base-path "cookies")))
     :webkit-cookie-persistent-storage-text)
    (setf (ffi-buffer-cookie-policy cookie-manager) (default-cookie-policy *browser*))
    context))

(define-class gtk-request-data ()
  ((gtk-request
    :type (maybe webkit:webkit-uri-request))
   (gtk-response
    :type (maybe webkit:webkit-uri-response))
   (gtk-resource
    :type (maybe webkit:webkit-web-resource)))
  (:export-class-name-p t)
  ;; We export these accessors because it can be useful to inspect the guts of a
  ;; request, plus the upstream WebKit API is stable enough.
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Related to WebKit's request objects."))

(defun make-decide-policy-handler (buffer)
  (lambda (web-view response-policy-decision policy-decision-type-response)
    (declare (ignore web-view))
    (if (and (request-resource-hook buffer)
             (hooks:handlers (request-resource-hook buffer)))
        (run-thread "asynchronous decide-policy processing"
          (handler-bind ((error (lambda (c)
                                  (echo-warning "decide policy error: ~a" c)
                                  (invoke-restart 'ignore-policy-decision))))
            (restart-case (on-signal-decide-policy buffer response-policy-decision policy-decision-type-response)
              (ignore-policy-decision ()
                (setf (provisional-load-p buffer) nil)
                (within-gtk-thread
                  (webkit:webkit-policy-decision-ignore response-policy-decision))))))
        (handler-bind ((error (lambda (c)
                                (echo-warning "decide policy error: ~a" c)
                                (invoke-restart 'ignore-policy-decision))))
          (restart-case (on-signal-decide-policy buffer response-policy-decision policy-decision-type-response)
            (ignore-policy-decision ()
              (setf (provisional-load-p buffer) nil)
              (webkit:webkit-policy-decision-ignore response-policy-decision)))))
    t))

(defmacro connect-signal-function (object signal fn)
  "Connect SIGNAL to OBJECT with a function FN.
OBJECT must have the `gtk-object' and `handler-ids' slots.
See also `connect-signal'."
  `(let ((handler-id (gobject:g-signal-connect
                      (gtk-object ,object) ,signal ,fn)))
     (push handler-id (handler-ids ,object))))

(defmacro connect-signal (object signal new-thread-p (&rest args) &body body)
  "Connect SIGNAL to OBJECT with a lambda that takes ARGS.
OBJECT must have the `gtk-object' and `handler-ids' slots. If
`new-thread-p' is non-nil, then a new thread will be launched for the
response.  The BODY is wrapped with `with-protect'."
  (multiple-value-bind (forms declares documentation)
      (alex:parse-body body :documentation t)
    `(let ((handler-id (gobject:g-signal-connect
                        (gtk-object ,object) ,signal
                        (lambda (,@args)
                          ,@(sera:unsplice documentation)
                          ,@declares
                          ,(if new-thread-p
                               `(run-thread "renderer signal handler"
                                    ,@forms)
                               `(with-protect ("Error in signal on renderer thread: ~a" :condition)
                                  ,@forms))))))
       (push handler-id (handler-ids ,object)))))

(defun configure-web-view (view &optional background-rgba)
  (when view
    (when background-rgba
      (webkit:webkit-web-view-set-background-color view background-rgba))
    (let ((settings (webkit:webkit-web-view-get-settings view)))
      (when (getf *options* :verbose)
        (setf (webkit:webkit-settings-enable-write-console-messages-to-stdout settings) t))
      (setf (webkit:webkit-settings-enable-resizable-text-areas settings) t
            (webkit:webkit-settings-enable-developer-extras settings) t
            (webkit:webkit-settings-enable-page-cache settings) t
            (webkit:webkit-settings-enable-encrypted-media settings) t)))
  view)

(defun restrict-auxiliary-context-menu (context-menu)
  (let ((items (loop for i below (webkit:webkit-context-menu-get-n-items context-menu)
                     collect (webkit:webkit-context-menu-get-item-at-position context-menu i))))
    (dolist (item items)
      (unless (eq (webkit:webkit-context-menu-item-get-stock-action item)
                  :webkit-context-menu-action-inspect-element)
        (webkit:webkit-context-menu-remove context-menu item)))))

(defun auxiliary-decide-policy (web-view decision decision-type)
  (declare (ignore web-view))
  (if (member decision-type '(:webkit-policy-decision-type-navigation-action
                              :webkit-policy-decision-type-new-window-action))
      (let* ((action (webkit:webkit-navigation-policy-decision-get-navigation-action decision))
             (request (webkit:webkit-navigation-action-get-request action))
             (uri (or (webkit:webkit-uri-request-uri request) "")))
        (if (or (uiop:emptyp uri) (string= uri "about:blank"))
            (webkit:webkit-policy-decision-use decision)
            (webkit:webkit-policy-decision-ignore decision)))
      (webkit:webkit-policy-decision-use decision))
  t)

(defun set-container-bar-height (container height)
  (when container
    (cond
      ((plusp height)
       (setf (gtk:gtk-widget-height-request container) height)
       (gtk:gtk-widget-show-all container))
      (t
       (setf (gtk:gtk-widget-height-request container) 0)
       (gtk:gtk-widget-hide container))))
  height)

(defun container-bar-height (container)
  (if (and container (gtk:gtk-widget-visible container))
      (max 0 (gtk:gtk-widget-height-request container))
      0))

(defun detach-widget (widget)
  (when widget
    (when-let ((parent (gtk:gtk-widget-parent widget)))
      (gtk:gtk-container-remove parent widget))))

(defun ensure-prompt-view-listeners (window)
  (with-slots (prompt-buffer-view prompt-view-handler-ids) window
    (when (and prompt-buffer-view (null prompt-view-handler-ids))
      (flet ((connect-prompt (signal fn)
               (push (gobject:g-signal-connect prompt-buffer-view signal fn)
                     prompt-view-handler-ids)))
        (connect-prompt
         "focus-in-event"
         (lambda (widget event)
           (declare (ignore widget event))
           (with-protect ("Error in signal on renderer thread: ~a" :condition)
             (when-let ((prompt (first (nyxt::active-prompt-buffers window))))
               (on-signal-focus prompt)))
           nil))
        (connect-prompt
         "key_press_event"
         (lambda (widget event)
           (declare (ignore widget))
           (with-protect ("Error in signal on renderer thread: ~a" :condition)
             (when-let ((prompt (first (nyxt::active-prompt-buffers window))))
               (on-signal-key-press-event prompt event)))))
        (connect-prompt
         "button-press-event"
         (lambda (widget event)
           (declare (ignore widget))
           (with-protect ("Error in signal on renderer thread: ~a" :condition)
             (when-let ((prompt (first (nyxt::active-prompt-buffers window))))
               (on-signal-button-press-event prompt event)))))
        (connect-prompt
         "scroll-event"
         (lambda (widget event)
           (declare (ignore widget))
           (with-protect ("Error in signal on renderer thread: ~a" :condition)
             (when-let ((prompt (first (nyxt::active-prompt-buffers window))))
               (on-signal-scroll-event prompt event)))))
        (connect-prompt
         "context-menu"
         (lambda (web-view context-menu event hit-test-result)
           (declare (ignore web-view event hit-test-result))
           (with-protect ("Error in signal on renderer thread: ~a" :condition)
             (restrict-auxiliary-context-menu context-menu))
           nil))
        (connect-prompt "decide-policy" #'auxiliary-decide-policy)))
    prompt-buffer-view))

(defun warm-prompt-view (window)
  "Load the prompt fonts in WINDOW's prompt view while it is hidden."
  (with-slots (prompt-buffer-view prompt-view-warm-p) window
    (unless (or prompt-view-warm-p
                (nyxt::active-prompt-buffers window)
                (null prompt-buffer-view))
      (setf prompt-view-warm-p t)
      (handler-bind ((error (lambda (condition)
                              (declare (ignore condition))
                              (return-from warm-prompt-view nil))))
        (let* ((font-css
                 (uiop:strcat
                  (lass:compile-and-write
                   '(:font-face
                     :font-family "Public Sans"
                     :font-style "normal"
                     :font-weight "400"
                     :src "url('nyxt-resource:PublicSans-Regular.woff')" "format('woff')"))
                  (lass:compile-and-write
                   '(:font-face
                     :font-family "DejaVu Sans Mono"
                     :src "url('nyxt-resource:DejaVuSansMono.ttf')" "format('ttf')"))
                  (lass:compile-and-write
                   '(body :font-family "'Public Sans',sans-serif"))
                  (lass:compile-and-write
                   '(span :font-family "'DejaVu Sans Mono'"))))
               (html
                 (spinneret:with-html-string
                   (:head (:style (:raw font-css)))
                   (:body (:span "0")))))
          (webkit2:webkit-web-view-evaluate-javascript
           prompt-buffer-view
           (ps:ps
             (setf (ps:@ document document-element |innerHTML|)
                   (ps:lisp html)))))))))

(define-ffi-method initialize-gtk-window ((window gtk-window))
  (with-slots (gtk-object root-box-layout horizontal-box-layout
               left-panel-container main-buffer-container right-panel-container
               prompt-buffer-container prompt-buffer-view
               status-buffer status-container
               message-buffer message-container
               key-string-buffer)
      window
    (unless gtk-object
      (setf gtk-object (make-instance 'gtk:gtk-window
                                      :type :toplevel
                                      :default-width 1600
                                      :default-height 1200)
            root-box-layout (make-instance 'gtk:gtk-box
                                           :orientation :vertical)
            horizontal-box-layout (make-instance 'gtk:gtk-box
                                                 :orientation :horizontal)
            left-panel-container (make-instance 'gtk:gtk-box
                                                :orientation :horizontal)
            main-buffer-container (make-instance 'gtk:gtk-box
                                                 :orientation :vertical)
            right-panel-container (make-instance 'gtk:gtk-box
                                                 :orientation :horizontal)
            prompt-buffer-container (make-instance 'gtk:gtk-box
                                                   :orientation :vertical)
            message-container (make-instance 'gtk:gtk-box
                                             :orientation :vertical)
            status-container (make-instance 'gtk:gtk-box
                                            :orientation :vertical)
            key-string-buffer (make-instance 'gtk:gtk-entry))
      (gtk:gtk-box-pack-start horizontal-box-layout
                              left-panel-container
                              :expand nil :fill t)
      (gtk:gtk-box-pack-start horizontal-box-layout
                              main-buffer-container
                              :expand t :fill t)
      (gtk:gtk-box-pack-start horizontal-box-layout
                              right-panel-container
                              :expand nil :fill t)
      (gtk:gtk-box-pack-start root-box-layout
                              horizontal-box-layout
                              :expand t :fill t)
      (gtk:gtk-box-pack-end root-box-layout
                            message-container
                            :expand nil)
      (gtk:gtk-box-pack-start message-container
                              (gtk-object message-buffer)
                              :expand t :fill t)
      (gtk:gtk-box-pack-end root-box-layout
                            status-container
                            :expand nil)
      (gtk:gtk-box-pack-start status-container
                              (gtk-object status-buffer)
                              :expand t :fill t)
      (setf prompt-buffer-view
            (configure-web-view
             (make-instance 'webkit:webkit-web-view
                            :web-context (get-web-context *browser* "default"))
             (canvas-color *browser*)))
      (gtk:gtk-box-pack-end root-box-layout
                            prompt-buffer-container
                            :expand nil)
      (gtk:gtk-box-pack-start prompt-buffer-container
                              prompt-buffer-view
                              :expand t :fill t)
      (gtk:gtk-container-add gtk-object root-box-layout)
      (ensure-prompt-view-listeners window)
      (connect-signal window "delete-event" nil (widget event)
        (declare (ignore widget event))
        (run-thread "window-close"
          (ffi-window-delete window))
        t)
      (connect-signal window "destroy" nil (widget)
        (declare (ignore widget))
        (on-signal-destroy window))
      (connect-signal window "window-state-event" nil (widget event)
        (declare (ignore widget))
        (let ((fullscreen-p)
              (maximized-p))
          (dolist (state (gdk:gdk-event-window-state-new-window-state event))
            (case state
              (:fullscreen
               (setq fullscreen-p t)
               (ffi-window-fullscreen window :user-event-p nil))
              (:maximized
               (setq maximized-p t)
               (ffi-window-maximize window :user-event-p nil))))
          (unless fullscreen-p (ffi-window-unfullscreen window :user-event-p nil))
          (unless maximized-p (ffi-window-unmaximize window :user-event-p nil)))
        nil)
      (gtk:gtk-widget-show-all gtk-object)
      (set-container-bar-height message-container (height message-buffer))
      (set-container-bar-height status-container (height status-buffer))
      (set-container-bar-height prompt-buffer-container 0)
      (setf (slot-value window 'status-initialized-p) t)
      (nyxt::print-status status-buffer)
      (ffi-print-message message-buffer "Ready.")
      (warm-prompt-view window)))
  window)

(defmethod customize-instance :after ((window gtk-window) &key)
  (initialize-gtk-window window))

(defmethod update-instance-for-redefined-class :after ((window window) added deleted plist &key)
  (declare (ignore added deleted plist))
  (customize-instance window))

(define-ffi-method on-signal-destroy ((window gtk-window))
  (when-let ((buffer (buffer window)))
    (detach-widget (gtk-object buffer))))

(define-ffi-method ffi-window-delete ((window gtk-window))
  (with-slots (gtk-object handler-ids prompt-buffer-view prompt-view-handler-ids
               status-buffer message-buffer)
      window
    (when gtk-object
      (when-let ((page (buffer window)))
        (detach-widget (gtk-object page)))
      (dolist (panel (panel-buffers window))
        (detach-widget (gtk-object panel)))
      (dolist (prompt (nyxt::active-prompt-buffers window))
        (when prompt
          (setf (gtk-object prompt) nil)))
      (setf (nyxt::active-prompt-buffers window) '())
      (when prompt-buffer-view
        (dolist (id prompt-view-handler-ids)
          (ignore-errors (gobject:g-signal-handler-disconnect prompt-buffer-view id)))
        (setf prompt-view-handler-ids nil))
      (dolist (aux (list status-buffer message-buffer))
        (when (and aux (gtk-object aux))
          (dolist (id (handler-ids aux))
            (ignore-errors (gobject:g-signal-handler-disconnect (gtk-object aux) id)))
          (setf (handler-ids aux) nil
                (gtk-object aux) nil)))
      (dolist (id handler-ids)
        (ignore-errors (gobject:g-signal-handler-disconnect gtk-object id)))
      (setf handler-ids nil)
      (gtk:gtk-widget-destroy gtk-object)
      (setf gtk-object nil
            prompt-buffer-view nil))))

(define-ffi-method ffi-window-fullscreen ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-fullscreen (gtk-object window)))

(define-ffi-method ffi-window-unfullscreen ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-unfullscreen (gtk-object window)))

(define-ffi-method ffi-window-maximize ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-maximize (gtk-object window)))

(define-ffi-method ffi-window-unmaximize ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-unmaximize (gtk-object window)))

(defun derive-key-string (keyval character)
  "Return string representation of a keyval.
Return nil when key must be discarded, e.g. for modifiers."
  (let ((result
          (match keyval
            ((or "Alt_L" "Super_L" "Control_L" "Shift_L"
                 "Alt_R" "Super_R" "Control_R" "Shift_R"
                 "ISO_Level3_Shift" "Arabic_switch")
             ;; Discard modifiers (they usually have a null character).
             nil)
            ((guard s (str:contains? "KP_" s))
             (str:replace-all "KP_" "keypad" s))
            ;; With a modifier, "-" does not print, so we me must translate it
            ;; to "hyphen" just like in `printable-p'.
            ("minus" "hyphen")
            ;; shift-tab:
            ("ISO_Left_Tab" "tab")
            ;; In most cases, return character and not keyval for punctuation.
            ;; For instance, C-[ is not printable but the keyval is "bracketleft".
            ;; ASCII control characters like Escape, Delete or BackSpace have a
            ;; non-printable character (usually beneath #\space), so we use the
            ;; keyval in this case.
            ;; Even if space in printable, C-space is not so we return the
            ;; keyval in this case.
            (_ (if (or (char<= character #\space)
                       (char= character #\Del))
                   keyval
                   (string character))))))
    (if (< 1 (length result))
        (str:replace-all "_" "" (string-downcase result))
        result)))

(defmethod printable-p ((window gtk-window) event)
  "Return the printable value of EVENT."
  ;; Generate the result of the current keypress into the dummy
  ;; key-string-buffer (a GtkEntry that's never shown on screen) so that we
  ;; can collect the printed representation of composed keypress, such as dead
  ;; keys.
  (with-slots (key-string-buffer) window
    (when key-string-buffer
      (gtk:gtk-entry-im-context-filter-keypress key-string-buffer event)
      (when (<= 1 (gtk:gtk-entry-text-length key-string-buffer))
        (prog1
            (match (gtk:gtk-entry-text key-string-buffer)
              ;; Special cases: these characters are not supported as is for keyspecs.
              (" " "space")
              ("-" "hyphen")
              (character character))
          (setf (gtk:gtk-entry-text key-string-buffer) ""))))))

(define-ffi-method on-signal-key-press-event ((sender gtk-buffer) event)
  (let* ((keycode (gdk:gdk-event-key-hardware-keycode event))
         (keyval (gdk:gdk-event-key-keyval event))
         (keyval-name (gdk:gdk-keyval-name keyval))
         (character (gdk:gdk-keyval-to-unicode keyval))
         (target-window (or (window sender) (current-window)))
         (printable-value (when target-window (printable-p target-window event)))
         (key-string (or printable-value
                         (derive-key-string keyval-name character)))
         (modifiers (input-modifier-translator sender (gdk:gdk-event-key-state event))))
    (log:debug sender key-string keycode character keyval-name modifiers)
    ;; Do not forward modifier-only presses to the renderer.
    (if key-string
        (flet ((key ()
                 (keymaps:make-key :code keycode
                                   :value (or (ignore-errors (keymaps:unshift key-string))
                                              key-string)
                                   :modifiers modifiers
                                   :status :pressed)))
          (dispatch-key-press sender event (key)))
        t)))

(define-ffi-method on-signal-button-press-event ((sender gtk-buffer) event)
  (let ((key-string (format nil "button~s" (gdk:gdk-event-button-button event)))
        (modifiers (input-modifier-translator sender (gdk:gdk-event-button-state event))))
    (flet ((key ()
             (keymaps:make-key :value key-string
                               :modifiers modifiers
                               :status :pressed)))
      (when key-string
        (dispatch-pointer-press sender event (key))))))

(define-ffi-method on-signal-scroll-event ((sender gtk-buffer) event)
  (let* ((button (match (gdk:gdk-event-scroll-direction event)
                   (:up 4)
                   (:down 5)
                   (:left 6)
                   (:right 7)
                   (:smooth (cond ((>= 0 (gdk:gdk-event-scroll-delta-y event)) 4)
                                  ((< 0 (gdk:gdk-event-scroll-delta-y event)) 5)
                                  ((>= 0 (gdk:gdk-event-scroll-delta-x event)) 6)
                                  ((< 0 (gdk:gdk-event-scroll-delta-x event)) 7)))))
         (key-string (format nil "button~s" button))
         (modifiers (input-modifier-translator sender (gdk:gdk-event-scroll-state event))))
    (when key-string
      (alex:appendf (key-stack sender)
                    (list (keymaps:make-key :value key-string
                                            :modifiers modifiers
                                            :status :pressed)))
      (dispatch-input-event event sender))))

(define-class gtk-scheme ()
  ((web-context
    nil
    :writer nil
    :reader t
    :documentation "See `webkit-web-context'.")
   (local-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages of other URI schemes cannot access URIs of
this scheme.")
   (no-access-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages of this URI scheme cannot access other URI schemes.")
   (secure-p
    nil
    :writer nil
    :reader t
    :documentation "Whether mixed content warnings aren't generated for this
scheme when included by an HTTPS page.

See https://developer.mozilla.org/en-US/docs/Web/Security/Mixed_content.")
   (cors-enabled-p
    nil
    :writer nil
    :reader t
    :documentation "Whether CORS requests are allowed.")
   (display-isolated-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages cannot display URIs unless they are from the
same scheme.
For example, pages in another origin cannot create iframes or hyperlinks to URIs
with this scheme.")
   (empty-document-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages are allowed to be loaded synchronously."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Related to WebKit's custom schemes."))

(defmethod manager ((scheme gtk-scheme))
  (when-let ((context (web-context scheme)))
    (webkit:webkit-web-context-get-security-manager context)))

(defmethod (setf local-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-local (manager scheme)
                                                                 (name scheme)))
  (setf (slot-value scheme 'local-p) value))

(defmethod (setf no-access-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-no-access (manager scheme)
                                                                     (name scheme)))
  (setf (slot-value scheme 'no-access-p) value))

(defmethod (setf secure-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-secure (manager scheme)
                                                                  (name scheme)))
  (setf (slot-value scheme 'secure-p) value))

(defmethod (setf cors-enabled-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-cors-enabled (manager scheme)
                                                                        (name scheme)))
  (setf (slot-value scheme 'cors-enabled-p) value))

(defmethod (setf display-isolated-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-display-isolated (manager scheme)
                                                                            (name scheme)))
  (setf (slot-value scheme 'display-isolated-p) value))

(defmethod (setf empty-document-p) (value (scheme gtk-scheme))
  (when (and value (manager scheme))
    (webkit:webkit-security-manager-register-uri-scheme-as-empty-document (manager scheme)
                                                                          (name scheme)))
  (setf (slot-value scheme 'empty-document-p) value))

(defmethod initialize-instance :after ((scheme gtk-scheme) &key)
  (when (and (null (web-context scheme))
             *browser*
             (typep *browser* 'gtk-browser))
    (setf (slot-value scheme 'web-context)
          (get-web-context *browser* "default")))
  ;; NOTE: No security settings for the nyxt scheme since:
  ;; - :local-p makes it inaccessible from other schemes.
  ;; - :display-isolated-p does not allow embedding a nyxt scheme page inside a
  ;;   page of the same scheme.
  ;; - :secure-p and :cors-enabled-p are too permissive for a scheme that allows
  ;;   evaluating Lisp code.
  ;; Therefore, no settings provide the best configuration so that:
  ;; - <iframe> embedding and exploitation are impossible.
  ;; - Redirection—both as window.location.(href|assign|replace) and
  ;;   HTTP status code 301—works.
  ;; - nyxt scheme pages are linkable from pages of other schemes.
  (match (name scheme)
    ((or "nyxt-resource" "lisp" "nyxt-terminal")
     (setf (secure-p scheme) t
           (cors-enabled-p scheme) t))
    ("nyxt-source" (setf (no-access-p scheme) t))
    (_ t)))

;; From https://github.com/umpirsky/language-list/tree/master/data directory
;; listing $(ls /data) and processed with:
;; (defun iso-languages (languages-file-string)
;;   (remove-if (lambda (lang)
;;                (or (uiop:emptyp lang)
;;                    (/= 1 (count #\_ lang))
;;                    (notevery #'upper-case-p (subseq lang 3))))
;;              (uiop:split-string languages-file-string :separator '(#\Space #\Tab #\Newline))))
(defvar *spell-check-languages*
  (list "be_BY" "en_LC" "en_VG" "ff_MR" "fr_SC" "ln_CF" "nl_BE" "rm_CH" "uk_UA"
        "af_NA" "el_CY" "en_LR" "en_VI" "ff_SN" "fr_SN" "ii_CN" "ln_CG" "nl_BQ"
        "af_ZA" "bg_BG" "el_GR" "en_LS" "en_VU" "fr_SY" "nl_CW" "rn_BI" "ur_IN"
        "en_MG" "en_WS" "fi_FI" "fr_TD" "is_IS" "lo_LA" "nl_NL" "ur_PK" "ak_GH"
        "en_AG" "en_MH" "en_ZA" "fr_TG" "nl_SR" "ro_MD" "en_AI" "en_MO" "en_ZM"
        "fo_FO" "fr_TN" "it_CH" "lt_LT" "nl_SX" "ro_RO" "uz_AF" "am_ET" "en_AS"
        "en_MP" "en_ZW" "fr_VU" "it_IT" "bn_BD" "en_AU" "en_MS" "fr_BE" "fr_WF"
        "it_SM" "lu_CD" "nn_NO" "ru_BY" "ar_AE" "bn_IN" "en_BB" "en_MT" "fr_BF"
        "fr_YT" "ru_KG" "sr_ME" "ar_BH" "en_BE" "en_MU" "es_AR" "fr_BI" "ja_JP"
        "lv_LV" "no_NO" "ru_KZ" "sr_RS" "ar_DJ" "bo_CN" "en_BM" "en_MW" "es_BO"
        "fr_BJ" "fy_NL" "ru_MD" "sr_XK" "ar_DZ" "bo_IN" "en_BS" "en_MY" "es_CL"
        "fr_BL" "ka_GE" "mg_MG" "om_ET" "ru_RU" "ar_EG" "en_BW" "en_NA" "es_CO"
        "fr_CA" "ga_IE" "om_KE" "ru_UA" "sv_AX" "uz_UZ" "ar_EH" "br_FR" "en_BZ"
        "en_NF" "es_CR" "fr_CD" "ki_KE" "mk_MK" "sv_FI" "ar_ER" "en_CA" "en_NG"
        "es_CU" "fr_CF" "gd_GB" "or_IN" "rw_RW" "sv_SE" "vi_VN" "ar_IL" "bs_BA"
        "en_CC" "en_NR" "es_DO" "fr_CG" "ml_IN" "ar_IQ" "en_CK" "en_NU" "es_EA"
        "fr_CH" "gl_ES" "os_GE" "se_FI" "sw_CD" "ar_JO" "en_CM" "en_NZ" "es_EC"
        "fr_CI" "kk_KZ" "os_RU" "se_NO" "sw_KE" "yo_BJ" "ar_KM" "en_CX" "en_PG"
        "es_ES" "fr_CM" "gu_IN" "se_SE" "sw_TZ" "yo_NG" "ar_KW" "en_DG" "en_PH"
        "es_GQ" "fr_DJ" "kl_GL" "mn_MN" "sw_UG" "ar_LB" "en_DM" "en_PK" "es_GT"
        "fr_DZ" "gv_IM" "sg_CF" "zh_CN" "ar_LY" "ca_AD" "en_ER" "en_PN" "es_HN"
        "fr_FR" "km_KH" "mr_IN" "ta_IN" "zh_HK" "ar_MA" "ca_ES" "en_FJ" "en_PR"
        "es_IC" "fr_GA" "ha_GH" "sh_BA" "ta_LK" "ar_MR" "ca_FR" "en_FK" "en_PW"
        "es_MX" "fr_GF" "kn_IN" "ms_BN" "pa_IN" "ta_MY" "ar_OM" "ca_IT" "en_FM"
        "en_RW" "es_NI" "fr_GN" "pa_PK" "si_LK" "ta_SG" "ar_PS" "en_GB" "en_SB"
        "es_PA" "fr_GP" "ko_KP" "ar_QA" "cs_CZ" "en_GD" "en_SC" "es_PE" "fr_GQ"
        "ko_KR" "pl_PL" "sk_SK" "te_IN" "ar_SA" "en_GG" "en_SD" "es_PH" "fr_HT"
        "ha_NE" "ar_SD" "cy_GB" "en_GH" "en_SG" "es_PR" "fr_KM" "ha_NG" "ms_MY"
        "ps_AF" "sl_SI" "th_TH" "ar_SO" "en_GI" "en_SH" "es_PY" "fr_LU" "ms_SG"
        "ar_SS" "da_DK" "en_GM" "en_SL" "es_SV" "fr_MA" "he_IL" "ks_IN" "pt_AO"
        "sn_ZW" "ti_ER" "ar_SY" "da_GL" "en_GU" "en_SS" "es_US" "fr_MC" "mt_MT"
        "pt_BR" "ti_ET" "zh_MO" "ar_TD" "en_GY" "en_SX" "es_UY" "fr_MF" "hi_IN"
        "kw_GB" "pt_CV" "so_DJ" "zh_SG" "ar_TN" "de_AT" "en_HK" "en_SZ" "es_VE"
        "fr_MG" "my_MM" "pt_GW" "so_ET" "tl_PH" "zh_TW" "ar_YE" "de_BE" "en_IE"
        "en_TC" "fr_ML" "hr_BA" "pt_MO" "so_KE" "de_CH" "en_IM" "en_TK" "et_EE"
        "fr_MQ" "hr_HR" "nb_NO" "pt_MZ" "so_SO" "to_TO" "zu_ZA" "as_IN" "de_DE"
        "en_IN" "en_TO" "fr_MR" "ky_KG" "nb_SJ" "pt_PT" "de_LI" "en_IO" "en_TT"
        "eu_ES" "fr_MU" "hu_HU" "pt_ST" "sq_AL" "tr_CY" "az_AZ" "de_LU" "en_JE"
        "en_TV" "fr_NC" "lb_LU" "nd_ZW" "pt_TL" "sq_MK" "tr_TR" "en_JM" "en_TZ"
        "fa_AF" "fr_NE" "hy_AM" "sq_XK" "dz_BT" "en_KE" "en_UG" "fa_IR" "fr_PF"
        "lg_UG" "ne_IN" "qu_BO" "en_KI" "en_UM" "fr_PM" "id_ID" "ne_NP" "qu_EC"
        "sr_BA" "ee_GH" "en_KN" "en_US" "ff_CM" "fr_RE" "ln_AO" "qu_PE" "ug_CN"
        "ee_TG" "en_KY" "en_VC" "ff_GN" "fr_RW" "ig_NG" "ln_CD" "nl_AW")
  "The list of languages available for spell checking in `set-spell-check-languages'.")

(define-command-global set-spell-check-languages
    (buffer &key
     (languages (prompt :prompt "Languages to spell check"
                        :sources (make-instance 'prompter:source
                                                :name "Language codes"
                                                :enable-marks-p t
                                                :constructor *spell-check-languages*))))
  (let ((pointer (cffi:foreign-alloc :string
                                     :initial-contents languages
                                     :null-terminated-p t)))
    (webkit:webkit-web-context-set-spell-checking-languages
     (webkit:webkit-web-view-web-context (gtk-object buffer))
     pointer)
    (cffi:foreign-free pointer)))

(define-ffi-method ffi-register-custom-scheme ((scheme gtk-scheme))
  (let ((contexts (if (web-context scheme)
                      (list (web-context scheme))
                      (when (and *browser* (typep *browser* 'gtk-browser))
                        (alex:hash-table-values (web-contexts *browser*))))))
    (dolist (context contexts)
      (webkit:webkit-web-context-register-uri-scheme-callback
       context
       (name scheme)
       (lambda (request)
         (handler-case
             (if (callback scheme)
                 (multiple-value-bind (data data-type status headers reason)
                     (funcall (callback scheme)
                              (webkit:webkit-uri-scheme-request-get-uri request))
                   (if (and (stringp data) (string= data ""))
                       (values "<html><head></head><body></body></html>"
                               (or data-type "text/html;charset=utf8")
                               (or status 200)
                               headers
                               reason)
                       (values data data-type (or status 200) headers reason)))
                 (values "" "text/plain" 404 nil "Not Found"))
           (condition (condition)
             (when (error-callback scheme)
               (funcall (error-callback scheme) condition))
             (error-help "Scheme error" (princ-to-string condition)))))
       (or (error-callback scheme)
           (lambda (c) (echo-warning "Error while routing ~s resource: ~a" scheme c))))))
  scheme)

(defmethod customize-instance :after ((buffer gtk-buffer) &key &allow-other-keys)
  (ffi-buffer-initialize-foreign-object buffer))

(define-ffi-method ffi-buffer-url ((buffer gtk-buffer))
  (quri:uri (or (and (gtk-object buffer)
                     (webkit:webkit-web-view-uri (gtk-object buffer)))
                "")))

(define-ffi-method ffi-buffer-title ((buffer gtk-buffer))
  (or (and (gtk-object buffer)
           (webkit:webkit-web-view-title (gtk-object buffer)))
      ""))

(define-ffi-method on-signal-load-failed-with-tls-errors ((buffer gtk-buffer) url)
  "Return nil to propagate further (i.e. raise load-failed signal), T otherwise."
  (nyxt::tls-help buffer url)
  t)

(define-ffi-method on-signal-decide-policy ((buffer gtk-buffer) response-policy-decision policy-decision-type-response)
  (block nil
  (let ((is-known-type t) (event-type :other) (modifiers ())
        is-new-window navigation-action navigation-type
        mouse-button url request mime-type method request-headers response-headers
        file-name toplevel-p response)
    (match policy-decision-type-response
      (:webkit-policy-decision-type-navigation-action
       (setf navigation-type
             (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision)))
      (:webkit-policy-decision-type-new-window-action
       (setf navigation-type
             (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision))
       (setf is-new-window t))
      (:webkit-policy-decision-type-response
       (setf request
             (webkit:webkit-response-policy-decision-request response-policy-decision))
       (setf is-known-type
             (webkit:webkit-response-policy-decision-is-mime-type-supported
              response-policy-decision))
       (setf response
             (webkit:webkit-response-policy-decision-response response-policy-decision))
       (setf mime-type
             (webkit:webkit-uri-response-mime-type response))
       (setf method
             (webkit:webkit-uri-request-get-http-method request))
       (setf file-name
             (webkit:webkit-uri-response-suggested-filename response))))
    ;; Set Event-Type
    (setf event-type
          (match navigation-type
            (:webkit-navigation-type-link-clicked :link-click)
            (:webkit-navigation-type-form-submitted :form-submission)
            (:webkit-navigation-type-back-forward :backward-or-forward)
            (:webkit-navigation-type-reload :reload)
            (:webkit-navigation-type-form-resubmitted :form-resubmission)
            (_ :other)))
    ;; Get Navigation Parameters from WebKitNavigationAction object
    (when navigation-type
      (setf navigation-action
            (webkit:webkit-navigation-policy-decision-get-navigation-action
             response-policy-decision))
      (setf request
            (webkit:webkit-navigation-action-get-request navigation-action))
      (setf mouse-button
            (format nil "button~d"
                    (webkit:webkit-navigation-action-get-mouse-button navigation-action)))
      (setf modifiers
            (input-modifier-translator buffer
                                 (webkit:webkit-navigation-action-get-modifiers navigation-action))))
    (setf url (quri:uri (webkit:webkit-uri-request-uri request)))
    (when (nyxt/mode/blocker:host-blocked-p (blocked-host-set buffer) (quri:uri-host url))
      (setf (provisional-load-p buffer) nil)
      (webkit:webkit-policy-decision-ignore response-policy-decision)
      (return nil))
    (setf request-headers
          (let ((headers (webkit:webkit-uri-request-get-http-headers request)))
            (unless (cffi:null-pointer-p headers)
              (webkit:soup-message-headers-get-headers headers))))
    (setf response-headers
          (when response
            (let ((headers (webkit:webkit-uri-response-get-http-headers response)))
              (unless (cffi:null-pointer-p headers)
                (webkit:soup-message-headers-get-headers headers)))))
    (setf toplevel-p
          (quri:uri= url
                     (quri:uri (or (and (gtk-object buffer)
                                        (webkit:webkit-web-view-uri (gtk-object buffer)))
                                   ""))))
    (let* ((request-data
            (hooks:run-hook
             (request-resource-hook buffer)
             (sera:lret ((data (make-instance
                                'request-data
                                :buffer buffer
                                :url (quri:copy-uri url)
                                :keys (unless (uiop:emptyp mouse-button)
                                        (list (keymaps:make-key :value mouse-button
                                                                :modifiers modifiers)))
                                :event-type event-type
                                :new-window-p is-new-window
                                :http-method method
                                :request-headers request-headers
                                :response-headers response-headers
                                :toplevel-p toplevel-p
                                :mime-type mime-type
                                :known-type-p is-known-type
                                :file-name file-name)))
                        (setf (gtk-request data) request
                              (gtk-response data) response))))
           (keymap (when request-data
                     (nyxt::get-keymap (buffer request-data)
                                       (request-resource-keyscheme-map (buffer request-data)))))
           (bound-function (when request-data
                             (the (or symbol keymaps:keymap null)
                                  (keymaps:lookup-key (keys request-data) keymap)))))
      (cond
       ((not (typep request-data 'request-data))
        (log:debug "Don't forward to ~s's renderer (non request data)."
                   buffer)
        (setf (provisional-load-p buffer) nil)
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ;; FIXME: Do we ever use it? Do we actually need it?
       (bound-function
        (log:debug "Resource request key sequence ~a" (keyspecs-with-keycode (keys request-data)))
        (setf (provisional-load-p buffer) nil)
        (webkit:webkit-policy-decision-ignore response-policy-decision)
        (run-thread "request-resource-bound-function"
          (funcall bound-function :url url :buffer buffer)))
       ((new-window-p request-data)
        (log:debug "Load URL in new buffer: ~a" (render-url (url request-data)))
        (setf (provisional-load-p buffer) nil)
        (webkit:webkit-policy-decision-ignore response-policy-decision)
        (let ((target-url (url request-data)))
          (run-thread "new-window-open-url"
            (nyxt::open-urls (list target-url)))))
       ((null (valid-scheme-p (quri:uri-scheme (url request-data))))
        (setf (provisional-load-p buffer) nil)
        (log:warn "Unsupported URI scheme: ~s." (quri:uri-scheme (url request-data))))
       ((not (known-type-p request-data))
        (log:debug "Initiate download of ~s." (render-url (url request-data)))
        (setf (provisional-load-p buffer) nil)
        (webkit:webkit-policy-decision-download response-policy-decision))
       ((quri:uri= url (url request-data))
        (log:debug "Forward to ~s's renderer (unchanged URL)."
                   buffer)
        (webkit:webkit-policy-decision-use response-policy-decision))
       ((and (toplevel-p request-data)
             (not (quri:uri= (quri:uri (webkit:webkit-uri-request-uri request))
                             (url request-data))))
        ;; Low-level URL string, we must not render punycode so use
        ;; `quri:render-uri'.
        ;; See https://datatracker.ietf.org/doc/html/rfc3492.
        (setf (webkit:webkit-uri-request-uri request) (quri:render-uri (url request-data)))
        (log:debug "Don't forward to ~s's renderer (resource request replaced with ~s)."
                   buffer
                   (render-url (url request-data)))
        ;; Warning: We must ignore the policy decision _before_ we
        ;; start the new load request, or else WebKit will be
        ;; confused about which URL to load.
        (webkit:webkit-policy-decision-ignore response-policy-decision)
        (webkit:webkit-web-view-load-request (gtk-object buffer) request))
       (t
        (log:info "Cannot redirect to ~a in an iframe, forwarding to the original URL (~a)."
                  (render-url (url request-data))
                  (webkit:webkit-uri-request-uri request))
        (webkit:webkit-policy-decision-use response-policy-decision)))))))

(defmacro run-buffer-thread (name buffer &body body)
  `(run-thread ,name
     (when (and (gtk-object ,buffer)
                (not (nyxt::dead-buffer-p ,buffer)))
       ,@body)))

;; See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#WebKitLoadEvent
(defmethod on-signal-load-changed ((buffer gtk-buffer) load-event)
  ;; `url' can be nil if buffer didn't have any URL associated
  ;; to the web view, e.g. the start page, or if the load failed.
  (when (and (web-buffer-p buffer) (gtk-object buffer))
    (let* ((raw-uri (webkit:webkit-web-view-uri (gtk-object buffer)))
           (parsed (ignore-errors (quri:uri (or raw-uri ""))))
           (url (if (url-empty-p parsed)
                    (url buffer)
                    parsed)))
      (cond ((eq load-event :webkit-load-started)
             (setf (nyxt::status buffer) :loading)
             (run-buffer-thread "load-started" buffer
               (on-signal-load-started buffer url)
               (unless (internal-url-p url)
                 (echo "Loading ~s." (render-url url)))))
            ((eq load-event :webkit-load-redirected)
             (run-buffer-thread "load-redirected" buffer
               (setf (url buffer) url)
               (on-signal-load-redirected buffer url)))
            ((eq load-event :webkit-load-committed)
             (setf (provisional-load-p buffer) nil)
             (run-buffer-thread "load-committed" buffer
               (on-signal-load-committed buffer url)))
            ((eq load-event :webkit-load-finished)
             (setf (provisional-load-p buffer) nil)
             (unless (eq (slot-value buffer 'nyxt::status) :failed)
               (setf (nyxt::status buffer) :finished))
             (let ((page-title (ffi-buffer-title buffer)))
               (run-buffer-thread "load-finished" buffer
                 (unless (or (nyxt::load-failed-p buffer) (url-empty-p url))
                   (setf (url buffer) url))
                 (on-signal-load-finished buffer url page-title)
                 (unless (internal-url-p url)
                   (echo "Finished loading ~s." (render-url url))))))))))

(define-ffi-method on-signal-mouse-target-changed ((buffer gtk-buffer)
                                                   hit-test-result
                                                   modifiers)
  (declare (ignore modifiers))
  (if-let ((url (or (webkit:webkit-hit-test-result-link-uri hit-test-result)
                    (webkit:webkit-hit-test-result-image-uri hit-test-result)
                    (webkit:webkit-hit-test-result-media-uri hit-test-result))))
    (progn
      (when-let ((window (window buffer)))
        (nyxt::print-message (plump:encode-entities (str:concat "→ " (render-url url)))
                             window))
      (setf (url-at-point buffer) (quri:uri url)))
    (progn
      (when-let ((window (window buffer)))
        (nyxt::print-message "" window))
      (setf (url-at-point buffer) (quri:uri "")))))

(define-ffi-method ffi-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (when (slot-value window 'gtk-object)
    (gtk:gtk-window-present (slot-value window 'gtk-object)))
  (call-next-method))

(define-ffi-method ffi-window-title ((window gtk-window))
  (if (slot-value window 'gtk-object)
      (or (gtk:gtk-window-title (slot-value window 'gtk-object)) "")
      ""))
(define-ffi-method (setf ffi-window-title) (title (window gtk-window))
  (when (slot-value window 'gtk-object)
    (setf (gtk:gtk-window-title (slot-value window 'gtk-object)) title)))

(define-ffi-method ffi-window-active ((browser gtk-browser))
  "Return the focused window."
  (or (find-if (lambda (win)
                 (and (slot-value win 'gtk-object)
                      (gtk:gtk-window-is-active (slot-value win 'gtk-object))))
               (window-list))
      (call-next-method)))

(define-ffi-method ffi-add-panel-buffer ((window gtk-window) (buffer gtk-buffer) side width)
  (let ((target (ecase side
                  (:left (slot-value window 'left-panel-container))
                  (:right (slot-value window 'right-panel-container))))
        (widget (gtk-object buffer)))
    (when (and target widget)
      (let ((parent (gtk:gtk-widget-parent widget)))
        (unless (eq parent target)
          (when parent
            (gtk:gtk-container-remove parent widget))
          (gtk:gtk-box-pack-start target widget :expand nil :fill t)))
      (setf (gtk:gtk-widget-width-request widget) (max 0 width)
            (gtk:gtk-widget-height-request widget) -1)
      (gtk:gtk-widget-show target)
      (gtk:gtk-widget-show widget)))
  buffer)

(define-ffi-method ffi-remove-panel-buffer ((window gtk-window) (buffer gtk-buffer))
  (declare (ignore window))
  (when-let ((widget (gtk-object buffer)))
    (detach-widget widget)
    (gtk:gtk-widget-hide widget))
  buffer)

(define-ffi-method ffi-window-set-buffer ((window gtk-window) (buffer gtk-buffer) &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (when-let ((buried-buffer (and (buffer window) (gtk-object (buffer window)))))
    (unless (eq buried-buffer (gtk-object buffer))
      (detach-widget buried-buffer)))
  (when-let ((widget (gtk-object buffer)))
    (let ((container (slot-value window 'main-buffer-container)))
      (unless (eq (gtk:gtk-widget-parent widget) container)
        (detach-widget widget)
        (gtk:gtk-box-pack-start container widget :expand t :fill t)))
    (gtk:gtk-widget-show widget))
  (unless (slot-value window 'status-initialized-p)
    (setf (slot-value window 'status-initialized-p) t)
    (nyxt::print-status (status-buffer window))
    (ffi-print-message (message-buffer window) "Ready."))
  (when focus (ffi-focus-buffer buffer))
  buffer)

(define-ffi-method ffi-height ((buffer prompt-buffer))
  (if-let ((window (window buffer)))
    (container-bar-height (slot-value window 'prompt-buffer-container))
    0))
(define-ffi-method (setf ffi-height) ((height integer) (buffer prompt-buffer))
  (when-let ((window (window buffer)))
    (set-container-bar-height (slot-value window 'prompt-buffer-container) height)))

(define-ffi-method ffi-focus-buffer ((buffer prompt-buffer))
  (when-let ((view (or (gtk-object buffer)
                       (and (window buffer)
                            (slot-value (window buffer) 'prompt-buffer-view)))))
    (gtk:gtk-widget-grab-focus view))
  buffer)

(define-ffi-method ffi-focused-p ((buffer prompt-buffer))
  (when-let ((view (or (gtk-object buffer)
                       (and (window buffer)
                            (slot-value (window buffer) 'prompt-buffer-view)))))
    (gtk:gtk-widget-is-focus view)))

(define-ffi-method ffi-focus-buffer ((buffer gtk-buffer))
  "Focus BUFFER in WINDOW."
  (when-let ((view (gtk-object buffer)))
    (gtk:gtk-widget-grab-focus view))
  buffer)

(define-ffi-method ffi-height ((buffer status-buffer))
  (if-let ((window (window buffer)))
    (container-bar-height (slot-value window 'status-container))
    0))
(define-ffi-method (setf ffi-height) ((height integer) (buffer status-buffer))
  (when-let ((window (window buffer)))
    (set-container-bar-height (slot-value window 'status-container) height)))

(define-ffi-method ffi-height ((buffer message-buffer))
  (if-let ((window (window buffer)))
    (container-bar-height (slot-value window 'message-container))
    0))
(define-ffi-method (setf ffi-height) ((height integer) (buffer message-buffer))
  (when-let ((window (window buffer)))
    (set-container-bar-height (slot-value window 'message-container) height)))

(defun get-bounds (object)
  (when-let ((widget (slot-value object 'gtk-object)))
    (gtk:gtk-widget-get-allocation widget)))

(define-ffi-method ffi-height ((buffer gtk-buffer))
  (if-let ((bounds (get-bounds buffer)))
    (gdk:gdk-rectangle-height bounds)
    0))
(define-ffi-method (setf ffi-height) ((height integer) (buffer gtk-buffer))
  (when-let ((widget (gtk-object buffer)))
    (setf (gtk:gtk-widget-height-request widget) height))
  height)
(define-ffi-method ffi-width ((buffer gtk-buffer))
  (if-let ((bounds (get-bounds buffer)))
    (gdk:gdk-rectangle-width bounds)
    0))
(define-ffi-method (setf ffi-width) ((width integer) (buffer gtk-buffer))
  (when-let ((widget (gtk-object buffer)))
    (setf (gtk:gtk-widget-width-request widget) width))
  width)

(define-ffi-method ffi-height ((window gtk-window))
  (if-let ((win (slot-value window 'gtk-object)))
    (or (nth-value 1 (gtk:gtk-window-get-size win)) 0)
    0))
(define-ffi-method (setf ffi-height) ((height integer) (window gtk-window))
  (when-let ((win (slot-value window 'gtk-object)))
    (let ((width (or (gtk:gtk-window-get-size win) 1600)))
      (gtk:gtk-window-resize win width (max 1 height))))
  height)
(define-ffi-method ffi-width ((window gtk-window))
  (if-let ((win (slot-value window 'gtk-object)))
    (or (gtk:gtk-window-get-size win) 0)
    0))
(define-ffi-method (setf ffi-width) ((width integer) (window gtk-window))
  (when-let ((win (slot-value window 'gtk-object)))
    (let ((height (or (nth-value 1 (gtk:gtk-window-get-size win)) 1200)))
      (gtk:gtk-window-resize win (max 1 width) height)))
  width)

(defun process-file-chooser-request (web-view file-chooser-request)
  (declare (ignore web-view))
  (with-protect ("Failed to process file chooser request: ~a" :condition)
    (when (native-dialogs *browser*)
      (gobject:g-object-ref (gobject:pointer file-chooser-request))
      (run-thread "file chooser"
                  (let* ((multiple (webkit:webkit-file-chooser-request-select-multiple
                                    file-chooser-request))
                         (files (mapcar
                                 #'uiop:native-namestring
                                 (handler-case
                                     (prompt :prompt (format nil "File~@[s~*~] to input" multiple)
                                             :input (or
                                                     (and
                                                      (webkit:webkit-file-chooser-request-selected-files
                                                       file-chooser-request)
                                                      (first
                                                       (webkit:webkit-file-chooser-request-selected-files
                                                        file-chooser-request)))
                                                     (uiop:native-namestring (uiop:getcwd)))
                                             :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                                             :sources (make-instance 'nyxt/mode/file-manager:file-source
                                                                     :enable-marks-p multiple))
                                   (prompt-buffer-canceled ()
                                     nil)))))
                    (if files
                        (webkit:webkit-file-chooser-request-select-files
                         file-chooser-request
                         (cffi:foreign-alloc :string
                                             :initial-contents (mapcar #'cffi:foreign-string-alloc files)
                                             :count (if multiple
                                                        (length files)
                                                        1)
                                             :null-terminated-p t))
                        (webkit:webkit-file-chooser-request-cancel file-chooser-request))))
      t)))

(defun process-color-chooser-request (web-view color-chooser-request)
  (with-protect ("Failed to process file chooser request: ~a" :condition)
    (when-let ((buffer (and (native-dialogs *browser*)
                            (find web-view (buffer-list) :key #'gtk-object))))
      (gobject:g-object-ref (gobject:pointer color-chooser-request))
      (run-thread
          "color chooser"
        (ps-labels :buffer buffer
          ((get-rgba
            (color)
            (let ((div (ps:chain document (create-element "div"))))
              (setf (ps:chain div style color)
                    (ps:lisp color))
              (ps:chain document body (append-child div))
              (ps:stringify (ps:chain window (get-computed-style div) color))))
           (get-opacity (color)
                        (let ((div (ps:chain document (create-element "div"))))
                          (setf (ps:chain div style color)
                                (ps:lisp color))
                          (ps:chain document body (append-child div))
                          (ps:stringify (ps:chain window (get-computed-style div) opacity)))))
          (let* ((rgba (gdk:make-gdk-rgba))
                 (rgba (progn (webkit:webkit-color-chooser-request-get-rgba
                               color-chooser-request rgba)
                              rgba))
                 (color-name (prompt1 :prompt "Color"
                                      :input (format nil "rgba(~d, ~d, ~d, ~d)"
                                                     (round (* 255 (gdk:gdk-rgba-red rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-green rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-blue rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-alpha rgba))))
                                      :sources 'color-source))
                 (color (get-rgba color-name))
                 (opacity (sera:parse-float (get-opacity color-name)))
                 (rgba (gdk:gdk-rgba-parse color)))
            (unless (uiop:emptyp color)
              (webkit:webkit-color-chooser-request-set-rgba
               color-chooser-request
               (gdk:make-gdk-rgba :red (gdk:gdk-rgba-red rgba)
                                  :green (gdk:gdk-rgba-green rgba)
                                  :blue (gdk:gdk-rgba-blue rgba)
                                  :alpha (coerce opacity 'double-float)))
              (webkit:webkit-color-chooser-request-finish (g:pointer color-chooser-request))))))
      t)))

(defun process-script-dialog (web-view dialog)
  (declare (ignore web-view))
  (with-protect ("Failed to process dialog: ~a" :condition)
    (when (native-dialogs *browser*)
      (let ((dialog (gobject:pointer dialog)))
        (webkit:webkit-script-dialog-ref dialog)
        (run-thread "script dialog"
          (case (webkit:webkit-script-dialog-get-dialog-type dialog)
            (:webkit-script-dialog-alert (echo (webkit:webkit-script-dialog-get-message dialog)))
            (:webkit-script-dialog-prompt
             (let ((text (first (handler-case
                                    (prompt
                                     :prompt (webkit:webkit-script-dialog-get-message dialog)
                                     :input (webkit:webkit-script-dialog-prompt-get-default-text dialog)
                                     :sources 'prompter:raw-source)
                                  (prompt-buffer-canceled () nil)))))
               (if text
                   (webkit:webkit-script-dialog-prompt-set-text dialog text)
                   (progn
                     (webkit:webkit-script-dialog-prompt-set-text dialog (cffi:null-pointer))
                     (webkit:webkit-script-dialog-close dialog)))))
            (:webkit-script-dialog-confirm
             (webkit:webkit-script-dialog-confirm-set-confirmed
              dialog (if-confirm ((webkit:webkit-script-dialog-get-message dialog)))))
            (:webkit-script-dialog-before-unload-confirm
             (webkit:webkit-script-dialog-confirm-set-confirmed
              dialog (if-confirm ((webkit:webkit-script-dialog-get-message dialog)
                                  :yes "leave" :no "stay")))))
          (webkit:webkit-script-dialog-close dialog)
          (webkit:webkit-script-dialog-unref dialog))
        t))))

(defun process-permission-request (web-view request)
  (g:g-object-ref (g:pointer request))
  (run-thread "permission requester"
    (if-confirm ((format
                  nil "[~a] ~a"
                  (webkit:webkit-web-view-uri web-view)
                  (etypecase request
                    (webkit:webkit-geolocation-permission-request
                     "Grant this website geolocation access?")
                    (webkit:webkit-notification-permission-request
                     "Grant this website notifications access?")
                    (webkit:webkit-pointer-lock-permission-request
                     "Grant this website pointer access?")
                    (webkit:webkit-device-info-permission-request
                     "Grant this website device info access?")
                    (webkit:webkit-install-missing-media-plugins-permission-request
                     (format nil "Grant this website a media install permission for ~s?"
                             (webkit:webkit-install-missing-media-plugins-permission-request-get-description
                              request)))
                    (webkit:webkit-media-key-system-permission-request
                     (format nil "Grant this website an EME ~a key access?"
                             (webkit:webkit-media-key-system-permission-get-name request)))
                    (webkit:webkit-user-media-permission-request
                     (format nil "Grant this website a~@[~*n audio~]~@[~* video~] access?"
                             (webkit:webkit-user-media-permission-is-for-audio-device request)
                             (webkit:webkit-user-media-permission-is-for-video-device request)))
                    (webkit:webkit-website-data-access-permission-request
                     (format nil "Grant ~a an access to ~a data?"
                             (webkit:webkit-website-data-access-permission-request-get-requesting-domain
                              request)
                             (webkit:webkit-website-data-access-permission-request-get-current-domain
                              request)))))
                 :yes "grant" :no "deny")
        (webkit:webkit-permission-request-allow request)
        (webkit:webkit-permission-request-deny request))))

(defun process-notification (web-view notification)
  (when (native-dialogs *browser*)
    (let* ((title (webkit:webkit-notification-get-title notification))
           (body (webkit:webkit-notification-get-body notification)))
      (echo "[~a] ~a: ~a" (webkit:webkit-web-view-uri web-view) title body)
      t)))

(define-ffi-method ffi-buffer-initialize-foreign-object ((buffer prompt-buffer))
  (setf (gtk-object buffer)
        (when-let ((window (window buffer)))
          (slot-value window 'prompt-buffer-view)))
  buffer)

(define-ffi-method ffi-buffer-initialize-foreign-object ((buffer gtk-buffer))
  "Initialize BUFFER's GTK web view."
  (setf (gtk-object buffer)
        (configure-web-view
         (make-instance 'webkit:webkit-web-view
                        :web-context (get-web-context *browser* "default"))
         (canvas-color buffer)))
  (when (document-buffer-p buffer)
    (setf (ffi-buffer-smooth-scrolling-enabled-p buffer) (smooth-scrolling buffer)))
  (connect-signal buffer "button-press-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-button-press-event buffer event))
  (connect-signal buffer "key_press_event" nil (widget event)
    (declare (ignore widget))
    (on-signal-key-press-event buffer event))
  (connect-signal buffer "focus-in-event" nil (widget event)
    (declare (ignore widget event))
    (on-signal-focus buffer)
    nil)
  (connect-signal buffer "scroll-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-scroll-event buffer event))
  (connect-signal buffer "web-process-terminated" nil (web-view reason)
    (echo-warning
     "Web process terminated for buffer ~a (opening ~a) because ~[it crashed~;of memory exhaustion~;we had to close it~]"
     (id buffer)
     (url buffer)
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (log:debug
     "Web process terminated for web view ~a because of ~[WEBKIT_WEB_PROCESS_CRASHED~;WEBKIT_WEB_PROCESS_EXCEEDED_MEMORY_LIMIT~;WEBKIT_WEB_PROCESS_TERMINATED_BY_API~]"
     web-view
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (ffi-buffer-delete buffer))
  (connect-signal buffer "close" nil (web-view)
    (declare (ignore web-view))
    (log:debug "Closed ~a" buffer))
  (connect-signal buffer "context-menu" nil (web-view context-menu event hit-test-result)
    (declare (ignore web-view event hit-test-result))
    (if (auxiliary-buffer-p buffer)
        (restrict-auxiliary-context-menu context-menu)
        (progn
          (loop with length = (webkit:webkit-context-menu-get-n-items context-menu)
                for i below length
                for item = (webkit:webkit-context-menu-get-item-at-position context-menu i)
                do (match (webkit:webkit-context-menu-item-get-stock-action item)
                     (:webkit-context-menu-action-open-link-in-new-window
                      (webkit:webkit-context-menu-remove context-menu item)
                      (webkit:webkit-context-menu-insert
                       context-menu
                       (webkit:webkit-context-menu-item-new-from-stock-action-with-label
                        :webkit-context-menu-action-open-link-in-new-window
                        "Open Link in New Buffer")
                       i))))
          (webkit:webkit-context-menu-append
           context-menu (webkit:webkit-context-menu-item-new-separator))
          (let ((accessible-commands
                  (mapcar #'name
                          (nyxt::list-commands
                           :global-p t
                           :mode-symbols (mapcar #'sera:class-name-of
                                                 (sera:filter #'enabled-p (modes buffer)))))))
            (maphash (lambda (label function)
                       (flet ((make-item (label target)
                                (sera:lret ((item (webkit:webkit-context-menu-item-new-from-stock-action-with-label
                                                   :webkit-context-menu-action-action-custom label)))
                                  (gobject:g-signal-connect
                                   (webkit:webkit-context-menu-item-get-g-action item) "activate"
                                   (lambda (action parameter)
                                     (declare (ignore action parameter))
                                     (if (command-p target)
                                         (nyxt:dispatch-command target buffer)
                                         (run-thread "context-menu-command"
                                           (funcall target))))))))
                         (cond
                           ((or (and (command-p function)
                                     (member (name function) accessible-commands))
                                (and (functionp function)
                                     (not (command-p function))))
                            (webkit:webkit-context-menu-append context-menu (make-item label function)))
                           ((listp function)
                            (let ((submenu (webkit:webkit-context-menu-new)))
                              (loop for (command command-label) in function
                                    do (webkit:webkit-context-menu-append
                                        submenu (make-item command-label command)))
                              (webkit:webkit-context-menu-append
                               context-menu
                               (webkit:webkit-context-menu-item-new-with-submenu label submenu)))))))
                     nyxt::*context-menu-commands*))))
    nil)
  (if (auxiliary-buffer-p buffer)
      (connect-signal-function buffer "decide-policy" #'auxiliary-decide-policy)
      (when (web-buffer-p buffer)
        (connect-signal-function buffer "decide-policy" (make-decide-policy-handler buffer))
        (connect-signal buffer "resource-load-started" nil (web-view resource request)
          (declare (ignore web-view))
          (when (nyxt/mode/blocker:request-blocked-p (blocked-host-set buffer)
                                                     (webkit:webkit-uri-request-get-uri request))
            (webkit:webkit-uri-request-set-uri request "data:,"))
          (let* ((response (webkit:webkit-web-resource-response resource))
                 (request-data (make-instance
                                'request-data
                                :buffer buffer
                                :url (quri:uri (webkit:webkit-uri-request-get-uri request))
                                :event-type :other
                                :new-window-p nil
                                :resource-p t
                                :http-method (webkit:webkit-uri-request-get-http-method request)
                                :response-headers (when response
                                                    (let ((headers (webkit:webkit-uri-response-get-http-headers response)))
                                                      (unless (cffi:null-pointer-p headers)
                                                        (webkit:soup-message-headers-get-headers headers))))
                                :request-headers (let ((headers (webkit:webkit-uri-request-get-http-headers request)))
                                                   (unless (cffi:null-pointer-p headers)
                                                     (webkit:soup-message-headers-get-headers headers)))
                                :toplevel-p nil
                                :mime-type (when response
                                             (webkit:webkit-uri-response-mime-type response))
                                :known-type-p t)))
            (setf (gtk-response request-data) response
                  (gtk-request request-data) request
                  (gtk-resource request-data) resource)
            (when (request-resource-hook buffer)
              (hooks:run-hook (request-resource-hook buffer) request-data))))
        (connect-signal buffer "load-changed" nil (web-view load-event)
          (declare (ignore web-view))
          (on-signal-load-changed buffer load-event))
        (connect-signal buffer "mouse-target-changed" nil (web-view hit-test-result modifiers)
          (declare (ignore web-view))
          (on-signal-mouse-target-changed buffer hit-test-result modifiers))
        (connect-signal-function buffer "script-dialog" #'process-script-dialog)
        (connect-signal-function buffer "run-file-chooser" #'process-file-chooser-request)
        (connect-signal-function buffer "run-color-chooser" #'process-color-chooser-request)
        (when (handle-permission-requests-p buffer)
          (connect-signal-function buffer "permission-request" #'process-permission-request))
        (connect-signal-function buffer "show-notification" #'process-notification)
        (connect-signal buffer "load-failed-with-tls-errors" nil (web-view failing-url certificate errors)
          (declare (ignore web-view certificate errors))
          (setf (provisional-load-p buffer) nil)
          (on-signal-load-failed buffer (quri:uri failing-url))
          (on-signal-load-failed-with-tls-errors buffer (quri:uri failing-url)))
        (connect-signal buffer "notify::uri" nil (web-view param-spec)
          (declare (ignore web-view param-spec))
          (run-buffer-thread "notify-uri" buffer
            (on-signal-notify-uri buffer nil)))
        (connect-signal buffer "notify::title" nil (web-view param-spec)
          (declare (ignore web-view param-spec))
          (run-buffer-thread "notify-title" buffer
            (on-signal-notify-title buffer nil)))
        (connect-signal buffer "load-failed" nil (web-view load-event failing-url error)
          (declare (ignore load-event web-view))
          (setf (provisional-load-p buffer) nil)
          (on-signal-load-failed buffer (quri:uri failing-url))
          (cond ((= 302 (webkit::g-error-code error))
                 (on-signal-load-canceled buffer (quri:uri failing-url)))
                ((or (member (slot-value buffer 'nyxt::status) '(:finished :failed))
                     (= 204 (webkit::g-error-code error)))
                 nil)
                (t
                 (echo "Failed to load URL ~a in buffer ~a." failing-url (id buffer))
                 (setf (nyxt::status buffer) :failed)
                 (ffi-buffer-load-alternate-html
                  buffer
                  (network-error-html buffer failing-url)
                  failing-url
                  failing-url)))
          t)
        (connect-signal buffer "create" nil (web-view navigation-action)
          (declare (ignore web-view))
          (let ((url (webkit:webkit-uri-request-uri
                      (webkit:webkit-navigation-action-get-request
                       (gobject:pointer navigation-action)))))
            (run-thread "window-open"
              (make-buffer-focus :url (quri:uri (or url ""))))
            (cffi:null-pointer)))
        (connect-signal buffer "enter-fullscreen" nil (web-view)
          (declare (ignore web-view))
          (when-let ((window (window buffer)))
            (ffi-window-fullscreen window :user-event-p nil)
            (disable-message-buffer window)
            (disable-status-buffer window))
          nil)
        (connect-signal buffer "leave-fullscreen" nil (web-view)
          (declare (ignore web-view))
          (when-let ((window (window buffer)))
            (ffi-window-unfullscreen window :user-event-p nil)
            (enable-message-buffer window)
            (enable-status-buffer window))
          nil)))
  buffer)

(define-ffi-method ffi-buffer-delete ((buffer prompt-buffer))
  (setf (gtk-object buffer) nil)
  (when (and (window buffer)
             (not (nyxt::active-prompt-buffers (window buffer))))
    (setf (ffi-height buffer) 0)))

(define-ffi-method ffi-buffer-delete ((buffer gtk-buffer))
  (with-slots (gtk-object handler-ids provisional-load-p) buffer
    (setf provisional-load-p nil)
    (when gtk-object
      (dolist (id handler-ids)
        (ignore-errors (gobject:g-signal-handler-disconnect gtk-object id)))
      (setf handler-ids nil)
      (ignore-errors (webkit:webkit-web-view-try-close gtk-object))
      (detach-widget gtk-object)
      (gtk:gtk-widget-destroy gtk-object)
      (setf gtk-object nil))))

(defun same-document-fragment-load-p (buffer url)
  (when (and (quri:uri-fragment url)
             (gtk-object buffer))
    (when-let ((current-string (webkit:webkit-web-view-uri (gtk-object buffer))))
      (when (plusp (length current-string))
        (let ((current (ignore-errors (quri:uri current-string))))
          (and current
               (equal (quri:uri-scheme current) (quri:uri-scheme url))
               (equalp (quri:uri-authority current) (quri:uri-authority url))
               (equal (or (quri:uri-path current) "") (or (quri:uri-path url) ""))
               (equal (quri:uri-query current) (quri:uri-query url))))))))

(define-ffi-method ffi-buffer-load ((buffer gtk-buffer) url)
  "Load URL in BUFFER."
  (declare (type quri:uri url))
  (when-let ((view (gtk-object buffer)))
    (unless (same-document-fragment-load-p buffer url)
      (setf (provisional-load-p buffer) t))
    (when (web-buffer-p buffer)
      (setf (nyxt::status buffer) :loading))
    (webkit:webkit-web-view-load-uri view (quri:render-uri url))))

(define-ffi-method ffi-buffer-reload ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (setf (provisional-load-p buffer) t)
    (when (web-buffer-p buffer)
      (setf (nyxt::status buffer) :loading))
    (webkit:webkit-web-view-reload view))
  buffer)

(define-ffi-method ffi-buffer-load-alternate-html ((buffer gtk-buffer)
                                                   html-content
                                                   content-url
                                                   url)
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-load-alternate-html view
                                                html-content
                                                (quri:render-uri (url content-url))
                                                (if (uiop:emptyp url) "about:blank" url))))

(define-ffi-method ffi-buffer-set-background-color ((buffer prompt-buffer) color)
  (let ((rgba (css-color->gdk-rgba color))
        (view (or (gtk-object buffer)
                  (and (window buffer)
                       (slot-value (window buffer) 'prompt-buffer-view)))))
    (when (and rgba view)
      (webkit:webkit-web-view-set-background-color view rgba))))

(define-ffi-method ffi-buffer-set-background-color ((buffer gtk-buffer) color)
  (let ((rgba (css-color->gdk-rgba color))
        (view (gtk-object buffer)))
    (when (and rgba view)
      (webkit:webkit-web-view-set-background-color view rgba))))

(defun wait-for-committed-load (buffer &optional (timeout 5.0))
  (unless (renderer-thread-p nyxt::*renderer*)
    (let ((deadline (+ (get-internal-real-time)
                       (round (* timeout internal-time-units-per-second)))))
      (loop while (and (provisional-load-p buffer)
                       (gtk-object buffer)
                       (not (nyxt::dead-buffer-p buffer))
                       (< (get-internal-real-time) deadline))
            do (sleep 0.005)))))

(defun decode-jsc-result (result jsc-result)
  (cond
    ((or (null jsc-result)
         (webkit:jsc-value-is-undefined jsc-result)
         (webkit:jsc-value-is-null jsc-result))
     nil)
    ((or (webkit:jsc-value-is-object jsc-result)
         (webkit:jsc-value-is-array jsc-result))
     (if-let ((json (and (not (webkit:jsc-value-is-function jsc-result))
                         (ignore-errors (webkit:jsc-value-to-json jsc-result 0)))))
       (cl-json:decode-json-from-string json)
       result))
    (t result)))

(defmethod ffi-buffer-evaluate-javascript ((buffer gtk-buffer) javascript &optional world-name)
  (wait-for-committed-load buffer)
  (if (renderer-thread-p nyxt::*renderer*)
      (when-let ((view (gtk-object buffer)))
        (let ((done-p nil)
              (value nil))
          (apply #'webkit2:webkit-web-view-evaluate-javascript
                 view
                 javascript
                 (lambda (result jsc-result)
                   (setf value (decode-jsc-result result jsc-result)
                         done-p t))
                 (lambda (condition)
                   (nyxt::javascript-error-handler condition)
                   (setf done-p t))
                 (when world-name
                   (list world-name)))
          (loop until (or done-p (null (gtk-object buffer)))
                do (glib:g-main-context-iteration (cffi:null-pointer) t))
          value))
      (let ((channel (nyxt::make-channel 1)))
        (within-gtk-thread
          (if-let ((view (gtk-object buffer)))
            (apply #'webkit2:webkit-web-view-evaluate-javascript
                   view
                   javascript
                   (lambda (result jsc-result)
                     (lparallel.queue:push-queue (decode-jsc-result result jsc-result) channel))
                   (lambda (condition)
                     (nyxt::javascript-error-handler condition)
                     (lparallel.queue:push-queue nil channel))
                   (when world-name
                     (list world-name)))
            (lparallel.queue:push-queue nil channel)))
        (lparallel.queue:pop-queue channel))))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer gtk-buffer) javascript &optional world-name)
  (%within-renderer-thread-async
   (lambda ()
     (when-let ((view (gtk-object buffer)))
       (apply #'webkit2:webkit-web-view-evaluate-javascript
              view
              javascript
              nil
              #'nyxt::javascript-error-handler
              (when world-name
                (list world-name)))))))

(defun list-of-string-to-foreign (list)
  (if list
      (cffi:foreign-alloc :string
                          :count (length list)
                          :initial-contents list
                          :null-terminated-p t)
      (cffi:null-pointer)))

(define-class gtk-user-style ()
  ((gtk-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Related to WebKit's user style sheets."))

(define-ffi-method ffi-buffer-add-user-style ((buffer gtk-buffer) (style gtk-user-style))
  (when-let ((view (gtk-object buffer))
             (code (nyxt/mode/user-script:code style)))
    (let* ((content-manager (webkit:webkit-web-view-get-user-content-manager view))
           (frames (if (nyxt/mode/user-script:all-frames-p style)
                       :webkit-user-content-inject-all-frames
                       :webkit-user-content-inject-top-frame))
           (style-level (if (eq (nyxt/mode/user-script:level style) :author)
                            :webkit-user-style-level-author
                            :webkit-user-style-level-user))
           (style-sheet
             (if (nyxt/mode/user-script:world-name style)
                 (webkit:webkit-user-style-sheet-new-for-world
                  code
                  frames style-level
                  (nyxt/mode/user-script:world-name style)
                  (list-of-string-to-foreign (nyxt/mode/user-script:include style))
                  (list-of-string-to-foreign (nyxt/mode/user-script:exclude style)))
                 (webkit:webkit-user-style-sheet-new
                  code
                  frames style-level
                  (list-of-string-to-foreign (nyxt/mode/user-script:include style))
                  (list-of-string-to-foreign (nyxt/mode/user-script:exclude style))))))
      (setf (gtk-object style) style-sheet)
      (webkit:webkit-user-content-manager-add-style-sheet
       content-manager style-sheet)))
  style)

(define-ffi-method ffi-buffer-remove-user-style ((buffer gtk-buffer) (style gtk-user-style))
  (when-let ((view (gtk-object buffer)))
    (when (and style (gtk-object style))
      (webkit:webkit-user-content-manager-remove-style-sheet
       (webkit:webkit-web-view-get-user-content-manager view)
       (gtk-object style))
      (setf (gtk-object style) nil))))

(define-class gtk-user-script ()
  ((gtk-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Related to WebKitUserScript."))

(define-ffi-method ffi-buffer-add-user-script ((buffer gtk-buffer) (script gtk-user-script))
  (if-let ((code (nyxt/mode/user-script:code script)))
    (when-let ((view (gtk-object buffer)))
      (let* ((content-manager (webkit:webkit-web-view-get-user-content-manager view))
             (frames (if (nyxt/mode/user-script:all-frames-p script)
                         :webkit-user-content-inject-all-frames
                         :webkit-user-content-inject-top-frame))
             (inject-time (if (eq :document-start (nyxt/mode/user-script:run-at script))
                              :webkit-user-script-inject-at-document-start
                              :webkit-user-script-inject-at-document-end))
             (allow-list (list-of-string-to-foreign
                          (or (nyxt/mode/user-script:include script)
                              '("http://*/*" "https://*/*"))))
             (block-list (list-of-string-to-foreign
                          (nyxt/mode/user-script:exclude script)))
             (user-script (if (nyxt/mode/user-script:world-name script)
                              (webkit:webkit-user-script-new-for-world
                               code frames inject-time
                               (nyxt/mode/user-script:world-name script) allow-list block-list)
                              (webkit:webkit-user-script-new
                               code frames inject-time allow-list block-list))))
        (setf (gtk-object script) user-script)
        (webkit:webkit-user-content-manager-add-script
         content-manager user-script))
      script)
    (echo-warning "User script ~a is empty." script)))

(define-ffi-method ffi-buffer-remove-user-script ((buffer gtk-buffer) (script gtk-user-script))
  (when-let ((view (gtk-object buffer)))
    (let ((content-manager (webkit:webkit-web-view-get-user-content-manager view)))
      (when (and script (gtk-object script))
        (webkit:webkit-user-content-manager-remove-script
         content-manager (gtk-object script))
        (setf (gtk-object script) nil)))))

(defmacro define-ffi-settings-accessor (setting-name webkit-setting)
  (let ((full-name (intern (format nil "FFI-BUFFER-~a" setting-name))))
    (symbol-function full-name)
    `(progn
       (define-ffi-method ,full-name ((buffer gtk-buffer))
         (when-let ((view (gtk-object buffer)))
           (,webkit-setting (webkit:webkit-web-view-get-settings view))))
       (define-ffi-method (setf ,full-name) (value (buffer gtk-buffer))
         (when-let ((view (gtk-object buffer)))
           (setf (,webkit-setting (webkit:webkit-web-view-get-settings view))
                 value))
         value))))

(define-ffi-settings-accessor smooth-scrolling-enabled-p webkit:webkit-settings-enable-smooth-scrolling)
(define-ffi-settings-accessor media-enabled-p webkit:webkit-settings-enable-media)
(define-ffi-settings-accessor auto-load-image-enabled-p webkit:webkit-settings-auto-load-images)

(defmethod ffi-buffer-blocked-hosts ((buffer gtk-buffer))
  (blocked-hosts buffer))

(defvar *blocked-host-filter-store* nil)

(defun blocked-host-filter-store ()
  (or *blocked-host-filter-store*
      (setf *blocked-host-filter-store*
            (webkit:webkit-user-content-filter-store-new
             (uiop:native-namestring
              (ensure-directories-exist
               (uiop:merge-pathnames* #p"nyxt/blocked-hosts/"
                                      (uiop:xdg-cache-home))))))))

(defun json-string (string)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (otherwise (write-char char out))))
    (write-char #\" out)))

(defun host-url-filter (host)
  "Return a WebKit url-filter that matches HOST and nothing under another name.
WebKit's filter language accepts a restricted regular expression."
  (if (find #\: host)
      (format nil "^https?://\\[~a\\]" (ppcre:quote-meta-chars host))
      (format nil "^https?://~a[:/]" (ppcre:quote-meta-chars host))))

(defun blocked-host-rules (hosts)
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for host in hosts
          for first = t then nil
          do (unless first
               (write-char #\, out))
             (format out "{\"trigger\":{\"url-filter\":~a,\"url-filter-is-case-sensitive\":false},\"action\":{\"type\":\"block\"}}"
                     (json-string (host-url-filter host))))
    (write-char #\] out)))

(defun blocked-host-filter-identifier (buffer)
  (format nil "nyxt-blocked-hosts-~a" (id buffer)))

(defstruct blocked-host-filter-save
  channel
  buffer
  identifier
  store)

(defvar *blocked-host-filter-saves* (make-hash-table))
(defvar *blocked-host-filter-save-id* 0)

(cffi:defcallback blocked-host-filter-saved :void
    ((source :pointer) (result :pointer) (user-data :pointer))
  (declare (ignore source))
  (let* ((id (cffi:pointer-address user-data))
         (save (gethash id *blocked-host-filter-saves*)))
    (remhash id *blocked-host-filter-saves*)
    (when save
      (lparallel.queue:push-queue
       (handler-case
           (let ((filter (webkit:webkit-user-content-filter-store-save-finish
                          (blocked-host-filter-save-store save)
                          result))
                 (view (gtk-object (blocked-host-filter-save-buffer save))))
             (when view
               (let ((manager (webkit:webkit-web-view-get-user-content-manager view)))
                 (webkit:webkit-user-content-manager-remove-filter-by-id
                  manager (blocked-host-filter-save-identifier save))
                 (webkit:webkit-user-content-manager-add-filter manager filter)))
             (webkit:webkit-user-content-filter-unref filter)
             t)
         (error (condition) condition))
       (blocked-host-filter-save-channel save)))))

(defun clear-blocked-host-filter (buffer)
  (when (gtk-object buffer)
    (within-gtk-thread
      (when-let ((view (gtk-object buffer)))
        (webkit:webkit-user-content-manager-remove-filter-by-id
         (webkit:webkit-web-view-get-user-content-manager view)
         (blocked-host-filter-identifier buffer))))))

(defun install-blocked-host-filter (buffer hosts)
  (when (gtk-object buffer)
    (let* ((identifier (blocked-host-filter-identifier buffer))
           (octets (babel:string-to-octets (blocked-host-rules hosts) :encoding :utf-8))
           (channel (nyxt::make-channel 1))
           (save-id (incf *blocked-host-filter-save-id*))
           (save (make-blocked-host-filter-save :channel channel
                                                :buffer buffer
                                                :identifier identifier)))
      (setf (gethash save-id *blocked-host-filter-saves*) save)
      (within-gtk-thread
        (if (gtk-object buffer)
            (let ((bytes (cffi:with-pointer-to-vector-data (data octets)
                           (webkit:g-bytes-new data (length octets)))))
              (setf (blocked-host-filter-save-store save) (blocked-host-filter-store))
              (webkit:webkit-user-content-filter-store-save
               (blocked-host-filter-save-store save)
               identifier
               bytes
               (cffi:null-pointer)
               (cffi:callback blocked-host-filter-saved)
               (cffi:make-pointer save-id))
              (webkit:g-bytes-unref bytes))
            (progn
              (remhash save-id *blocked-host-filter-saves*)
              (lparallel.queue:push-queue t channel))))
      (let ((result (lparallel.queue:pop-queue channel)))
        (when (typep result 'error)
          (error result))))))

(defmethod (setf ffi-buffer-blocked-hosts) (hosts (buffer gtk-buffer))
  (setf (blocked-hosts buffer) hosts
        (blocked-host-set buffer) (when hosts
                                    (nyxt/mode/blocker:blocked-host-table hosts)))
  (if hosts
      (install-blocked-host-filter buffer hosts)
      (clear-blocked-host-filter buffer))
  hosts)

(define-ffi-method ffi-buffer-sound-enabled-p ((buffer gtk-buffer))
  (if-let ((view (gtk-object buffer)))
    (not (webkit:webkit-web-view-get-is-muted view))
    t))
(define-ffi-method (setf ffi-buffer-sound-enabled-p) (value (buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-set-is-muted view (not value)))
  value)

;; KLUDGE: PDF.js in WebKit (actual for version 2.41.4) always saves
;; PDFs as "document.pdf". This is because WebKit does not pass "file"
;; parameter to the viewer. See
;; https://stackoverflow.com/questions/47098206/pdf-js-downloading-as-document-pdf-instead-of-filename .
;; Here we restore the original file name from an URL if a suggested
;; file name looks suspicious.
(sera:-> maybe-fix-pdfjs-filename (string quri:uri)
         (values string &optional))
(defun maybe-fix-pdfjs-filename (suggested-file-name uri)
  (let ((pathname (pathname (quri:uri-path uri))))
    (if (and (string= suggested-file-name "document.pdf")
             (string= (pathname-type pathname) "pdf"))
        (uiop:native-namestring
         (make-pathname :name (pathname-name pathname)
                        :type "pdf"))
        suggested-file-name)))

(defun wrap-download (webkit-download)
  (let ((download-buffer (find (webkit:webkit-download-get-web-view webkit-download)
                               (buffer-list) :key #'gtk-object)))
    (sera:lret ((original-url (if download-buffer (url download-buffer) (quri:uri "")))
                (download (make-instance 'nyxt/mode/download:download
                                         :url (webkit:webkit-uri-request-uri
                                               (webkit:webkit-download-get-request webkit-download))
                                         :gtk-object webkit-download)))
      (setf (nyxt/mode/download::cancel-function download)
            (lambda ()
              (setf (nyxt/mode/download:status download) :canceled)
              (webkit:webkit-download-cancel webkit-download)))
      (push download (downloads *browser*))
      (connect-signal download "received-data" nil (webkit-download data-length)
        (declare (ignore data-length))
        (setf (nyxt/mode/download:bytes-downloaded download)
              (webkit:webkit-download-get-received-data-length webkit-download))
        (setf (nyxt/mode/download:completion-percentage download)
              (* 100 (webkit:webkit-download-estimated-progress webkit-download))))
      (connect-signal download "decide-destination" nil (webkit-download suggested-file-name)
        (when-let* ((suggested-file-name (maybe-fix-pdfjs-filename suggested-file-name original-url))
                    (download-dir (or (ignore-errors (download-directory download-buffer))
                                      (make-instance 'download-directory)))
                    (download-directory (files:expand download-dir))
                    (native-download-directory (unless (files:nil-pathname-p download-directory)
                                                 (uiop:native-namestring download-directory)))
                    (path (str:concat native-download-directory suggested-file-name))
                    (unique-path (download-manager::ensure-unique-file path))
                    (file-path (format nil "file://~a" unique-path)))
          (if (string= path unique-path)
              (log:debug "Downloading file to ~s." unique-path)
              (echo "Destination ~s exists, saving as ~s." path unique-path))
          (webkit:webkit-download-set-destination webkit-download file-path)))
      (connect-signal download "created-destination" nil (webkit-download destination)
        (declare (ignore destination))
        (setf (nyxt/mode/download:destination-path download)
              (uiop:ensure-pathname
               (quri:uri-path (quri:uri
                               (webkit:webkit-download-destination webkit-download))))))
      (connect-signal download "failed" nil (webkit-download error)
        (declare (ignore error))
        (unless (eq (nyxt/mode/download:status download) :canceled)
          (setf (nyxt/mode/download:status download) :failed))
        (echo "Download failed for ~s."
              (webkit:webkit-uri-request-uri
               (webkit:webkit-download-get-request webkit-download))))
      (connect-signal download "finished" nil (webkit-download)
        (declare (ignore webkit-download))
        (unless (member (nyxt/mode/download:status download) '(:canceled :failed))
          (setf (nyxt/mode/download:status download) :finished)
          ;; If download was too small, it may not have been updated.
          (setf (nyxt/mode/download:completion-percentage download) 100))))))

(define-ffi-method ffi-buffer-download ((buffer gtk-buffer) url)
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-download-uri view url)))

(define-ffi-method ffi-buffer-user-agent ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (when-let ((settings (webkit:webkit-web-view-get-settings view)))
      (webkit:webkit-settings-user-agent settings))))

(define-ffi-method (setf ffi-buffer-user-agent) (value (buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (when-let ((settings (webkit:webkit-web-view-get-settings view)))
      (setf (webkit:webkit-settings-user-agent settings) value))))

(define-ffi-method ffi-buffer-zoom-ratio ((buffer gtk-buffer))
  (if-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-zoom-level view)
    (or (zoom-ratio buffer) 1.0)))
(define-ffi-method (setf ffi-buffer-zoom-ratio) (value (buffer gtk-buffer))
  (cond
    ((and (floatp value) (plusp value))
     (when-let ((view (gtk-object buffer)))
       (setf (webkit:webkit-web-view-zoom-level view) value)))
    ((and (floatp value) (zerop value))
     (when-let ((view (gtk-object buffer)))
       (setf (webkit:webkit-web-view-zoom-level view) 1.0)))
    (t
     (echo-warning "Zoom ratio must be a non-negative floating point number."))))

(define-ffi-method ffi-inspector-show ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-inspector-show
     (webkit:webkit-web-view-get-inspector view))))

(defstruct cookie-policy-query
  channel
  cookie-manager)

(defvar *cookie-policy-queries* (make-hash-table))
(defvar *cookie-policy-query-id* 0)
(defvar *cookie-policy-lock* (bt:make-lock "nyxt-gtk-cookie-policy"))

(defun webkit-policy->cookie-policy (policy)
  (match policy
    (:webkit-cookie-policy-accept-always :accept)
    (:webkit-cookie-policy-accept-never :never)
    (:webkit-cookie-policy-accept-no-third-party :no-third-party)
    (_ :accept)))

(cffi:defcallback cookie-policy-fetched :void
    ((source :pointer) (result :pointer) (user-data :pointer))
  (declare (ignore source))
  (let* ((id (cffi:pointer-address user-data))
         (query (bt:with-lock-held (*cookie-policy-lock*)
                  (prog1 (gethash id *cookie-policy-queries*)
                    (remhash id *cookie-policy-queries*)))))
    (when query
      (lparallel.queue:push-queue
       (handler-case
           (webkit-policy->cookie-policy
            (webkit:webkit-cookie-manager-get-accept-policy-finish
             (cookie-policy-query-cookie-manager query)
             result))
         (error () nil))
       (cookie-policy-query-channel query)))))

(defmethod ffi-buffer-cookie-policy ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (if (renderer-thread-p nyxt::*renderer*)
        (let ((done-p nil)
              (value nil)
              (channel (nyxt::make-channel 1))
              (query-id (bt:with-lock-held (*cookie-policy-lock*)
                          (incf *cookie-policy-query-id*))))
          (let* ((context (webkit:webkit-web-view-web-context view))
                 (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
            (bt:with-lock-held (*cookie-policy-lock*)
              (setf (gethash query-id *cookie-policy-queries*)
                    (make-cookie-policy-query :channel channel
                                              :cookie-manager cookie-manager)))
            (webkit:webkit-cookie-manager-get-accept-policy
             cookie-manager
             (cffi:null-pointer)
             (cffi:callback cookie-policy-fetched)
             (cffi:make-pointer query-id))
            (loop until done-p
                  do (multiple-value-bind (result received-p)
                         (lparallel.queue:try-pop-queue channel :timeout 0)
                       (if received-p
                           (setf value result
                                 done-p t)
                           (glib:g-main-context-iteration (cffi:null-pointer) t))))
            value))
        (let ((result-channel (nyxt::make-channel 1))
              (query-id (bt:with-lock-held (*cookie-policy-lock*)
                          (incf *cookie-policy-query-id*))))
          (within-gtk-thread
            (if-let ((current-view (gtk-object buffer)))
              (let* ((context (webkit:webkit-web-view-web-context current-view))
                     (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
                (bt:with-lock-held (*cookie-policy-lock*)
                  (setf (gethash query-id *cookie-policy-queries*)
                        (make-cookie-policy-query :channel result-channel
                                                  :cookie-manager cookie-manager)))
                (webkit:webkit-cookie-manager-get-accept-policy
                 cookie-manager
                 (cffi:null-pointer)
                 (cffi:callback cookie-policy-fetched)
                 (cffi:make-pointer query-id)))
              (lparallel.queue:push-queue nil result-channel)))
          (lparallel.queue:pop-queue result-channel)))))
(define-ffi-method (setf ffi-buffer-cookie-policy) (value (buffer gtk-buffer))
  "Set the cookie policy to VALUE.
Valid values are determined by the `cookie-policy' type."
  (when-let ((view (gtk-object buffer)))
    (let* ((context (webkit:webkit-web-view-web-context view))
           (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
      (setf (ffi-buffer-cookie-policy cookie-manager) value)))
  buffer)
(defmethod (setf ffi-buffer-cookie-policy) (value (cookie-manager webkit:webkit-cookie-manager))
  "Set the cookie policy to VALUE.
Valid values are determined by the `cookie-policy' type."
  (webkit:webkit-cookie-manager-set-accept-policy
   cookie-manager
   (match value
     (:accept :webkit-cookie-policy-accept-always)
     (:never :webkit-cookie-policy-accept-never)
     (:no-third-party :webkit-cookie-policy-accept-no-third-party))))

(defmethod ffi-preferred-languages ((buffer gtk-buffer))
  "Not supported by WebKitGTK.
Only the setf method is."
  nil)
(define-ffi-method (setf ffi-preferred-languages) (language-list (buffer gtk-buffer))
  "LANGUAGE-LIST is a list of strings like '(\"en_US\" \"fr_FR\")."
  (when-let ((view (gtk-object buffer)))
    (let ((langs (cffi:foreign-alloc :string
                                     :initial-contents language-list
                                     :null-terminated-p t)))
      (webkit:webkit-web-context-set-preferred-languages
       (webkit:webkit-web-view-web-context view)
       langs)
      (cffi:foreign-free langs))))

(define-ffi-method ffi-focused-p ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (gtk:gtk-widget-is-focus view)))

(define-ffi-method itp-enabled-p ((buffer gtk-buffer))
  "Return non-nil when Intelligent Tracking Prevention is enabled."
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-website-data-manager-get-itp-enabled
     (webkit:webkit-web-context-website-data-manager
      (webkit:webkit-web-view-web-context view)))))
(define-ffi-method (setf itp-enabled-p) (value (buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-website-data-manager-set-itp-enabled
     (webkit:webkit-web-context-website-data-manager
      (webkit:webkit-web-view-web-context view))
     value)))

(defmethod ffi-buffer-copy ((gtk-buffer gtk-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (setf (clipboard-text) text)
      (let ((selection (nyxt::buffer-selection-text gtk-buffer))
            (channel (nyxt::make-channel 1)))
        (within-gtk-thread
          (if-let ((view (gtk-object gtk-buffer)))
            (webkit:webkit-web-view-can-execute-editing-command
             view webkit2:+webkit-editing-command-copy+
             (lambda (can-execute?)
               (when can-execute?
                 (webkit:webkit-web-view-execute-editing-command
                  view webkit2:+webkit-editing-command-copy+))
               (lparallel.queue:push-queue can-execute? channel))
             (lambda (e)
               (echo-warning "~s failed to copy to clipboard." e)
               (lparallel.queue:push-queue nil channel)))
            (lparallel.queue:push-queue nil channel)))
        (lparallel.queue:pop-queue channel)
        ;; Take ownership after WebKit's copy. A headless GTK clipboard write
        ;; does not reach the compositor, and this process has to answer later
        ;; paste requests.
        (if (and (stringp selection) (not (uiop:emptyp selection)))
            (progn
              (setf (clipboard-text) selection)
              (echo "~s copied to clipboard." selection)
              selection)
            (let ((clip (clipboard-text)))
              (when clip
                (echo "~s copied to clipboard." clip))
              clip)))))

(defmethod ffi-buffer-paste ((gtk-buffer gtk-buffer) &optional (text nil text-provided-p))
  (let ((text (if text-provided-p text (clipboard-text))))
    (when text
      (setf (clipboard-text) text))
    (%within-renderer-thread-async
     (lambda ()
       (when-let ((view (gtk-object gtk-buffer)))
         (if text
             (webkit:webkit-web-view-execute-editing-command-with-argument
              view "InsertText" text)
             (webkit:webkit-web-view-can-execute-editing-command
              view webkit2:+webkit-editing-command-paste+
              (lambda (can-execute?)
                (when can-execute?
                  (webkit:webkit-web-view-execute-editing-command
                   view webkit2:+webkit-editing-command-paste+)))
              (lambda (e) (echo-warning "~s failed to paste." e)))))))
    text))

(defmethod ffi-buffer-cut ((gtk-buffer gtk-buffer))
  (let ((selection (nyxt::buffer-selection-text gtk-buffer))
        (channel (nyxt::make-channel 1)))
    (within-gtk-thread
      (if-let ((view (gtk-object gtk-buffer)))
        (webkit:webkit-web-view-can-execute-editing-command
         view webkit2:+webkit-editing-command-cut+
         (lambda (can-execute?)
           (if can-execute?
               (progn
                 (webkit:webkit-web-view-execute-editing-command
                  view webkit2:+webkit-editing-command-cut+)
                 (lparallel.queue:push-queue t channel))
               (lparallel.queue:push-queue nil channel)))
         (lambda (e)
           (echo-warning "Cannot cut: ~a" e)
           (lparallel.queue:push-queue nil channel)))
        (lparallel.queue:push-queue nil channel)))
    (when (lparallel.queue:pop-queue channel)
      (when (and (stringp selection) (not (uiop:emptyp selection)))
        (setf (clipboard-text) selection))
      selection)))

(define-ffi-method ffi-buffer-select-all ((gtk-buffer gtk-buffer))
  (when-let ((view (gtk-object gtk-buffer)))
    (webkit:webkit-web-view-can-execute-editing-command
     view webkit2:+webkit-editing-command-select-all+
     (lambda (can-execute?)
       (when can-execute?
         (webkit:webkit-web-view-execute-editing-command
          view webkit2:+webkit-editing-command-select-all+)))
     (lambda (e) (echo-warning "Cannot select all: ~a" e)))))

(define-ffi-method ffi-buffer-undo ((gtk-buffer gtk-buffer))
  (when-let ((view (gtk-object gtk-buffer)))
    (webkit:webkit-web-view-can-execute-editing-command
     view webkit2:+webkit-editing-command-undo+
     (lambda (can-execute?)
       (when can-execute?
         (webkit:webkit-web-view-execute-editing-command
          view webkit2:+webkit-editing-command-undo+)))
     (lambda (e) (echo-warning "Cannot undo: ~a" e)))))

(define-ffi-method ffi-buffer-redo ((gtk-buffer gtk-buffer))
  (when-let ((view (gtk-object gtk-buffer)))
    (webkit:webkit-web-view-can-execute-editing-command
     view webkit2:+webkit-editing-command-redo+
     (lambda (can-execute?)
       (when can-execute?
         (webkit:webkit-web-view-execute-editing-command
          view webkit2:+webkit-editing-command-redo+)))
     (lambda (e) (echo-warning "Cannot redo: ~a" e)))))

(define-ffi-method ffi-buffer-navigate-backwards ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-go-back view)))

(define-ffi-method ffi-buffer-navigate-forwards ((buffer gtk-buffer))
  (when-let ((view (gtk-object buffer)))
    (webkit:webkit-web-view-go-forward view)))
