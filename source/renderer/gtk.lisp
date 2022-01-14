;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(setf +renderer+ "GTK")

(define-class gtk-browser ()
  (#+darwin
   (modifiers '()
              :documentation "On macOS some modifiers like Super and Meta are
seen like regular keys.
To work around this issue, we store them in this list while they are pressed.
See `push-modifiers', `pop-modifiers' and `key-event-modifiers'.")
   (modifier-translator #'translate-modifiers
                        :documentation "Function that returns a list of
modifiers understood by `keymap:make-key'.  You can customize this slot if you
want to change the behaviour of modifiers, for instance swap 'control' and
'meta':

\(defun my-translate-modifiers (modifier-state &optional event)
  \"Swap control and meta.\"
  (declare (ignore event))
  (let ((plist '(:control-mask \"meta\"
                 :mod1-mask \"control\" ;; Usually it is Alt.
                 :mod5-mask nil         ;; See your config for what mod1-5 mean.
                 :shift-mask \"shift\"
                 :super-mask \"super\"
                 :hyper-mask \"hyper\"
                 :meta-mask nil         ;; Meta.
                 :lock-mask nil)))
    (delete nil (mapcar (lambda (mod) (getf plist mod)) modifier-state))))

\(define-configuration browser
  ((modifier-translator #'my-translate-modifiers)))"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class browser (gtk-browser))

(define-class gtk-window ()
  ((gtk-object)
   (handler-ids
    :documentation "See `gtk-buffer' slot of the same name.")
   (root-box-layout)
   (horizontal-box-layout)
   (panel-buffer-container-left)
   (panel-buffer-container-right)
   (panel-buffers-left)
   (panel-buffers-right)
   (main-buffer-container)
   (prompt-buffer-container)
   (prompt-buffer-view
    :documentation "Shared web view between all prompt buffers of this window.")
   (status-container)
   (message-container)
   (message-view)
   (key-string-buffer))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class window (gtk-window))

(define-class gtk-buffer ()
  ((gtk-object)
   (handler-ids
    :documentation "Store all GObject signal handler IDs so that we can disconnect the signal handler when the object is finalised.
See https://developer.gnome.org/gobject/stable/gobject-Signals.html#signal-memory-management.")
   (gtk-proxy-url (quri:uri ""))
   (proxy-ignored-hosts '())
   (data-manager-path (make-instance 'data-manager-data-path)
                      :documentation "Directory in which the WebKitGTK
data-manager will store the data separately for each buffer.")
   (gtk-extensions-path (make-instance 'gtk-extensions-data-path)
                        :documentation "Directory to store the WebKit-specific extensions in.")
   (loading-webkit-history-p nil
                             :type boolean
                             :export nil
                             :documentation "Internal hack, do not use me!
WebKitGTK may trigger 'load-failed' when loading a page from the WebKit-history
cache.  Upstream bug?  We use this slot to know when to ignore these load
failures."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class buffer (gtk-buffer))

(defclass webkit-web-context (webkit:webkit-web-context) ()
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((web-context webkit-web-context) &key)
  #+webkit2-sandboxing
  (webkit:webkit-web-context-set-sandbox-enabled web-context t)
  web-context)

(defvar gtk-running-p nil
  "Non-nil if the GTK main loop is running.
See `ffi-initialize' and `ffi-kill-browser'.

Restarting GTK within the same Lisp image breaks WebKitGTK.
As a workaround, we never leave the GTK main loop when running from a REPL.

See https://github.com/atlas-engineer/nyxt/issues/740")

(defun renderer-thread-p ()
  #+darwin
  (string= "main thread" (bt:thread-name (bt:current-thread)))
  #-darwin
  (string= "cl-cffi-gtk main thread" (bt:thread-name (bt:current-thread))))

(defmacro within-gtk-thread (&body body)
  "Protected `gtk:within-gtk-thread'."
  `(gtk:within-gtk-thread
     (with-protect ("Error on GTK thread: ~a" :condition)
       ,@body)))

(defmethod ffi-within-renderer-thread ((browser gtk-browser) thunk)
  (declare (ignore browser))
  (within-gtk-thread
    (funcall thunk)))

(defun %within-renderer-thread (thunk)
  "If the current thread is the renderer thread, execute THUNK with `funcall'.
Otherwise run the THUNK on the renderer thread by passing it a channel and wait on the channel's result."
  (if (renderer-thread-p)
      (funcall thunk)
      (let ((channel (make-channel 1)))
        (within-gtk-thread
          (funcall thunk channel))
        (calispel:? channel))))

(defun %within-renderer-thread-async (thunk)
  "Same as `%within-renderer-thread' but THUNK is not blocking and does
not return."
  (if (renderer-thread-p)
      (funcall thunk)
      (within-gtk-thread
        (funcall thunk))))

(defmacro define-ffi-method (name args &body body)
  "Define an FFI method to run in the renderer thread.

Always returns a value retrieved from the renderer thread, using Calispel
channels if the current thread is not the renderer one.

It's a `defmethod' wrapper. If you don't need the body of the method to execute in
the renderer thread, use `defmethod' instead."
  (multiple-value-bind (forms declares docstring)
      (alex:parse-body body :documentation t)
    `(defmethod ,name ,args
       ,@(sera:unsplice docstring)
       ,@declares
       (if (renderer-thread-p)
           (progn
             ,@forms)
           (let ((channel (make-channel 1)))
             (within-gtk-thread
               (calispel:!
                channel
                (progn
                  ,@forms)))
             (calispel:? channel))))))

(defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
  "gtk:within-main-loop handles all the GTK initialization. On
   GNU/Linux, Nyxt could hang after 10 minutes if it's not
   used. Conversely, on Darwin, if gtk:within-main-loop is used, no
   drawing happens. Drawing operations on Darwin MUST originate from
   the main thread, which the GTK main loop is not guaranteed to be
   on."
  (log:debug "Initializing GTK Interface")
  (setf (uiop:getenv "WEBKIT_FORCE_SANDBOX") "0")
  (if gtk-running-p
      (within-gtk-thread
        (finalize browser urls startup-timestamp))
      #-darwin
      (progn
        (setf gtk-running-p t)
        (glib:g-set-prgname "nyxt")
        (gdk:gdk-set-program-class "Nyxt")
        (gtk:within-main-loop
          (with-protect ("Error on GTK thread: ~a" :condition)
            (finalize browser urls startup-timestamp)))
        (unless *run-from-repl-p*
          (gtk:join-gtk-main)))
      #+darwin
      (progn
        (setf gtk-running-p t)
        (glib:g-set-prgname "nyxt")
        (gdk:gdk-set-program-class "Nyxt")
        (finalize browser urls startup-timestamp)
        (gtk:gtk-main))))

(define-ffi-method ffi-kill-browser ((browser gtk-browser))
  ;; TODO: Terminating the GTK thread from the REPL seems to prevent Nyxt from
  ;; starting again.
  (unless *run-from-repl-p*
    (gtk:leave-gtk-main)))

(define-class data-manager-data-path (data-path)
  ((dirname (uiop:xdg-cache-home +data-root+ "data-manager"))
   (ref :initform "data-manager"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod expand-data-path ((profile nosave-data-profile) (path data-manager-data-path))
  "We shouldn't store any `data-manager' data for `nosave-data-profile'."
  nil)

(define-class gtk-extensions-data-path (data-path)
  ((dirname (uiop:xdg-config-home +data-root+ "extensions"))
   (ref :initform "gtk-extensions"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod expand-data-path ((profile nosave-data-profile) (path gtk-extensions-data-path))
  "We shouldn't enable (possibly) user-identifying extensions for `nosave-data-profile'."
  nil)

(define-class gtk-download (download)
  ((gtk-object)
   (handler-ids
    :documentation "See `gtk-buffer' slot of the same name."))
  (:accessor-name-transformer (class*:make-name-transformer name)))
(define-user-class download (gtk-download))

(defmethod expand-data-path ((profile data-profile) (path gtk-extensions-data-path))
  "Return finalized path for gtk-extension directory."
  (expand-default-path path :root (uiop:native-namestring
                                   (if (str:emptyp (namestring (dirname path)))
                                       (uiop:xdg-data-home +data-root+ "gtk-extensions")
                                       (dirname path)))))


(defun make-web-view (&key context-buffer)
  "Return a web view instance.
When passed a web buffer, create a buffer-local web context.
Such contexts are not needed for internal buffers."
  (if context-buffer
      (make-instance 'webkit:webkit-web-view
                     :web-context (make-context context-buffer))
      (make-instance 'webkit:webkit-web-view)))

(defun make-decide-policy-handler (buffer)
  (lambda (web-view response-policy-decision policy-decision-type-response)
    (declare (ignore web-view))
    ;; Even if errors are caught with `with-protect', we must ignore the policy
    ;; decision on error, lest we load a web page in an internal buffer for
    ;; instance.
    (g:g-object-ref (g:pointer response-policy-decision))
    (run-thread "asynchronous decide-policy processing"
      (handler-bind ((error (lambda (c)
                              (echo-warning "decide policy error: ~a" c)
                              ;; TODO: Don't automatically call the restart when from the REPL?
                              ;; (unless *run-from-repl-p*
                              ;;   (invoke-restart 'ignore-policy-decision))
                              (invoke-restart 'ignore-policy-decision))))
        (restart-case (on-signal-decide-policy buffer response-policy-decision policy-decision-type-response)
          (ignore-policy-decision ()
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
                               `(run-thread ,@forms)
                               `(with-protect ("Error in signal on renderer thread: ~a" :condition)
                                  ,@forms))))))
       (push handler-id (handler-ids ,object)))))

(defmethod initialize-instance :after ((buffer status-buffer) &key)
  (%within-renderer-thread-async
   (lambda ()
     (with-slots (gtk-object) buffer
       (setf gtk-object (make-web-view :context-buffer buffer))
       (connect-signal-function
        buffer "decide-policy"
        (make-decide-policy-handler buffer))))))

(defmethod initialize-instance :after ((window gtk-window) &key)
  (%within-renderer-thread-async
   (lambda ()
     (with-slots (gtk-object root-box-layout horizontal-box-layout
                  panel-buffer-container-left
                  panel-buffer-container-right
                  main-buffer-container
                  active-buffer prompt-buffer-container
                  prompt-buffer-view
                  status-buffer status-container
                  message-container message-view
                  id key-string-buffer) window
       (setf id (get-unique-identifier *browser*))
       (setf gtk-object (make-instance 'gtk:gtk-window
                                       :type :toplevel
                                       :default-width 1024
                                       :default-height 768))
       (setf root-box-layout (make-instance 'gtk:gtk-box
                                            :orientation :vertical
                                            :spacing 0))
       (setf horizontal-box-layout (make-instance 'gtk:gtk-box
                                                  :orientation :horizontal
                                                  :spacing 0))
       (setf panel-buffer-container-left (make-instance 'gtk:gtk-box
                                                        :orientation :horizontal
                                                        :spacing 0))
       (setf panel-buffer-container-right (make-instance 'gtk:gtk-box
                                                         :orientation :horizontal
                                                         :spacing 0))
       (setf main-buffer-container (make-instance 'gtk:gtk-box
                                                  :orientation :vertical
                                                  :spacing 0))
       (setf prompt-buffer-container (make-instance 'gtk:gtk-box
                                                    :orientation :vertical
                                                    :spacing 0))
       (setf message-container (make-instance 'gtk:gtk-box
                                              :orientation :vertical
                                              :spacing 0))
       (setf status-container (make-instance 'gtk:gtk-box
                                             :orientation :vertical
                                             :spacing 0))
       (setf key-string-buffer (make-instance 'gtk:gtk-entry))
       (setf active-buffer (make-dummy-buffer))

       ;; Add the views to the box layout and to the window
       (gtk:gtk-box-pack-start main-buffer-container (gtk-object active-buffer) :expand t :fill t)
       (gtk:gtk-box-pack-start horizontal-box-layout panel-buffer-container-left :expand nil)
       (gtk:gtk-box-pack-start horizontal-box-layout main-buffer-container :expand t :fill t)
       (gtk:gtk-box-pack-start horizontal-box-layout panel-buffer-container-right :expand nil)
       (gtk:gtk-box-pack-start root-box-layout horizontal-box-layout :expand t :fill t)

       (setf message-view (make-web-view))
       (gtk:gtk-box-pack-end root-box-layout message-container :expand nil)
       (gtk:gtk-box-pack-start message-container message-view :expand t)
       (setf (gtk:gtk-widget-size-request message-container)
             (list -1 (message-buffer-height window)))

       (setf status-buffer (make-instance 'user-status-buffer))
       (gtk:gtk-box-pack-end root-box-layout status-container :expand nil)
       (gtk:gtk-box-pack-start status-container (gtk-object status-buffer) :expand t)
       (setf (gtk:gtk-widget-size-request status-container)
             (list -1 (height status-buffer)))

       (setf prompt-buffer-view (make-web-view))
       (gtk:gtk-box-pack-end root-box-layout prompt-buffer-container :expand nil)
       (gtk:gtk-box-pack-start prompt-buffer-container prompt-buffer-view :expand t)
       (setf (gtk:gtk-widget-size-request prompt-buffer-container)
             (list -1 0))

       (gtk:gtk-container-add gtk-object root-box-layout)
       (setf (slot-value *browser* 'last-active-window) window)
       (gtk:gtk-widget-show-all gtk-object)
       (connect-signal window "key_press_event" nil (widget event)
         (declare (ignore widget))
         #+darwin
         (push-modifier *browser* event)
         (on-signal-key-press-event window event))
       (connect-signal window "key_release_event" nil (widget event)
         (declare (ignore widget))
         #+darwin
         (pop-modifier *browser* event)
         (on-signal-key-release-event window event))
       (connect-signal window "destroy" nil (widget)
         (declare (ignore widget))
         (on-signal-destroy window))
       (connect-signal window "window-state-event" nil (widget event)
         (declare (ignore widget))
         (setf (fullscreen-p window)
               (find :fullscreen
                     (gdk:gdk-event-window-state-new-window-state event)))
         nil)))))

(define-ffi-method on-signal-destroy ((window gtk-window))
  ;; remove buffer from window to avoid corruption of buffer
  (gtk:gtk-container-remove (root-box-layout window) (gtk-object (active-buffer window)))
  (window-delete window))

(define-ffi-method ffi-window-delete ((window gtk-window))
  (gtk:gtk-widget-destroy (gtk-object window)))

(define-ffi-method ffi-window-fullscreen ((window gtk-window))
  (gtk:gtk-window-fullscreen (gtk-object window)))

(define-ffi-method ffi-window-unfullscreen ((window gtk-window))
  (gtk:gtk-window-unfullscreen (gtk-object window)))

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

#+darwin
(defmethod push-modifier ((browser gtk-browser) event)
  (let* ((modifier-state (gdk:gdk-event-key-state event))
         (key-value (gdk:gdk-event-key-keyval event))
         (key-value-name (gdk:gdk-keyval-name key-value)))
    (when (member :control-mask modifier-state)
      (push :control-mask (modifiers browser)))
    (when (member :shift-mask modifier-state)
      (push :shift-mask (modifiers browser)))
    (when (or (string= key-value-name "Arabic_switch")
              (string= key-value-name "Alt_L")
              (string= key-value-name "Alt_R"))
      (push :mod1-mask (modifiers browser)))
    (when (and (member :mod2-mask modifier-state)
               (member :meta-mask modifier-state))
      (push :super-mask (modifiers browser))))
  (setf (modifiers browser) (delete-duplicates (modifiers browser))))

#+darwin
(defmethod pop-modifier ((browser gtk-browser) event)
  (let* ((modifier-state (gdk:gdk-event-key-state event))
         (key-value (gdk:gdk-event-key-keyval event))
         (key-value-name (gdk:gdk-keyval-name key-value)))
    (when (member :control-mask modifier-state)
      (alex:deletef (modifiers browser) :control-mask))
    (when (member :shift-mask modifier-state)
      (alex:deletef (modifiers browser) :shift-mask))
    (when (or (string= key-value-name "Arabic_switch")
              (string= key-value-name "Alt_L")
              (string= key-value-name "Alt_R"))
      (alex:deletef (modifiers browser) :mod1-mask))
    (when (and (member :mod2-mask modifier-state)
               (member :meta-mask modifier-state))
      (alex:deletef (modifiers browser) :super-mask))))

(-> translate-modifiers (list &optional gdk:gdk-event) list)
(defun translate-modifiers (modifier-state &optional event)
  "Return list of modifiers fit for `keymap:make-key'.
See `gtk-browser's `modifier-translator' slot."
  (declare (ignore event))
  (let ((plist '(:control-mask "control"
                 :mod1-mask "meta"
                 :mod5-mask nil
                 :shift-mask "shift"
                 :super-mask "super"
                 :hyper-mask "hyper"
                 :meta-mask nil
                 :lock-mask nil)))
    (delete nil (mapcar (lambda (mod) (getf plist mod)) modifier-state))))

#+darwin
(defun key-event-modifiers (key-event)
  (declare (ignore key-event))
  (modifiers *browser*))

#-darwin
(defun key-event-modifiers (key-event)
  (gdk:gdk-event-key-state key-event))

;; REVIEW: Remove after upstream fix is merged in Quicklisp, see https://github.com/crategus/cl-cffi-gtk/issues/74.
;; Wait for https://github.com/Ferada/cl-cffi-gtk/issues/new.
(defun gdk-event-button-state (button-event)
  "Return BUTTON-EVENT modifiers as a `gdk-modifier-type', i.e. a list of keywords."
  (let ((state (gdk:gdk-event-button-state button-event)))
    (if (listp state)
        state
        (cffi:with-foreign-objects ((modifiers 'gdk:gdk-modifier-type))
          (setf (cffi:mem-ref modifiers 'gdk:gdk-modifier-type) state)
          (cffi:mem-ref modifiers 'gdk:gdk-modifier-type)))))

#+darwin
(defun button-event-modifiers (button-event)
  (declare (ignore button-event))
  (modifiers *browser*))

#-darwin
(defun button-event-modifiers (button-event)
  (gdk-event-button-state button-event))

#+darwin
(defun scroll-event-modifiers (scroll-event)
  (declare (ignore scroll-event))
  (modifiers *browser*))

#-darwin
(defun scroll-event-modifiers (scroll-event)
  (gdk:gdk-event-scroll-state scroll-event))

(defmethod printable-p ((window gtk-window) event)
  "Return the printable value of EVENT."
  ;; Generate the result of the current keypress into the dummy
  ;; key-string-buffer (a GtkEntry that's never shown on screen) so that we
  ;; can collect the printed representation of composed keypress, such as dead
  ;; keys.
  (gtk:gtk-entry-im-context-filter-keypress (key-string-buffer window) event)
  (when (<= 1 (gtk:gtk-entry-text-length (key-string-buffer window)))
    (prog1
        (match (gtk:gtk-entry-text (key-string-buffer window))
          ;; Special cases: these characters are not supported as is for keyspecs.
          ;; See `self-insert' for the reverse translation.
          (" " "space")
          ("-" "hyphen")
          (character character))
      (setf (gtk:gtk-entry-text (key-string-buffer window)) ""))))

(defun update-prompt (buffer)
  (run-thread "Prompt updater"
    ;; Rebind prompt-buffer to ensure the watcher does not mix up the
    ;; different prompt-buffers.
    (let ((prompt-buffer buffer))
      (update-prompt-input prompt-buffer))))

(define-ffi-method on-signal-key-press-event ((sender gtk-window) event)
  (let* ((keycode (gdk:gdk-event-key-hardware-keycode event))
         (keyval (gdk:gdk-event-key-keyval event))
         (keyval-name (gdk:gdk-keyval-name keyval))
         (character (gdk:gdk-keyval-to-unicode keyval))
         (printable-value (printable-p sender event))
         (key-string (or printable-value
                         (derive-key-string keyval-name character)))
         (modifiers (funcall (modifier-translator *browser*)
                             (key-event-modifiers event)
                             event))
         (buffer (or (current-prompt-buffer)
                     (active-buffer sender))))
    (if modifiers
        (log:debug key-string keycode character keyval-name modifiers)
        (log:debug key-string keycode character keyval-name))
    (when (prompt-buffer-p buffer)
      (update-prompt buffer))
    (if key-string
        (progn
          (alex:appendf (key-stack sender)
                        (list (keymap:make-key :code keycode
                                               :value key-string
                                               :modifiers modifiers
                                               :status :pressed)))
          (funcall (input-dispatcher sender) event
                   buffer
                   sender printable-value))
        ;; Do not forward modifier-only to renderer.
        t)))

(defmethod gtk-object ((prompt-buffer prompt-buffer))
  (prompt-buffer-view (window prompt-buffer)))

(define-ffi-method on-signal-key-release-event ((sender gtk-window) event)
  (declare (ignore sender event))
  ;; REVIEW: Is there any use for handling release events?
  nil)

(defun sender-window (sender)
  (or (find sender (window-list) :key #'active-prompt-buffers)
      (find sender (window-list) :key #'active-buffer)
      (current-window)))

(define-ffi-method on-signal-button-press-event ((sender gtk-buffer) event)
  (let* ((button (gdk:gdk-event-button-button event))
         ;; REVIEW: No need to store X and Y?
         ;; (x (gdk:gdk-event-button-x event))
         ;; (y (gdk:gdk-event-button-y event))
         (window (sender-window sender))
         (key-string (format nil "button~s" button))
         (modifiers (funcall (modifier-translator *browser*)
                             (button-event-modifiers event)
                             event))
         (buffer (or (current-prompt-buffer)
                     sender)))
    (when (prompt-buffer-p buffer)
      (update-prompt buffer))
    (when key-string
      (alex:appendf (key-stack window)
                    (list (keymap:make-key
                           :value key-string
                           :modifiers modifiers
                           :status :pressed)))
      (funcall (input-dispatcher window) event sender window nil))))

(define-ffi-method on-signal-scroll-event ((sender gtk-buffer) event)
  (let* ((button (match (gdk:gdk-event-scroll-direction event)
                   (:up 4)
                   (:down 5)
                   (:left 6)
                   (:right 7)
                   (:smooth
                    (cond
                      ((>= 0 (gdk:gdk-event-scroll-delta-y event))
                       4)
                      ((< 0 (gdk:gdk-event-scroll-delta-y event))
                       5)
                      ((>= 0 (gdk:gdk-event-scroll-delta-x event))
                       6)
                      ((< 0 (gdk:gdk-event-scroll-delta-x event))
                       7)))))
         (window (sender-window sender))
         (key-string (format nil "button~s" button))
         (modifiers (funcall (modifier-translator *browser*)
                             (scroll-event-modifiers event)
                             event)))
    (when key-string
      (alex:appendf (key-stack window)
                    (list (keymap:make-key
                           :value key-string
                           :modifiers modifiers
                           :status :pressed)))
      (funcall (input-dispatcher window) event sender window nil))))

(defun make-data-manager (buffer)
  (let* ((path (expand-path (data-manager-path buffer)))
         (manager (apply #'make-instance `(webkit:webkit-website-data-manager
                                           ,@(when path `(:base-data-directory ,path))
                                           :is-ephemeral ,(not path)))))
    manager))

(defun process-gopher-scheme (request)
  (let* ((url (webkit:webkit-uri-scheme-request-get-uri request))
         (contents (cl-gopher:get-line-contents (cl-gopher:parse-gopher-uri url))))
    (render-gopher-contents contents)))

(defun make-context (&optional buffer)
  ;; This is to ensure that paths are not expanded when we make
  ;; contexts for `nosave-buffer's.
  (with-current-buffer buffer
    (let* ((manager (make-data-manager buffer))
           (context (make-instance 'webkit-web-context :website-data-manager manager))
           (cookie-manager (webkit:webkit-web-context-get-cookie-manager context))
           (extensions-path (expand-path (gtk-extensions-path buffer))))
      (webkit:webkit-web-context-add-path-to-sandbox
       context (namestring (asdf:system-relative-pathname :nyxt "libraries/web-extensions/")) t)
      (when extensions-path
        ;; TODO: Should we also use `connect-signal' here?  Does this yield a memory leak?
        (gobject:g-signal-connect
         context "initialize-web-extensions"
         (lambda (context)
           (with-protect ("Error in signal thread: ~a" :condition)
             (webkit:webkit-web-context-set-web-extensions-directory
              context extensions-path)
             (webkit:webkit-web-context-set-web-extensions-initialization-user-data
              context (glib:g-variant-new-string
                       (flet ((describe-extension (extension &key privileged-p)
                                (cons (nyxt/web-extensions::name extension)
                                      (vector (id extension)
                                              (nyxt/web-extensions::manifest extension)
                                              (if privileged-p 1 0)
                                              (nyxt/web-extensions::extension-files extension)
                                              (id buffer)))))
                         (let ((extensions
                                 (when buffer
                                   (sera:filter #'nyxt/web-extensions::extension-p (modes buffer)))))
                           (encode-json
                            (if (or (background-buffer-p buffer)
                                    (panel-buffer-p buffer))
                                (alex:when-let* ((extension
                                                  (or (find buffer extensions :key #'background-buffer)
                                                      (find buffer extensions :key #'nyxt/web-extensions:popup-buffer))))
                                  (list (describe-extension extension :privileged-p t)))
                                (mapcar #'describe-extension extensions)))))))))))
      ;; Is not used anywhere at the moment.
      (webkit:webkit-web-context-register-uri-scheme-callback
       context "web-extension"
       (lambda (request)
         (let ((data "<h1>Resource not found</h1>")
               (type "text/html"))
           (with-protect ("Error while processing the web-extension scheme: ~a" :condition)
             (sera:and-let* ((path (webkit:webkit-uri-scheme-request-get-path request))
                             (parts (str:split "/" path :limit 2))
                             (extension-id (first parts))
                             (inner-path (second parts))
                             (extension (find extension-id (sera:filter #'nyxt/web-extensions::extension-p
                                                                        (modes buffer))
                                              :key #'id
                                              :test #'string-equal))
                             (full-path (nyxt/web-extensions:merge-extension-path extension inner-path)))
               (setf data (alex:read-file-into-byte-vector full-path)
                     type (mimes:mime full-path))))
           (values data type)))
       (lambda (condition)
         (echo-warning "Error while re-routing web accessible resource: ~a" condition)))
      (webkit:webkit-web-context-register-uri-scheme-callback
       context "nyxt"
       (lambda (request)
         (with-protect ("Error while processing the \"nyxt:\" URL: ~a" :condition)
           (sera:and-let* ((url (quri:uri (webkit:webkit-uri-scheme-request-get-uri request)))
                           (function-name (parse-nyxt-url url))
                           (page-generating-function (gethash function-name *nyxt-url-commands*)))
                          (let ((result (multiple-value-list (apply page-generating-function
                                                                    (nth-value 1 (parse-nyxt-url url))))))
               (cond
                 ((and (alex:length= result 2)
                       (arrayp (first result))
                       (stringp (second result)))
                  (values (first result) (second result)))
                 ((arrayp (first result))
                  (first result))
                 (t (error "Cannot display evaluation result")))))))
       (lambda (condition)
         (echo-warning "Error while routing \"nyxt:\" URL: ~a" condition)))
      (webkit:webkit-web-context-register-uri-scheme-callback
       context "lisp"
       (lambda (request)
         (let ((url (quri:uri (webkit:webkit-uri-scheme-request-get-uri request))))
           (if (or (status-buffer-p buffer)
                   (panel-buffer-p buffer)
                   (internal-url-p (url buffer)))
               (let* ((schemeless-url (schemeless-url url))
                      (code-raw (quri:url-decode schemeless-url :lenient t))
                      ;; All URLs WebKitGTK gives us end with an unnecessary forward slash.
                      (code (sera:slice code-raw 0 -1)))
                 (log:debug "Evaluate Lisp code from internal page: ~a" code)
                 (values (let ((result (first (evaluate code))))
                           ;; Objects and other complex structures make cl-json choke.
                           (unless (or (typep result 'standard-object)
                                       (and (typep result 'cons)
                                            (some #'listp result)))
                             (cl-json:encode-json-to-string result)))
                         "application/json"))
               (values "undefined" "application/json"))))
       (lambda (condition)
         (echo-warning "Error while routing \"lisp:\" URL: ~a" condition)))
      (webkit:webkit-web-context-register-uri-scheme-callback
       context "gopher"
       #'process-gopher-scheme
       (lambda (condition)
         (echo-warning "Error while routing \"gopher:\" URL: ~a" condition)))
      (webkit:webkit-security-manager-register-uri-scheme-as-local
       (webkit:webkit-web-context-get-security-manager context) "nyxt")
      (webkit:webkit-security-manager-register-uri-scheme-as-cors-enabled
       (webkit:webkit-web-context-get-security-manager context) "lisp")
      (when (and buffer
                 (web-buffer-p buffer)
                 (expand-path (cookies-path buffer)))
        (webkit:webkit-cookie-manager-set-persistent-storage
         cookie-manager
         (expand-path (cookies-path buffer))
         :webkit-cookie-persistent-storage-text)
        (set-cookie-policy cookie-manager (default-cookie-policy buffer)))
      context)))

(defmethod initialize-instance :after ((buffer gtk-buffer) &key)
  (ffi-buffer-make buffer))

(define-ffi-method ffi-buffer-url ((buffer gtk-buffer))
  (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer))))

(define-ffi-method ffi-buffer-title ((buffer gtk-buffer))
  (or (webkit:webkit-web-view-title (gtk-object buffer)) ""))

(define-ffi-method on-signal-load-failed-with-tls-errors ((buffer gtk-buffer) certificate url)
  "Return nil to propagate further (i.e. raise load-failed signal), T otherwise."
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (host (quri:uri-host url)))
    (if (and (certificate-exceptions buffer)
             (member host (certificate-exceptions buffer) :test #'string=))
        (progn
          (webkit:webkit-web-context-allow-tls-certificate-for-host
           context
           (gobject:pointer certificate)
           host)
          (buffer-load url :buffer buffer)
          t)
        (progn
          (tls-help buffer url)
          t))))

(define-ffi-method on-signal-decide-policy ((buffer gtk-buffer) response-policy-decision policy-decision-type-response)
  (let ((is-new-window nil) (is-known-type t) (event-type :other)
        (navigation-action nil) (navigation-type nil)
        (mouse-button nil) (modifiers ())
        (url nil) (request nil))
    (match policy-decision-type-response
      (:webkit-policy-decision-type-navigation-action
       (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision)))
      (:webkit-policy-decision-type-new-window-action
       (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision))
       (setf is-new-window t))
      (:webkit-policy-decision-type-response
       (setf request (webkit:webkit-response-policy-decision-request response-policy-decision))
       (setf is-known-type
             (webkit:webkit-response-policy-decision-is-mime-type-supported
              response-policy-decision))))
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
      (setf navigation-action (webkit:webkit-navigation-policy-decision-get-navigation-action
                               response-policy-decision))
      (setf request (webkit:webkit-navigation-action-get-request navigation-action))
      (setf mouse-button (format nil "button~d"
                                 (webkit:webkit-navigation-action-get-mouse-button
                                  navigation-action)))
      (setf modifiers (funcall (modifier-translator *browser*)
                               (webkit:webkit-navigation-action-get-modifiers navigation-action))))
    (setf url (quri:uri (webkit:webkit-uri-request-uri request)))
    (let ((request-data (preprocess-request
                         (make-instance 'request-data
                                        :buffer buffer
                                        :url (quri:copy-uri url)
                                        :keys (unless (uiop:emptyp mouse-button)
                                                (list (keymap:make-key
                                                       :value mouse-button
                                                       :modifiers modifiers)))
                                        :event-type event-type
                                        :new-window-p is-new-window
                                        :known-type-p is-known-type))))
      (if request-data
          (if (null (hooks:handlers (request-resource-hook buffer)))
              (progn
                (log:debug "Forward to ~s's renderer (no request-resource-hook handlers)."
                           buffer)
                (webkit:webkit-policy-decision-use response-policy-decision))
              (let ((request-data
                      (hooks:run-hook
                       (request-resource-hook buffer)
                       (hooks:run-hook (pre-request-hook buffer)
                                       request-data))))
                (cond
                  ((not (typep request-data 'request-data))
                   (log:debug "Don't forward to ~s's renderer (non request data)."
                              buffer)
                   (webkit:webkit-policy-decision-ignore response-policy-decision))
                  ((quri:uri= url (url request-data))
                   (log:debug "Forward to ~s's renderer (unchanged URL)."
                              buffer)
                   (webkit:webkit-policy-decision-use response-policy-decision))
                  ((and (quri:uri= (url buffer) (quri:uri (webkit:webkit-uri-request-uri request)))
                        (not (quri:uri= (quri:uri (webkit:webkit-uri-request-uri request))
                                        (url request-data))))
                   ;; Low-level URL string, we must not render the puni codes so use
                   ;; `quri:render-uri'.
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
                   (webkit:webkit-policy-decision-use response-policy-decision)))))
          (progn
            (log:debug "Don't forward to ~s's renderer (non request data)."
                       buffer)
            (webkit:webkit-policy-decision-ignore response-policy-decision))))))

(defmethod on-signal-load-changed ((buffer gtk-buffer) load-event)
  ;; `url' can be nil if buffer didn't have any URL associated
  ;; to the web view, e.g. the start page, or if the load failed.
  (let* ((url (ignore-errors
               (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer)))))
         (url (if (url-empty-p url)
                  (url buffer)
                  url)))
    (cond ((eq load-event :webkit-load-started)
           (setf (slot-value buffer 'status) :loading)
           (print-status nil (get-containing-window-for-buffer buffer *browser*))
           (echo "Loading ~s." (render-url url)))
          ((eq load-event :webkit-load-redirected)
           (on-signal-load-redirected buffer url))
          ((eq load-event :webkit-load-committed)
           (on-signal-load-committed buffer url))
          ((eq load-event :webkit-load-finished)
           (setf (loading-webkit-history-p buffer) nil)
           (unless (eq (slot-value buffer 'status) :failed)
             (setf (slot-value buffer 'status) :finished))
           (on-signal-load-finished buffer url)
           (print-status nil (get-containing-window-for-buffer buffer *browser*))
           (echo "Finished loading ~s." (render-url url))))))

(define-ffi-method on-signal-mouse-target-changed ((buffer gtk-buffer) hit-test-result modifiers)
  (declare (ignore modifiers))
  (alex:if-let ((url (or (webkit:webkit-hit-test-result-link-uri hit-test-result)
                         (webkit:webkit-hit-test-result-image-uri hit-test-result)
                         (webkit:webkit-hit-test-result-media-uri hit-test-result))))
    (progn
      (print-message (str:concat "→ " (quri:url-decode url :lenient t)))
      (setf (url-at-point buffer) (quri:uri url)))
    (progn
      (print-message "")
      (setf (url-at-point buffer) (quri:uri "")))))

(define-ffi-method ffi-window-make ((browser gtk-browser))
  "Make a window."
  (make-instance 'user-window))

(define-ffi-method ffi-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (gtk:gtk-window-present (gtk-object window))
  (setf (slot-value *browser* 'last-active-window) window))

(define-ffi-method ffi-window-set-title ((window gtk-window) title)
  "Set the title for a window."
  (setf (gtk:gtk-window-title (gtk-object window)) title))

(define-ffi-method ffi-window-active ((browser gtk-browser))
  "Return the window object for the current window."
  (setf (slot-value browser 'last-active-window)
        (or (find-if #'gtk:gtk-window-is-active (window-list) :key #'gtk-object)
            (slot-value browser 'last-active-window)
            (first (window-list)))))

(define-ffi-method ffi-window-set-buffer ((window gtk-window) (buffer gtk-buffer) &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (let ((old-buffer (active-buffer window)))
    (gtk:gtk-container-remove (main-buffer-container window) (gtk-object old-buffer))
    (gtk:gtk-box-pack-start (main-buffer-container window) (gtk-object buffer) :expand t :fill t)
    (gtk:gtk-widget-show (gtk-object buffer))
    (when focus
      (gtk:gtk-widget-grab-focus (gtk-object buffer)))
    (nyxt/web-extensions::tabs-on-activated old-buffer buffer)
    buffer))

(define-ffi-method ffi-window-add-panel-buffer ((window gtk-window) (buffer panel-buffer) side)
  "Add a panel buffer to a window."
  (match side
    (:left (gtk:gtk-box-pack-start (panel-buffer-container-left window) (gtk-object buffer))
           (push buffer (panel-buffers-left window)))
    (:right (gtk:gtk-box-pack-end (panel-buffer-container-right window) (gtk-object buffer))
            (push buffer (panel-buffers-right window))))
  (setf (gtk:gtk-widget-size-request (gtk-object buffer))
        (list (width buffer) -1))
  (gtk:gtk-widget-show (gtk-object buffer)))

(define-ffi-method ffi-window-set-panel-buffer-width ((window gtk-window) (buffer panel-buffer) width)
  "Set the width of a panel buffer."
  (setf (gtk:gtk-widget-size-request (gtk-object buffer))
        (list width -1)))

(define-ffi-method ffi-window-delete-panel-buffer ((window gtk-window) (buffer panel-buffer))
  "Remove a panel buffer from a window."
  (cond ((find buffer (panel-buffers-left window))
         (setf (panel-buffers-left window) (remove buffer (panel-buffers-left window)))
         (gtk:gtk-container-remove (panel-buffer-container-left window) (gtk-object buffer)))
        ((find buffer (panel-buffers-right window))
         (setf (panel-buffers-right window) (remove buffer (panel-buffers-right window)))
         (gtk:gtk-container-remove (panel-buffer-container-right window) (gtk-object buffer)))))

(define-ffi-method ffi-window-set-prompt-buffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (prompt-buffer-container window))
        (list -1 height))
  (if (eql 0 height)
      (gtk:gtk-widget-grab-focus (gtk-object (active-buffer window)))
      (gtk:gtk-widget-grab-focus (prompt-buffer-view window))))

(define-ffi-method ffi-window-get-prompt-buffer-height ((window gtk-window))
  (nth-value 1 (gtk:gtk-widget-size-request (prompt-buffer-container window))))

(define-ffi-method ffi-window-get-status-buffer-height ((window gtk-window))
  (nth-value 1 (gtk:gtk-widget-size-request (status-container window))))

(define-ffi-method ffi-window-set-status-buffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (status-container window))
        (list -1 height)))

(define-ffi-method ffi-window-get-message-buffer-height ((window gtk-window))
  (nth-value 1 (gtk:gtk-widget-size-request (message-container window))))

(define-ffi-method ffi-window-set-message-buffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (message-container window))
        (list -1 height)))

(defun process-file-chooser-request (web-view file-chooser-request)
  (declare (ignore web-view))
  (with-protect ("Failed to process file chooser request: ~a" :condition)
    (when (native-dialogs *browser*)
      (gobject:g-object-ref (gobject:pointer file-chooser-request))
      (run-thread
        (let ((files (mapcar
                      #'uiop:native-namestring
                      (handler-case
                          (prompt :prompt (format
                                           nil "File~@[s~*~] to input"
                                           (webkit:webkit-file-chooser-request-select-multiple
                                            file-chooser-request))
                                  :input (or
                                          (and
                                           (webkit:webkit-file-chooser-request-selected-files
                                            file-chooser-request)
                                           (first
                                            (webkit:webkit-file-chooser-request-selected-files
                                             file-chooser-request)))
                                          (uiop:native-namestring (uiop:getcwd)))
                                  :sources (list (make-instance 'file-source)))
                        (nyxt-prompt-buffer-canceled ()
                          nil)))))
          (if files
              (webkit:webkit-file-chooser-request-select-files
               file-chooser-request
               (cffi:foreign-alloc :string
                                   :initial-contents (if (webkit:webkit-file-chooser-request-select-multiple
                                                          file-chooser-request)
                                                         (mapcar #'cffi:foreign-string-alloc files)
                                                         (list (cffi:foreign-string-alloc (first files))))
                                   :count (if (webkit:webkit-file-chooser-request-select-multiple
                                               file-chooser-request)
                                              (length files)
                                              1)
                                   :null-terminated-p t))
              (webkit:webkit-file-chooser-request-cancel file-chooser-request))
          t)))))

(defun process-script-dialog (web-view dialog)
  (declare (ignore web-view))
  (with-protect ("Failed to process dialog: ~a" :condition)
    (when (native-dialogs *browser*)
      (let ((dialog (gobject:pointer dialog)))
        (webkit:webkit-script-dialog-ref dialog)
        (run-thread
          (case (webkit:webkit-script-dialog-get-dialog-type dialog)
            (:webkit-script-dialog-alert (echo (webkit:webkit-script-dialog-get-message dialog)))
            (:webkit-script-dialog-prompt
             (let ((text (first (handler-case
                                    (prompt
                                     :input (webkit:webkit-script-dialog-prompt-get-default-text dialog)
                                     :prompt (webkit:webkit-script-dialog-get-message dialog)
                                     :sources (list (make-instance 'prompter:raw-source)))
                                  (nyxt-prompt-buffer-canceled (c) (declare (ignore c)) nil)))))
               (if text
                   (webkit:webkit-script-dialog-prompt-set-text dialog text)
                   (progn
                     (webkit:webkit-script-dialog-prompt-set-text dialog (cffi:null-pointer))
                     (webkit:webkit-script-dialog-close dialog)))))
            (:webkit-script-dialog-confirm
             (webkit:webkit-script-dialog-confirm-set-confirmed
              dialog (if-confirm
                      ((webkit:webkit-script-dialog-get-message dialog))
                      t nil)))
            (:webkit-script-dialog-before-unload-confirm
             (webkit:webkit-script-dialog-confirm-set-confirmed
              dialog (if-confirm
                      ;; FIXME: This asks for keyword override in if-confirm.
                      ((format nil "~a ['yes' = leave, 'no' = stay]" (webkit:webkit-script-dialog-get-message dialog)))
                      t nil))))
          (webkit:webkit-script-dialog-close dialog)
          (webkit:webkit-script-dialog-unref dialog))
        t))))

(defun process-permission-request (web-view request)
  (g:g-object-ref (g:pointer request))
  (run-thread
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
                             request))))))
                (webkit:webkit-permission-request-allow request)
                (webkit:webkit-permission-request-deny request))))

(define-ffi-method ffi-buffer-make ((buffer gtk-buffer))
  "Initialize BUFFER's GTK web view."
  (unless (gtk-object buffer) ; Buffer may already have a view, e.g. the prompt-buffer.
    (setf (gtk-object buffer) (make-web-view :context-buffer buffer)))
  (if (smooth-scrolling buffer)
      (ffi-buffer-enable-smooth-scrolling buffer t)
      (ffi-buffer-enable-smooth-scrolling buffer nil))
  (connect-signal-function buffer "decide-policy" (make-decide-policy-handler buffer))
  (connect-signal buffer "load-changed" t (web-view load-event)
    (declare (ignore web-view))
    (on-signal-load-changed buffer load-event))
  (connect-signal buffer "mouse-target-changed" nil (web-view hit-test-result modifiers)
    (declare (ignore web-view))
    (on-signal-mouse-target-changed buffer hit-test-result modifiers))
  ;; Mouse events are captured by the web view first, so we must intercept them here.
  (connect-signal buffer "button-press-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-button-press-event buffer event))
  (connect-signal buffer "scroll-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-scroll-event buffer event))
  (connect-signal-function buffer "script-dialog" #'process-script-dialog)
  (connect-signal-function buffer "run-file-chooser" #'process-file-chooser-request)
  (connect-signal-function buffer "permission-request" #'process-permission-request)
  ;; TLS certificate handling
  (connect-signal buffer "load-failed-with-tls-errors" nil (web-view failing-url certificate errors)
    (declare (ignore web-view errors))
    (on-signal-load-failed-with-tls-errors buffer certificate (quri:uri failing-url)))
  (connect-signal buffer "notify::uri" nil (web-view param-spec)
    (declare (ignore web-view param-spec))
    (on-signal-notify-uri buffer nil))
  (connect-signal buffer "notify::title" nil (web-view param-spec)
    (declare (ignore web-view param-spec))
    (on-signal-notify-title buffer nil))
  (connect-signal buffer "web-process-terminated" nil (web-view reason)
    ;; TODO: Bind WebKitWebProcessTerminationReason in cl-webkit.
    (echo-warning
     "Web process terminated for buffer ~a (opening ~a) because ~[it crashed~;of memory exhaustion~;we had to close it~]"
     (id buffer)
     (url buffer)
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (log:debug
     "Web process terminated for web view ~a because of ~[WEBKIT_WEB_PROCESS_CRASHED~;WEBKIT_WEB_PROCESS_EXCEEDED_MEMORY_LIMIT~;WEBKIT_WEB_PROCESS_TERMINATED_BY_API~]"
     web-view
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (buffer-delete buffer))
  (connect-signal buffer "close" nil (web-view)
    (mapc (lambda (handler-id)
            (gobject:g-signal-handler-disconnect web-view handler-id))
          (handler-ids buffer))
    (buffer-hide buffer)
    (gtk:gtk-widget-destroy web-view)
    (setf (gtk-object buffer) nil))
  (connect-signal buffer "load-failed" nil (web-view load-event failing-url error)
    (declare (ignore load-event web-view))
    ;; TODO: WebKitGTK sometimes (when?) triggers "load-failed" when loading a
    ;; page from the webkit-history cache.  Upstream bug?  Anyways, we should
    ;; ignore these.
    (if (loading-webkit-history-p buffer)
        (setf (loading-webkit-history-p buffer) nil)
        (unless (or (member (slot-value buffer 'status) '(:finished :failed))
                    ;; WebKitGTK emits the WEBKIT_PLUGIN_ERROR_WILL_HANDLE_LOAD
                    ;; (204) if the plugin will handle loading content of the
                    ;; URL. This often happens with videos. The only thing we
                    ;; can do is ignore it.
                    ;;
                    ;; TODO: Use cl-webkit provided error types. How
                    ;; do we use it, actually?
                    (= 204 (webkit::g-error-code error))
                    (= 302 (webkit::g-error-code error)))
          (echo "Failed to load URL ~a in buffer ~a." failing-url (id buffer))
          (setf (slot-value buffer 'status) :failed)
          (html-set
           (spinneret:with-html-string
             (:h1 "Page could not be loaded.")
             (:h2 "URL: " failing-url)
             (:ul
              (:li "Try again in a moment, maybe the site will be available again.")
              (:li "If the problem persists for every site, check your Internet connection.")
              (:li "Make sure the URL is valid."
                   (when (quri:uri-https-p (quri:uri failing-url))
                     "If this site does not support HTTPS, try with HTTP (insecure)."))))
           buffer)))
    t)
  (connect-signal buffer "create" nil (web-view navigation-action)
    (declare (ignore web-view))
    (let ((new-buffer (buffer-make *browser*))
          (url (webkit:webkit-uri-request-uri
                (webkit:webkit-navigation-action-get-request
                 (gobject:pointer navigation-action)))))
      (ffi-buffer-load new-buffer (quri:uri url))
      (window-set-buffer (current-window) new-buffer)
      (gtk-object new-buffer)))
  ;; Remove "download to disk" from the right click context menu because it
  ;; bypasses request resource signal
  (connect-signal buffer "context-menu" nil (web-view context-menu event hit-test-result)
    (declare (ignore web-view event hit-test-result))
    (let ((length (webkit:webkit-context-menu-get-n-items context-menu)))
      (dotimes (i length)
        (let ((item (webkit:webkit-context-menu-get-item-at-position context-menu i)))
          (match (webkit:webkit-context-menu-item-get-stock-action item)
            (:webkit-context-menu-action-download-link-to-disk
             (webkit:webkit-context-menu-remove context-menu item))))))
    nil)
  (connect-signal buffer "enter-fullscreen" nil (web-view)
    (declare (ignore web-view))
    (hooks:run-hook toggle-fullscreen-before-hook)
    (present-current-window)
    (hooks:run-hook toggle-fullscreen-after-hook)
    nil)
  (connect-signal buffer "leave-fullscreen" nil (web-view)
    (declare (ignore web-view))
    (hooks:run-hook toggle-fullscreen-before-hook)
    (unpresent-current-window)
    (hooks:run-hook toggle-fullscreen-after-hook)
    nil)
  (connect-signal buffer "user-message-received" nil (view message)
    (declare (ignorable view))
    (g:g-object-ref (g:pointer message))
    (run-thread
      "Process user messsage"
      (nyxt/web-extensions:process-user-message buffer message))
    (sleep 0.01)
    (run-thread
      "Reply user message"
      (nyxt/web-extensions:reply-user-message buffer message))
    t)
  (nyxt/web-extensions::tabs-on-created buffer)
  buffer)

(define-ffi-method ffi-buffer-delete ((buffer gtk-buffer))
  (nyxt/web-extensions::tabs-on-removed buffer)
  (if (slot-value buffer 'gtk-object) ; Not all buffers have their own web view, e.g. prompt buffers.
      (webkit:webkit-web-view-try-close (gtk-object buffer))
      (buffer-hide buffer)))

(define-ffi-method ffi-buffer-load ((buffer gtk-buffer) url)
  "Load URL in BUFFER.
An optimization technique is to make use of the renderer history cache.
For WebKit, if the URL matches an entry in the webkit-history then we fetch the
page from the cache.

We don't use the cache if URL matches BUFFER's URL since this means the user
requested a reload."
  (declare (type quri:uri url))
  (let* ((history (webkit-history buffer))
         (entry (or (find url history :test #'quri:uri= :key #'webkit-history-entry-url)
                    (find url history :test #'quri:uri= :key #'webkit-history-entry-original-url))))
    ;; Mark buffer as :loading right away so functions like `window-set-buffer'
    ;; don't try to reload if they are called before the "load-changed" signal
    ;; is emitted.
    (setf (slot-value buffer 'status) :loading)
    (if (and entry (not (quri:uri= url (url buffer))))
        (progn
          (log:debug "Load URL from history entry ~a" entry)
          (load-webkit-history-entry buffer entry))
        (webkit:webkit-web-view-load-uri (gtk-object buffer) (quri:render-uri url)))))

(defmethod ffi-buffer-evaluate-javascript ((buffer gtk-buffer) javascript &optional world-name)
  (%within-renderer-thread
   (lambda (&optional channel)
     (when (gtk-object buffer)
       (webkit2:webkit-web-view-evaluate-javascript
        (gtk-object buffer)
        javascript
        (if channel
            (lambda (result jsc-result)
              (declare (ignore jsc-result))
              (calispel:! channel result))
          (lambda (result jsc-result)
            (declare (ignore jsc-result))
            result))
        (lambda (condition)
          (javascript-error-handler condition)
          ;; Notify the listener that we are done.
          (when channel
            (calispel:! channel nil)))
        world-name)))))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer gtk-buffer) javascript &optional world-name)
  (%within-renderer-thread-async
   (lambda ()
     (when (gtk-object buffer)
       (webkit2:webkit-web-view-evaluate-javascript
        (gtk-object buffer)
        javascript
        nil
        #'javascript-error-handler
        world-name)))))

(defun list-of-string-to-foreign (list)
  (if list
      (cffi:foreign-alloc :string
                          :count (length list)
                          :initial-contents list
                          :null-terminated-p t)
      (cffi:null-pointer)))

(define-ffi-method ffi-buffer-add-user-style ((buffer gtk-buffer) css &key
                                              world-name all-frames-p inject-as-author-p
                                              allow-list block-list)
  (let* ((content-manager
           (webkit:webkit-web-view-get-user-content-manager
            (gtk-object buffer)))
         (frames (if all-frames-p
                     :webkit-user-content-inject-all-frames
                     :webkit-user-content-inject-top-frame))
         (style-level (if inject-as-author-p
                          :webkit-user-style-level-author
                          :webkit-user-style-level-user))
         (style-sheet
           (if world-name
               (webkit:webkit-user-style-sheet-new-for-world
                css frames style-level world-name
                (list-of-string-to-foreign allow-list)
                (list-of-string-to-foreign block-list))
               (webkit:webkit-user-style-sheet-new
                css frames style-level
                (list-of-string-to-foreign allow-list)
                (list-of-string-to-foreign block-list)))))
    (webkit:webkit-user-content-manager-add-style-sheet
     content-manager style-sheet)
    style-sheet))

(define-ffi-method ffi-buffer-remove-user-style ((buffer gtk-buffer) style-sheet)
  (let ((content-manager
          (webkit:webkit-web-view-get-user-content-manager
           (gtk-object buffer))))
    (when style-sheet
      (webkit:webkit-user-content-manager-remove-style-sheet
       content-manager style-sheet))))

(define-ffi-method ffi-buffer-add-user-script ((buffer gtk-buffer) javascript &key
                                               world-name all-frames-p
                                               at-document-start-p run-now-p
                                               allow-list block-list)
  (let* ((content-manager
           (webkit:webkit-web-view-get-user-content-manager
            (gtk-object buffer)))
         (frames (if all-frames-p
                     :webkit-user-content-inject-all-frames
                     :webkit-user-content-inject-top-frame))
         (inject-time (if at-document-start-p
                          :webkit-user-script-inject-at-document-start
                          :webkit-user-script-inject-at-document-end))
         (script (if world-name
                     (webkit:webkit-user-script-new-for-world
                      javascript frames inject-time world-name
                      (list-of-string-to-foreign allow-list)
                      (list-of-string-to-foreign block-list))
                     (webkit:webkit-user-script-new
                      javascript frames inject-time
                      (list-of-string-to-foreign allow-list)
                      (list-of-string-to-foreign block-list)))))
    (webkit:webkit-user-content-manager-add-script
     content-manager script)
    (when (and run-now-p
               (member (slot-value buffer 'status)
                       '(:finished :failed)))
      (reload-buffers (list buffer)))
    script))

(define-ffi-method ffi-buffer-remove-user-script ((buffer gtk-buffer) script)
  (let ((content-manager
          (webkit:webkit-web-view-get-user-content-manager
           (gtk-object buffer))))
    (when script
      (webkit:webkit-user-content-manager-remove-script
       content-manager script))))

(define-ffi-method ffi-buffer-enable-javascript ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-javascript
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

(define-ffi-method ffi-buffer-enable-javascript-markup ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-javascript-markup
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

(define-ffi-method ffi-buffer-enable-smooth-scrolling ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-smooth-scrolling
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

#+webkit2-media
(define-ffi-method ffi-buffer-enable-media ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-media
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

(define-ffi-method ffi-buffer-auto-load-image ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-auto-load-images
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

#+webkit2-mute
(defmethod ffi-buffer-enable-sound ((buffer gtk-buffer) value)
  (webkit:webkit-web-view-set-is-muted (gtk-object buffer) (not value)))

(defmethod ffi-buffer-download ((buffer gtk-buffer) url)
  (let* ((webkit-download (webkit:webkit-web-view-download-uri (gtk-object buffer) url))
         (download (make-instance 'user-download
                                  :url url
                                  :gtk-object webkit-download)))
    (setf (cancel-function download)
          #'(lambda ()
              (setf (status download) :canceled)
              (webkit:webkit-download-cancel webkit-download)))
    (push download (downloads *browser*))
    (connect-signal download "received-data" nil (webkit-download data-length)
      (declare (ignore data-length))
      (setf (bytes-downloaded download)
            (webkit:webkit-download-get-received-data-length webkit-download))
      (setf (completion-percentage download)
            (* 100 (webkit:webkit-download-estimated-progress webkit-download))))
    (connect-signal download "decide-destination" nil (webkit-download suggested-file-name)
      (alex:when-let* ((download-dir (download-path buffer))
                       (download-directory (expand-path download-dir))
                       (path (str:concat download-directory suggested-file-name))
                       (unique-path (download-manager::ensure-unique-file path))
                       (file-path (format nil "file://~a" unique-path)))
        (if (string= path unique-path)
            (echo "Destination ~s exists, saving as ~s." path unique-path)
            (log:debug "Downloading file to ~s." unique-path))
        (webkit:webkit-download-set-destination webkit-download file-path)))
    (connect-signal download "created-destination" nil (webkit-download destination)
      (declare (ignore destination))
      (setf (destination-path download)
            (webkit:webkit-download-destination webkit-download)))
    (connect-signal download "failed" nil (webkit-download error)
      (declare (ignore error))
      (unless (eq (status download) :canceled)
        (setf (status download) :failed))
      (echo "Download failed for ~s."
            (webkit:webkit-uri-request-uri
             (webkit:webkit-download-get-request webkit-download))))
    (connect-signal download "finished" nil (webkit-download)
      (declare (ignore webkit-download))
      (unless (member (status download) '(:canceled :failed))
        (setf (status download) :finished)
        ;; If download was too small, it may not have been updated.
        (setf (completion-percentage download) 100)))
    download))

(define-ffi-method ffi-buffer-user-agent ((buffer gtk-buffer) &optional value)
  (if value
      (setf (webkit:webkit-settings-user-agent
             (webkit:webkit-web-view-get-settings (gtk-object buffer)))
            value)
      (webkit:webkit-settings-user-agent
       (webkit:webkit-web-view-get-settings (gtk-object buffer)))))

(define-ffi-method ffi-buffer-webgl-enabled-p ((buffer gtk-buffer))
  (webkit:webkit-settings-enable-webgl
   (webkit:webkit-web-view-get-settings (gtk-object buffer))))

(define-ffi-method ffi-buffer-enable-webgl ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-webgl
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

(define-ffi-method ffi-buffer-set-proxy ((buffer gtk-buffer)
                                         &optional (proxy-url (quri:uri ""))
                                         (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URL.
Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
For the user-level interface, see `proxy-mode'.

Note: WebKit supports three proxy 'modes': default (the system proxy),
custom (the specified proxy) and none."
  (declare (type quri:uri proxy-url))
  (setf (gtk-proxy-url buffer) proxy-url)
  (setf (proxy-ignored-hosts buffer) ignore-hosts)
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (settings (cffi:null-pointer))
         (mode :webkit-network-proxy-mode-no-proxy)
         (ignore-hosts (cffi:foreign-alloc :string
                                           :initial-contents ignore-hosts
                                           :null-terminated-p t)))
    (unless (url-empty-p proxy-url)
      (setf mode :webkit-network-proxy-mode-custom)
      (setf settings
            (webkit:webkit-network-proxy-settings-new
             (render-url proxy-url)
             ignore-hosts)))
    (cffi:foreign-free ignore-hosts)
    (webkit:webkit-web-context-set-network-proxy-settings
     context mode settings)))

(define-ffi-method ffi-buffer-get-proxy ((buffer gtk-buffer))
  "Return the proxy URL and list of ignored hosts (a list of strings) as second value."
  (the (values (or quri:uri null) list-of-strings)
       (values (gtk-proxy-url buffer)
               (proxy-ignored-hosts buffer))))

(define-ffi-method ffi-buffer-set-zoom-level ((buffer gtk-buffer) value)
  (when (and (floatp value) (>= value 0))
    (setf (webkit:webkit-web-view-zoom-level (gtk-object buffer)) value)))

(define-ffi-method ffi-generate-input-event ((window gtk-window) event)
  (when event
    ;; The "send_event" field is used to mark the event as an "unconsumed"
    ;; keypress.  The distinction allows us to avoid looping indefinitely.
    (etypecase event
      (gdk:gdk-event-button
       (setf (gdk:gdk-event-button-send-event event) t))
      (gdk:gdk-event-key
       (setf (gdk:gdk-event-key-send-event event) t))
      (gdk:gdk-event-scroll
       (setf (gdk:gdk-event-scroll-send-event event) t)))
    (gtk:gtk-main-do-event event)))

(define-ffi-method ffi-generated-input-event-p ((window gtk-window) event)
  (gdk:gdk-event-send-event event))

(define-ffi-method ffi-inspector-show ((buffer gtk-buffer))
  (setf (webkit:webkit-settings-enable-developer-extras
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        t)
  (webkit:webkit-web-inspector-show
   (webkit:webkit-web-view-get-inspector (gtk-object buffer))))

(define-ffi-method ffi-print-status ((window gtk-window) text)
  (let ((text (spinneret:with-html-string
               (:head (:style (style (status-buffer window))))
               (:body (:raw text)))))
    (with-slots (status-buffer) window
      (webkit2:webkit-web-view-evaluate-javascript
       (gtk-object (status-buffer window))
       (ps:ps (setf (ps:@ document body |innerHTML|)
                    (ps:lisp text)))))))

(define-ffi-method ffi-print-message ((window gtk-window) text)
  (let ((text (spinneret:with-html-string
               (:head (:style (message-buffer-style window)))
               (:body (:raw text)))))
    (with-slots (message-view) window
      (webkit2:webkit-web-view-evaluate-javascript
       (message-view window)
       (ps:ps (setf (ps:@ document body |innerHTML|)
                    (ps:lisp text)))))))

;; This method does not need a renderer, so no need to use `define-ffi-method'
;; which is prone to race conditions.
(defmethod ffi-display-url (text)
  (webkit:webkit-uri-for-display text))

(-> set-cookie-policy (webkit:webkit-cookie-manager cookie-policy) *)
(defun set-cookie-policy (cookie-manager cookie-policy)
  (webkit:webkit-cookie-manager-set-accept-policy
   cookie-manager
   (match cookie-policy
     (:accept :webkit-cookie-policy-accept-always)
     (:never :webkit-cookie-policy-accept-never)
     (:no-third-party :webkit-cookie-policy-accept-no-third-party))))

(define-ffi-method ffi-buffer-cookie-policy ((buffer gtk-buffer) value)
  "VALUE is one of`:always', `:never' or `:no-third-party'."
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
    (set-cookie-policy cookie-manager value)
    buffer))

(defmethod ffi-set-preferred-languages ((buffer gtk-buffer)
                                        language-list)
  "Set the list of preferred languages in the HTTP header \"Accept-Language:\".
LANGUAGE is a list of strings like '(\"en_US\" \"fr_FR\")."
  (let ((langs (cffi:foreign-alloc :string
                                   :initial-contents language-list
                                   :null-terminated-p t)))
    (webkit:webkit-web-context-set-preferred-languages
     (webkit:webkit-web-view-web-context (gtk-object buffer))
     langs)))

(defstruct webkit-history-entry
  title
  url
  original-url
  gtk-object)

(define-ffi-method webkit-history ((buffer gtk-buffer))
  "Return a list of `webkit-history-entry's for the current buffer.
Oldest entries come last.

This represents the history as remembered by WebKit.  Note that it is linear so
it does not map 1:1 with Nyxt's history tree.  Nonetheless it allows us to make
use of the WebKit history case for the current branch.  See `ffi-buffer-load'.

As a second value, return the current buffer index starting from 0."
  (let* ((bf-list (webkit:webkit-web-view-get-back-forward-list (gtk-object buffer)))
         (length (webkit:webkit-back-forward-list-get-length bf-list))
         (current (webkit:webkit-back-forward-list-get-current-item bf-list))
         (history-list nil)
         (current-index 0))
    ;; The back-forward list is both negatively and positively indexed.  Seems
    ;; that we can't easily know the first index nor the last one.  So let's
    ;; iterate over the length backwards and forwards to make sure we get all
    ;; elements in order.
    (loop for i from (- length) to length
          for item = (webkit:webkit-back-forward-list-get-nth-item bf-list i)
          when (eq item current)
          do (setf current-index (- length (length history-list))) ; Index from 0.
          when item
          do (push (make-webkit-history-entry
                    :title (webkit:webkit-back-forward-list-item-get-title item)
                    :url (quri:uri (webkit:webkit-back-forward-list-item-get-uri item))
                    :original-url (quri:uri (webkit:webkit-back-forward-list-item-get-original-uri item))
                    :gtk-object item)
                   history-list))
    (values history-list current-index)))

(defmethod load-webkit-history-entry ((buffer gtk-buffer) history-entry)
  (setf (loading-webkit-history-p buffer) t)
  (webkit:webkit-web-view-go-to-back-forward-list-item
   (gtk-object buffer)
   (webkit-history-entry-gtk-object history-entry)))

(define-ffi-method ffi-focused-p ((buffer gtk-buffer))
  (gtk:gtk-widget-is-focus (gtk-object buffer)))

;; Working with clipboard
(define-ffi-method (setf clipboard-text) (text (gtk-browser gtk-browser))
  (gtk:gtk-clipboard-set-text
   (gtk:gtk-clipboard-get "CLIPBOARD")
   text))

(define-ffi-method clipboard-text ((gtk-browser gtk-browser))
  (gtk:gtk-clipboard-wait-for-text
   (gtk:gtk-clipboard-get "CLIPBOARD")))

(define-ffi-method ffi-set-tracking-prevention ((buffer gtk-buffer) value)
  #+webkit2-tracking
  (webkit:webkit-website-data-manager-set-itp-enabled
   (webkit:webkit-web-context-website-data-manager
    (webkit:webkit-web-view-web-context (gtk-object buffer)))
   value))

(defmethod ffi-buffer-copy ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-copy+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-copy+)))
   (lambda (e) (echo-warning "Cannot copy: ~a" e))))

(defmethod ffi-buffer-paste ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-paste+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-paste+)))
   (lambda (e) (echo-warning "Cannot paste: ~a" e))))

(defmethod ffi-buffer-cut ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-cut+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-cut+)))
   (lambda (e) (echo-warning "Cannot cut: ~a" e))))

(defmethod ffi-buffer-select-all ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-select-all+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-select-all+)))
   (lambda (e) (echo-warning "Cannot select all: ~a" e))))

(defmethod ffi-buffer-undo ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-undo+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-undo+)))
   (lambda (e) (echo-warning "Cannot undo: ~a" e))))

(defmethod ffi-buffer-redo ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-redo+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-redo+)))
   (lambda (e) (echo-warning "Cannot redo: ~a" e))))
