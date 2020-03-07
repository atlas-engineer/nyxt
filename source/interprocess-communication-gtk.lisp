;;; interprocess-communication.lisp --- IPC for creating interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass gtk-browser (browser)
  ((modifiers :accessor modifiers :initform ())))

(define-class-type browser)
(declaim (type (browser-type) *browser-class*))
@export
(defparameter *browser-class* 'gtk-browser)

(defmethod initialize ((browser gtk-browser) urls startup-timestamp)
  "gtk:within-main-loop handles all the GTK initialization. On
   GNU/Linux, Next could hang after 10 minutes if it's not
   used. Conversely, on Darwin, if gtk:within-main-loop is used, no
   drawing happens. Drawing operations on Darwin MUST originate from
   the main thread, which the GTK main loop is not guaranteed to be
   on."
  (log:debug "Initializing GTK Interface")
  #+linux
  (progn
    (gtk:within-main-loop
      (gdk:gdk-set-program-class "next")
      (finalize browser urls startup-timestamp))
    (gtk:join-gtk-main))
  #+darwin
  (progn
    (gdk:gdk-set-program-class "next")
    (finalize browser urls startup-timestamp)
    (gtk:gtk-main)))

(defmethod kill-interface ((browser gtk-browser))
  (gtk:leave-gtk-main))

@export
@export-accessors
(defclass gtk-window (window)
  ((gtk-object :accessor gtk-object)
   (box-layout :accessor box-layout)
   (minibuffer-container :accessor minibuffer-container)
   (minibuffer-view :accessor minibuffer-view)))

(define-class-type window)
(declaim (type (window-type) *window-class*))
@export
(defparameter *window-class* 'gtk-window)

@export
@export-accessors
(defclass gtk-buffer (buffer)
  ((gtk-object :accessor gtk-object)))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
@export
(defparameter *buffer-class* 'gtk-buffer)

(defmethod initialize-instance :after ((window gtk-window) &key)
  (with-slots (gtk-object box-layout minibuffer-container minibuffer-view active-buffer id) window
    (setf id (get-unique-window-identifier *browser*))
    (setf gtk-object (make-instance 'gtk:gtk-window
                                    :type :toplevel
                                    :default-width 1024
                                    :default-height 768))
    (setf box-layout (make-instance 'gtk:gtk-box
                                    :orientation :vertical
                                    :spacing 0))
    (setf minibuffer-container (make-instance 'gtk:gtk-box
                                              :orientation :vertical
                                              :spacing 0))
    (setf minibuffer-view (make-instance 'webkit:webkit-web-view))
    (setf active-buffer (make-instance *buffer-class*))
    ;; Add the views to the box layout and to the window
    (gtk:gtk-box-pack-start box-layout (gtk-object active-buffer))
    (gtk:gtk-box-pack-end box-layout minibuffer-container :expand nil)
    (gtk:gtk-box-pack-start minibuffer-container minibuffer-view :expand t)
    (setf (gtk:gtk-widget-size-request minibuffer-container)
          (list -1 (status-buffer-height window)))
    (gtk:gtk-container-add gtk-object box-layout)
    (gtk:gtk-widget-show-all gtk-object)
    (gobject:g-signal-connect
     gtk-object "key_press_event"
     (lambda (widget event) (declare (ignore widget))
       (push-modifier *browser* event)
       (process-key-press-event window event)))
    (gobject:g-signal-connect
     gtk-object "key_release_event"
     (lambda (widget event) (declare (ignore widget))
       (pop-modifier *browser* event)))
    (gobject:g-signal-connect
     gtk-object "destroy"
     (lambda (widget) (declare (ignore widget))
       (process-destroy window)))))

(defmethod process-destroy ((window gtk-window))
  ;; remove buffer from window to avoid corruption of buffer
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (window-delete window))

(defmethod ipc-window-destroy ((window gtk-window))
  (gtk:gtk-widget-destroy (gtk-object window)))

(defmethod ipc-window-fullscreen ((window gtk-window))
  (gtk:gtk-window-fullscreen (gtk-object window)))

(defmethod ipc-window-unfullscreen ((window gtk-window))
  (gtk:gtk-window-unfullscreen (gtk-object window)))

(defun character->string (character &optional key-value)
  (match character
    (#\Return "RETURN")
    (#\Backspace "BACKSPACE")
    (#\Esc "ESCAPE")
    (#\- "HYPHEN")
    (#\Space "SPACE")
    (#\Tab "TAB")
    (_ (match key-value
         (65361 "Left")
         (65362 "Up")
         (65364 "Down")
         (65363 "Right")
         (_ (unless (eq character #\Nul)
              (string character)))))))

(defmethod translate-modifier-list ((browser gtk-browser) modifier-state)
  (let ((modifiers ()))
    (when (member :control-mask modifier-state)
      (push "C" modifiers))
    (when (member :mod1-mask modifier-state)
      (push "M" modifiers))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (push "S" modifiers))
    modifiers))

(defmethod push-modifier ((browser gtk-browser) event)
  (let ((modifier-state (gdk:gdk-event-key-state event))
        (key-value (gdk:gdk-event-key-keyval event)))
    (when (member :control-mask modifier-state)
      (push "C" (modifiers browser)))
    (when (member :shift-mask modifier-state)
      (push "s" (modifiers browser)))
    (when (or (member :mod1-mask modifier-state)
              (eq key-value 65406))
      (push "M" (modifiers browser)))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (push "S" (modifiers browser))))
  (setf (modifiers browser) (remove-duplicates (modifiers browser) :test #'equal)))

(defmethod pop-modifier ((browser gtk-browser) event)
  (let ((modifier-state (gdk:gdk-event-key-state event))
        (key-value (gdk:gdk-event-key-keyval event)))
    (when (member :control-mask modifier-state)
      (setf (modifiers browser) (remove "C" (modifiers browser) :test #'equal)))
    (when (member :shift-mask modifier-state)
      (setf (modifiers browser) (remove "s" (modifiers browser) :test #'equal)))
    (when (or (member :mod1-mask modifier-state)
              (eq key-value 65406))
      (setf (modifiers browser) (remove "M" (modifiers browser) :test #'equal)))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (setf (modifiers browser) (remove "S" (modifiers browser) :test #'equal)))))

(defparameter *previous-event* nil)

(defun duplicated-event? (event)        ; Taken from lispkit.
  "For some reason, we sometimes receive duplicate events.
TODO: Report issue to CL-CFFI-GTK."
  (when (and *previous-event* event)
    (= (gdk:GDK-EVENT-KEY-TIME event)
       (gdk:GDK-EVENT-KEY-TIME *previous-event*))))

(defmethod process-key-press-event ((sender gtk-window) event)
  (unless (duplicated-event? event)
    (let* ((character (gdk:gdk-keyval-to-unicode (gdk:gdk-event-key-keyval event)))
           (character-code (char-code character))
           (key-value (gdk:gdk-event-key-keyval event))
           (key-string (character->string character key-value)))
      (when key-string
        (log:debug character-code key-string (modifiers *browser*))
        (push-input-event character-code key-string (modifiers *browser*) -1 -1 nil sender)))))

(declaim (ftype (function (&optional buffer)) make-context))
(defun make-context (&optional buffer)
  (let* ((context (webkit:webkit-web-context-get-default))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
    (when (and buffer (not (str:emptyp (namestring (cookies-path buffer)))))
      (webkit:webkit-cookie-manager-set-persistent-storage
       cookie-manager
       (namestring (cookies-path buffer))
       :webkit-cookie-persistent-storage-text))
    context))

(defmethod initialize-instance :after ((buffer gtk-buffer) &key)
  (next-hooks:run-hook (buffer-before-make-hook *browser*) buffer)
  (setf (id buffer) (get-unique-buffer-identifier *browser*))
  (setf (gtk-object buffer)
        (make-instance 'webkit:webkit-web-view
                       ;; TODO: Should be :web-context, shouldn't it?
                       :context (make-context buffer)))
  (gobject:g-signal-connect
   (gtk-object buffer) "decide-policy"
   (lambda (web-view response-policy-decision policy-decision-type-response)
     (declare (ignore web-view))
     (process-decide-policy buffer response-policy-decision policy-decision-type-response)))
  (gobject:g-signal-connect
   (gtk-object buffer) "load-changed"
   (lambda (web-view load-event)
     (declare (ignore web-view))
     (process-load-changed buffer load-event)))
  (gobject:g-signal-connect
   (gtk-object buffer) "mouse-target-changed"
   (lambda (web-view hit-test-result modifiers)
     (declare (ignore web-view))
     (process-mouse-target-changed buffer hit-test-result modifiers)))
  ;; Modes might require that buffer exists, so we need to initialize them
  ;; after the view has been created.
  (initialize-modes buffer))

(defmethod process-decide-policy ((buffer gtk-buffer) response-policy-decision policy-decision-type-response)
  (let ((is-new-window nil) (is-known-type t) (event-type nil)
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
            (:webkit-navigation-type-other :other)))
    ;; Get Navigation Parameters from WebKitNavigationAction object
    (when navigation-type
      (setf navigation-action (webkit:webkit-navigation-policy-decision-get-navigation-action response-policy-decision))
      (setf request (webkit:webkit-navigation-action-get-request navigation-action))
      (setf mouse-button (format nil "button~d" (webkit:webkit-navigation-action-get-mouse-button navigation-action)))
      (setf modifiers (translate-modifier-list *browser* (webkit:webkit-navigation-action-get-modifiers navigation-action))))
    (setf url (webkit:webkit-uri-request-uri request))
    (request-resource buffer
                      :url url
                      :mouse-button mouse-button
                      :event-type event-type
                      :is-new-window is-new-window
                      :is-known-type is-known-type
                      :modifiers modifiers)))

(defmethod process-load-changed ((buffer gtk-buffer) load-event)
  (let ((url (webkit:webkit-web-view-uri (gtk-object buffer))))
    (cond ((eq load-event :webkit-load-started) nil)
          ((eq load-event :webkit-load-redirected) nil)
          ((eq load-event :webkit-load-committed)
           (did-commit-navigation buffer url))
          ((eq load-event :webkit-load-finished)
           (did-finish-navigation buffer url)))))

(defmethod process-mouse-target-changed ((buffer gtk-buffer) hit-test-result modifiers)
  (declare (ignore modifiers))
  (cond ((webkit:webkit-hit-test-result-link-uri hit-test-result)
         (push-url-at-point buffer (webkit:webkit-hit-test-result-link-uri hit-test-result)))
        ((webkit:webkit-hit-test-result-image-uri hit-test-result)
         (push-url-at-point buffer (webkit:webkit-hit-test-result-image-uri hit-test-result)))
        ((webkit:webkit-hit-test-result-media-uri hit-test-result)
         (push-url-at-point buffer (webkit:webkit-hit-test-result-media-uri hit-test-result)))))

@export
(defmethod ipc-window-make ((browser gtk-browser))
  "Make a window."
  (make-instance 'gtk-window))

@export
(defmethod ipc-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (gtk:gtk-window-present (gtk-object window)))

@export
(defmethod ipc-window-set-title ((window gtk-window) title)
  "Set the title for a window."
  (setf (gtk:gtk-window-title (gtk-object window)) title))

@export
(defmethod ipc-window-delete ((window gtk-window))
  "Delete a window object and remove it from the hash of windows."
  (gtk:gtk-widget-destroy (gtk-object window)))

@export
(defmethod ipc-window-active ((browser gtk-browser))
  "Return the window object for the currently active window."
  (loop for window being the hash-values of (windows browser)
        when (gtk:gtk-window-is-active (gtk-object window))
          do (setf (last-active-window browser) window))
  (last-active-window browser))

@export
(defmethod ipc-window-set-active-buffer ((window gtk-window) (buffer gtk-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER. "
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (gtk:gtk-box-pack-start (box-layout window) (gtk-object buffer) :expand t)
  (gtk:gtk-widget-show (gtk-object buffer))
  buffer)

@export
(defmethod ipc-window-set-minibuffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (minibuffer-container window))
        (list -1 height)))

@export
(defmethod ipc-buffer-make ((browser gtk-browser) &key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
   Run `*browser*'s `buffer-make-hook' over the created buffer before returning it.
   If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (declare (ignore dead-buffer)) ;; TODO: Dead Buffer
  (let* ((buffer (apply #'make-instance *buffer-class*
                        (append (when title `(:title ,title))
                                (when default-modes `(:default-modes ,default-modes))))))
    (unless (str:emptyp (namestring (cookies-path buffer)))
      (ensure-parent-exists (cookies-path buffer)))
    (setf (gethash (id buffer) (buffers browser)) buffer)
    (unless (last-active-buffer browser)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer browser) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (next-hooks:run-hook (buffer-make-hook browser) buffer)
    buffer))

@export
(defmethod ipc-buffer-delete ((buffer gtk-buffer))
  "Delete BUFFER from `*browser*'.
   Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (next-hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *browser*))))
        (replacement-buffer (or (%get-inactive-buffer)
                                (ipc-buffer-make *browser*))))
    (when parent-window
      (window-set-active-buffer parent-window replacement-buffer))
    (gtk:gtk-widget-destroy (gtk-object buffer))
    (remhash (id buffer) (buffers *browser*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *browser*)
      ((guard f f)
       (when *use-session*
         (funcall f))))))

@export
(defmethod ipc-buffer-load ((buffer gtk-buffer) uri)
  (webkit:webkit-web-view-load-uri (gtk-object buffer) uri))

@export
(defmethod ipc-buffer-evaluate-javascript ((buffer gtk-buffer) javascript &key callback)
  (webkit2:webkit-web-view-evaluate-javascript (gtk-object buffer) javascript callback))

@export
(defmethod ipc-minibuffer-evaluate-javascript ((window gtk-window) javascript &key callback)
  (webkit2:webkit-web-view-evaluate-javascript (minibuffer-view window) javascript callback))

@export
(defmethod ipc-buffer-enable-javascript ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-javascript
         (webkit:webkit-web-view-get-settings (gtk-object buffer)))
        value))

@export
(defmethod ipc-buffer-set-proxy ((buffer gtk-buffer) &optional proxy-uri (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
   Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
   For the user-level interface, see `proxy-mode'.

   Note: WebKit supports three proxy 'modes': default (the system proxy),
   custom (the specified proxy) and none."
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (settings (cffi:null-pointer))
         (mode :webkit-network-proxy-mode-no-proxy)
         (ignore-hosts (cffi:foreign-alloc :string
                                           :initial-contents ignore-hosts
                                           :null-terminated-p t)))
    (when proxy-uri
      (setf mode :webkit-network-proxy-mode-custom)
      (setf settings
            (webkit:webkit-network-proxy-settings-new
             proxy-uri
             ignore-hosts)))
    (cffi:foreign-free ignore-hosts)
    (webkit:webkit-web-context-set-network-proxy-settings
     context
     :webkit-network-proxy-mode-custom settings)))
