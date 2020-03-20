;;; renderer-gtk.lisp --- functions for creating GTK interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass gtk-browser (browser)
  ((modifiers :accessor modifiers :initform ())
   (modifier-translator :accessor modifier-translator
                        :initform #'translate-modifiers
                        :type function
                        :documentation "Function that returns a list of
modifiers understood by `keymap:make-key'.  You can customize this slot if you
want to change the behaviour of modifiers, for instance swap 'control' and
'meta'.")))

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
  #-darwin
  (progn
    (gtk:within-main-loop
      (gdk:gdk-set-program-class "next")
      (finalize browser urls startup-timestamp))
    (unless *keep-alive*
      (gtk:join-gtk-main)))
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
   (minibuffer-view :accessor minibuffer-view)
   (key-string-buffer :accessor key-string-buffer)))

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
  (with-slots (gtk-object box-layout minibuffer-container minibuffer-view active-buffer
               id key-string-buffer) window
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
    (setf key-string-buffer (make-instance 'gtk:gtk-entry))
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
       (process-key-press-event window event)))
    ;; REVIEW: No need to handle release events?
    (gobject:g-signal-connect
     gtk-object "destroy"
     (lambda (widget) (declare (ignore widget))
       (process-destroy window)))))

(defmethod process-destroy ((window gtk-window))
  ;; remove buffer from window to avoid corruption of buffer
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (window-delete window))

@export
(defmethod ipc-window-delete ((window gtk-window))
  "Delete a window object and remove it from the hash of windows."
  (gtk:gtk-widget-destroy (gtk-object window)))

(defmethod ipc-window-fullscreen ((window gtk-window))
  (gtk:gtk-window-fullscreen (gtk-object window)))

(defmethod ipc-window-unfullscreen ((window gtk-window))
  (gtk:gtk-window-unfullscreen (gtk-object window)))

(defun derive-key-string (character keyval-name)
  "Return string representation of a key, given the two CHARACTER and KEYVAL-NAME representations.
Return nil when key must be discarded, e.g. for modifiers."
  (let ((result
          (match keyval-name
            ((or "Alt_L" "Super_L" "Control_L" "Shift_L"
                 "Alt_R" "Super_R" "Control_R" "Shift_R"
                 "ISO_Level3_Shift")
             ;; Discard modifiers (they usually have a null character).
             nil)
            ((or "Escape" "BackSpace" "Return" "space" "Tab")
             ;; We can't use the rule "if character is #\Nul use keyval-name"
             ;; because some keys like Escape have a non-null character.
             keyval-name)
            ((guard s (str:contains? "KP_" s))
             (str:replace-all "KP_" "keypad" s))
            (_ (match character
                 (#\- "hyphen")
                 (#\Nul keyval-name)
                 (_ (string character)))))))
    (if (< 1 (length result))
        (str:replace-all "_" "" (string-downcase result))
        result)))

(declaim (ftype (function (list &optional gdk:gdk-event) list) translate-modifiers))
(defun translate-modifiers (modifier-state &optional event)
  "Return list of modifiers fit for `keymap:make-key'.
See `gtk-browser's `modifier-translator' slot."
  (let ((modifiers ())
        (macintosh-alt-key-value 65406)
        (key-value (match (type-of event)
                     ('gdk:gdk-event-key (gdk:gdk-event-key-keyval event))
                     (_ 0))))
    (when (member :control-mask modifier-state)
      (push "control" modifiers))
    (when (or (member :mod1-mask modifier-state)
              (= key-value macintosh-alt-key-value))
      (push "meta" modifiers))
    (when (member :shift-mask modifier-state)
      (push "shift" modifiers))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (push "super" modifiers))
    (when (member :hyper-mask modifier-state)
      (push "hyper" modifiers))
    modifiers))

(defun gdk-event-button-state (button-event) ; TODO: Fix upstream, see https://github.com/crategus/cl-cffi-gtk/issues/74.
  "Return BUTTON-EVENT modifiers as a `gdk-modifier-type', i.e. a list of keywords."
  (cffi:with-foreign-objects ((modifiers 'gdk:gdk-modifier-type))
    (setf (cffi:mem-ref modifiers 'gdk:gdk-modifier-type)
          (gdk:gdk-event-button-state button-event))
    (cffi:mem-ref modifiers 'gdk:gdk-modifier-type)))

(defmethod process-key-press-event ((sender gtk-window) event)
  (let* ((keycode (gdk:gdk-event-key-hardware-keycode event))
         (keyval (gdk:gdk-event-key-keyval event))
         (keyval-name (gdk:gdk-keyval-name keyval))
         (character (gdk:gdk-keyval-to-unicode keyval))
         (key-string (derive-key-string character keyval-name))
         (modifiers (funcall (modifier-translator *browser*)
                             (gdk:gdk-event-key-state event)
                             event)))
    ;; Generate the result of the current keypress into the dummy
    ;; key-string-buffer (a GtkEntry that's never shown on screen) so that we
    ;; can collect the printed representation of composed keypress, such as dead
    ;; keys.
    (gtk:gtk-entry-im-context-filter-keypress (key-string-buffer sender) event)
    (when (<= 1 (gtk:gtk-entry-text-length (key-string-buffer sender)))
      (match (gtk:gtk-entry-text (key-string-buffer sender))
        ;; REVIEW: Deal with these special keys in a single place?
        ((or " " "-"))
        (character (setf key-string character)))
      (setf (gtk:gtk-entry-text (key-string-buffer sender)) ""))
    (if modifiers
        (log:debug key-string keycode character keyval-name modifiers)
        (log:debug key-string keycode character keyval-name))
    (when key-string
      (alexandria:appendf (key-stack *browser*)
                          (list (keymap:make-key :code keycode
                                                 :value key-string
                                                 :modifiers modifiers
                                                 :status :pressed)))
      (funcall (input-dispatcher sender) event (active-buffer sender) sender))))

(defmethod process-button-press-event ((sender gtk-buffer) event)
  (let* ((button (gdk:gdk-event-button-button event))
         ;; REVIEW: No need to store X and Y?
         ;; (x (gdk:gdk-event-button-x event))
         ;; (y (gdk:gdk-event-button-y event))
         (window (find sender (window-list) :key #'active-buffer))
         (key-string (format nil "button~s" button)))
    (when key-string
      (alexandria:appendf (key-stack *browser*)
                          (list (keymap:make-key
                                 :value key-string
                                 :modifiers (funcall (modifier-translator *browser*)
                                                     (gdk:gdk-event-button-state event)
                                                     event)
                                 :status :pressed)))
      (funcall (input-dispatcher window) event sender window))))

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
  ;; Mouse events are captured by the web view first, so we must intercept them here.
  (gobject:g-signal-connect
   (gtk-object buffer) "button-press-event"
   (lambda (web-view event) (declare (ignore web-view))
     (process-button-press-event buffer event)))
  ;; TODO: Capture button-release-event?
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
      (setf modifiers (funcall (modifier-translator *browser*)
                               (webkit:webkit-navigation-action-get-modifiers navigation-action))))
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
  (make-instance *window-class*))

@export
(defmethod ipc-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (gtk:gtk-window-present (gtk-object window)))

@export
(defmethod ipc-window-set-title ((window gtk-window) title)
  "Set the title for a window."
  (setf (gtk:gtk-window-title (gtk-object window)) title))

@export
(defmethod ipc-window-active ((browser gtk-browser))
  "Return the window object for the currently active window."
  (setf (last-active-window browser)
        (or (find-if #'gtk:gtk-window-is-active (window-list) :key #'gtk-object)
            (last-active-window browser))))

@export
(defmethod ipc-window-set-active-buffer ((window gtk-window) (buffer gtk-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER. "
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (gtk:gtk-box-pack-start (box-layout window) (gtk-object buffer) :expand t)
  (gtk:gtk-widget-show (gtk-object buffer))
  (setf (active-buffer window) buffer)
  buffer)

@export
(defmethod ipc-window-set-minibuffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (minibuffer-container window))
        (list -1 height)))

@export
(defmethod ipc-buffer-make ((browser gtk-browser) &key title default-modes)
  "Make buffer with title TITLE and modes DEFAULT-MODES."
  (apply #'make-instance *buffer-class*
         (append (when title `(:title ,title))
                 (when default-modes `(:default-modes ,default-modes)))))

@export
(defmethod ipc-buffer-delete ((buffer gtk-buffer))
  (gtk:gtk-widget-destroy (gtk-object buffer)))

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
(defmethod ipc-buffer-enable-javascript-markup ((buffer gtk-buffer) value)
  (setf (webkit:webkit-settings-enable-javascript-markup
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

@export
(defmethod ipc-generate-input-event ((window gtk-window) event)
  ;; The "send_event" field is used to mark the event as an "unconsumed"
  ;; keypress.  The distinction allows us to avoid looping indefinitely.
  (setf (gdk:gdk-event-button-send-event event) t)
  (gtk:gtk-main-do-event event))

@export
(defmethod ipc-generated-input-event-p ((window gtk-window) event)
  (gdk:gdk-event-send-event event))
