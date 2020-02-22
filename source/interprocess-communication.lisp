;;; interprocess-communication.lisp --- IPC for creating interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass gtk-interface (interface)
  ((modifiers :accessor modifiers :initform ())))

(define-class-type interface)
(declaim (type (interface-type) *interface-class*))
@export
(defparameter *interface-class* 'gtk-interface)

(defmethod initialize ((interface gtk-interface))
  (log:debug "Initializing GTK Interface")
  (gtk:gtk-main))

(defmethod kill-interface ((interface gtk-interface))
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
    (setf id (get-unique-window-identifier *interface*))
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
    (setf minibuffer-view (make-instance 'cl-webkit2:webkit-web-view))
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
       (push-modifier *interface* event)
       (process-key-press-event window event)))
    (gobject:g-signal-connect
     gtk-object "key_release_event"
     (lambda (widget event) (declare (ignore widget))
       (pop-modifier *interface* event)))
    (gobject:g-signal-connect
     gtk-object "destroy"
     (lambda (widget) (declare (ignore widget))
       (process-destroy window)))))

(defmethod process-destroy ((window gtk-window))
  ;; remove buffer from window to avoid corruption of buffer
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (next-hooks:run-hook (window-delete-hook window) window)
  (remhash (id window) (windows *interface*))
  (when (zerop (hash-table-count (windows *interface*)))
    (kill-interface *interface*)))

(defmethod window-destroy ((window gtk-window))
  (gtk:gtk-widget-destroy (gtk-object window)))

(defun character->string (character &optional key-value)
  (cond ((eq character #\Return) "RETURN")
        ((eq character #\Backspace) "BACKSPACE")
        ((eq character #\Esc) "ESCAPE")
        ((eq character #\-) "HYPHEN")
        ((eq character #\Space) "SPACE")
        ((eq character #\Tab) "TAB")
        ((eq key-value 65361) "Left")
        ((eq key-value 65362) "Up")
        ((eq key-value 65364) "Down")
        ((eq key-value 65363) "Right")
        ((not (eq character #\Nul)) (string character))))

(defmethod push-modifier ((interface gtk-interface) event)
  (let ((modifier-state (gdk:gdk-event-key-state event))
        (key-value (gdk:gdk-event-key-keyval event)))
    (when (member :control-mask modifier-state)
      (push "C" (modifiers interface)))
    (when (or (member :mod1-mask modifier-state)
              (eq key-value 65406))
      (push "M" (modifiers interface)))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (push "S" (modifiers interface))))
  (setf (modifiers interface) (remove-duplicates (modifiers interface) :test #'equal)))

(defmethod pop-modifier ((interface gtk-interface) event)
  (let ((modifier-state (gdk:gdk-event-key-state event))
        (key-value (gdk:gdk-event-key-keyval event)))
    (when (member :control-mask modifier-state)
      (setf (modifiers interface) (remove "C" (modifiers interface) :test #'equal)))
    (when (or (member :mod1-mask modifier-state)
              (eq key-value 65406))
      (setf (modifiers interface) (remove "M" (modifiers interface) :test #'equal)))
    (when (or (member :super-mask modifier-state)
              (and (member :mod2-mask modifier-state)
                   (member :meta-mask modifier-state)))
      (setf (modifiers interface) (remove "S" (modifiers interface) :test #'equal)))))

(defmethod process-key-press-event ((sender gtk-window) event)
  (let* ((character (gdk:gdk-keyval-to-unicode (gdk:gdk-event-key-keyval event)))
         (character-code (char-code character))
         (key-value (gdk:gdk-event-key-keyval event))
         (key-string (character->string character key-value)))
    (when key-string
      (push-input-event character-code key-string (modifiers *interface*) -1 -1 nil sender))))

(defmethod initialize-instance :after ((buffer gtk-buffer) &key)
  (next-hooks:run-hook (buffer-before-make-hook *interface*) buffer)
  (setf (id buffer) (get-unique-buffer-identifier *interface*))
  (setf (gtk-object buffer) (make-instance 'cl-webkit2:webkit-web-view))

  ;; TODO: Why doesn't cookie storage work?
  (when (stringp (cookies-path buffer))
    (let* ((context (cl-webkit2:webkit-web-view-web-context (gtk-object buffer)))
           (cookie-manager (cl-webkit2:webkit-web-context-get-cookie-manager context)))
      (cl-webkit2:webkit-cookie-manager-set-persistent-storage
       cookie-manager
       (cookies-path buffer)
       :webkit-cookie-persistent-storage-text)))

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
  (declare (ignore buffer)))

(defmethod process-load-changed ((buffer gtk-buffer) load-event)
  (let ((url (cl-webkit2:webkit-web-view-uri (gtk-object buffer))))
    (cond ((eq load-event :webkit-load-started) nil)
          ((eq load-event :webkit-load-redirected) nil)
          ((eq load-event :webkit-load-committed)
           (did-commit-navigation buffer url))
          ((eq load-event :webkit-load-finished)
           (did-finish-navigation buffer url)))))

(defmethod process-mouse-target-changed ((buffer gtk-buffer) hit-test-result modifiers)
  (declare (ignore modifiers))
  (cond ((cl-webkit2:webkit-hit-test-result-link-uri hit-test-result)
         (push-url-at-point buffer (cl-webkit2:webkit-hit-test-result-link-uri hit-test-result)))
        ((cl-webkit2:webkit-hit-test-result-image-uri hit-test-result)
         (push-url-at-point buffer (cl-webkit2:webkit-hit-test-result-image-uri hit-test-result)))
        ((cl-webkit2:webkit-hit-test-result-media-uri hit-test-result)
         (push-url-at-point buffer (cl-webkit2:webkit-hit-test-result-media-uri hit-test-result)))))

@export
(defmethod ipc-window-make ((interface gtk-interface))
  "Make a window."
  (let* ((window (make-instance 'gtk-window)))
    (setf (gethash (id window) (windows interface)) window)
    (unless (last-active-window interface)
      (setf (last-active-window interface) window))
    (next-hooks:run-hook (window-make-hook interface) window)
    window))

@export
(defmethod ipc-window-set-title ((window gtk-window) title)
  "Set the title for a window."
  (setf (gtk:gtk-window-title (gtk-object window)) title))

@export
(defmethod ipc-window-delete ((window gtk-window))
  "Delete a window object and remove it from the hash of windows."
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (gtk:gtk-widget-destroy (gtk-object window))
  (next-hooks:run-hook (window-delete-hook window) window)
  (remhash (id window) (windows *interface*)))

@export
(defmethod ipc-window-active ((interface gtk-interface))
  "Return the window object for the currently active window."
  (loop for window being the hash-values of (windows interface)
        when (gtk:gtk-window-is-active (gtk-object window))
          do (setf (last-active-window interface) window))
  (last-active-window interface))

@export
(defmethod ipc-window-set-active-buffer ((window gtk-window) (buffer gtk-buffer))
  "Set INTERFACE's WINDOW buffer to BUFFER.
   Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
   proceeding."
  (next-hooks:run-hook (window-set-active-buffer-hook window) window buffer)
  (gtk:gtk-container-remove (box-layout window) (gtk-object (active-buffer window)))
  (gtk:gtk-box-pack-start (box-layout window) (gtk-object buffer) :expand t)
  (gtk:gtk-widget-show (gtk-object buffer))
  (setf (active-buffer window) buffer)
  (setf (last-active-buffer *interface*) buffer)
  buffer)

@export
(defmethod ipc-window-set-minibuffer-height ((window gtk-window) height)
  (setf (gtk:gtk-widget-size-request (minibuffer-container window))
        (list -1 height)))

@export
(defmethod ipc-buffer-make ((interface gtk-interface) &key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
   Run `*interface*'s `buffer-make-hook' over the created buffer before returning it.
   If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (declare (ignore dead-buffer)) ;; TODO: Dead Buffer
  (let* ((buffer (apply #'make-instance *buffer-class*
                        (append (when title `(:title ,title))
                                (when default-modes `(:default-modes ,default-modes))))))
    (unless (str:emptyp (namestring (cookies-path buffer)))
      (ensure-parent-exists (cookies-path buffer)))
    (setf (gethash (id buffer) (buffers interface)) buffer)
    (unless (last-active-buffer interface)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer interface) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (next-hooks:run-hook (buffer-make-hook interface) buffer)
    buffer))

@export
(defmethod ipc-buffer-delete ((buffer gtk-buffer))
  "Delete BUFFER from `*interface*'.
   Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (next-hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer)
                                (ipc-buffer-make *interface*))))
    (when parent-window
      (window-set-active-buffer parent-window replacement-buffer))
    (gtk:gtk-widget-destroy (gtk-object buffer))
    (remhash (id buffer) (buffers *interface*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *interface*)
      ((guard f f)
       (when *use-session*
         (funcall f))))))

@export
(defmethod ipc-buffer-load ((buffer gtk-buffer) uri)
  (cl-webkit2:webkit-web-view-load-uri (gtk-object buffer) uri))

@export
(defmethod ipc-buffer-evaluate-javascript ((buffer gtk-buffer) javascript &key callback)
  (webkit2:webkit-web-view-evaluate-javascript (gtk-object buffer) javascript callback))

@export
(defmethod ipc-minibuffer-evaluate-javascript ((window gtk-window) javascript &key callback)
  (webkit2:webkit-web-view-evaluate-javascript (minibuffer-view window) javascript callback))

@export
(defmethod ipc-buffer-enable-javascript ((buffer gtk-buffer) value)
  (setf (cl-webkit2:webkit-settings-enable-javascript
         (cl-webkit2:webkit-web-view-get-settings (gtk-object buffer)))
        value))

@export
(defmethod ipc-buffer-set-proxy ((buffer gtk-buffer) &optional (proxy-uri "") (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
   Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
   For the user-level interface, see `proxy-mode'.

   Note: WebKit supports three proxy 'modes': default (the system proxy),
   custom (the specified proxy) and none."
  ;; TODO: Implement support in cl-webkit
  )

@export
(defmethod ipc-buffer-get-proxy ((buffer gtk-buffer))
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy
   configuration.
   MODE is one of 'default' (use system configuration), 'custom'
   or 'none'.
   ADDRESS is in the form PROTOCOL://HOST:PORT."
  ;; TODO: Implement support in cl-webkit
  )
