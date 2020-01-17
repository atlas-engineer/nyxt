;;; interprocess-communication.lisp --- IPC for creating interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

@export
(defun rpc-window-make ()
  "Create a window and return the window object.
Run INTERFACE's `window-make-hook' over the created window."
  (let* ((window-id (get-unique-window-identifier))
         (window (make-instance *window-class* :id window-id)))
    (setf (gethash window-id (windows *interface*)) window)
    (incf (total-window-count *interface*))
    (%rpc-send "window_make" window-id)
    (unless (last-active-window *interface*)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and rpc-window-active would then return nil.
      (setf (last-active-window *interface*) window))
    (next-hooks:run-hook (window-make-hook *interface*) window)
    window))

(declaim (ftype (function (window string)) rpc-window-set-title))
@export
(defun rpc-window-set-title (window title)
  "Set the title for a given window."
  (%rpc-send "window_set_title" (id window) title))

(declaim (ftype (function (window)) rpc-window-delete))
@export
(defun rpc-window-delete (window)
  "Delete a window object and remove it from the hash of windows.
Once deleted, the `window-will-close' RPC endpoint will be called, running
INTERFACE's `window-delete-hook' over WINDOW."
  (%rpc-send "window_delete" (id window)))

@export
(defun rpc-window-active ()
  "Return the window object for the currently active window."
  (let ((window (gethash (%rpc-send "window_active")
                         (windows *interface*))))
    (when window
      (setf (last-active-window *interface*) window))
    (last-active-window *interface*)))

(declaim (ftype (function (window)) rpc-window-exists))
@export
(defun rpc-window-exists (window)
  "Return if a window exists."
  (%rpc-send "window_exists" (id window)))

(declaim (ftype (function (window buffer)) rpc-window-set-active-buffer))
@export
(defun rpc-window-set-active-buffer (window buffer)
  "Set INTERFACE's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (next-hooks:run-hook (window-set-active-buffer-hook window) window buffer)
  (%rpc-send "window_set_active_buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer)
  (when (and window buffer)
    (setf (last-active-buffer *interface*) buffer))
  buffer)

(declaim (ftype (function (window integer)) rpc-window-set-minibuffer-height))
@export
(defun rpc-window-set-minibuffer-height (window height)
  (%rpc-send "window_set_minibuffer_height" (id window) height))

(declaim (ftype (function (&key (:title string) (:default-modes list) (:dead-buffer buffer))) rpc-buffer-make))
@export
(defun rpc-buffer-make (&key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
Run `*interface*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let* ((buffer (if dead-buffer
                     (progn (setf (id dead-buffer) (get-unique-buffer-identifier))
                            dead-buffer)
                     (apply #'make-instance *buffer-class* :id (get-unique-buffer-identifier)
                            (append (when title `(:title ,title))
                                    (when default-modes `(:default-modes ,default-modes)))))))
    (next-hooks:run-hook (buffer-before-make-hook *interface*) buffer)
    (unless (str:emptyp (namestring (cookies-path buffer)))
      (ensure-parent-exists (cookies-path buffer)))
    (setf (gethash (id buffer) (buffers *interface*)) buffer)
    (incf (total-buffer-count *interface*))
    (%rpc-send "buffer_make" (id buffer)
               `(("cookies-path" ,(namestring (cookies-path buffer)))))
    (unless (last-active-buffer *interface*)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer *interface*) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (next-hooks:run-hook (buffer-make-hook *interface*) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after it has been created on the platform port.
    (initialize-modes buffer)
    buffer))

(declaim (ftype (function (buffer)) rpc-buffer-delete))
@export
(defun rpc-buffer-delete (buffer)
  "Delete BUFFER from `*interface*'.
Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (next-hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer)
                                (rpc-buffer-make))))
    (%rpc-send "buffer_delete" (id buffer))
    (when parent-window
      (window-set-active-buffer parent-window replacement-buffer))
    (remhash (id buffer) (buffers *interface*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *interface*)
      ((guard f f)
       (when *use-session*
         (funcall f))))))

(declaim (ftype (function (buffer string)) rpc-buffer-load))
@export
(defun rpc-buffer-load (buffer uri)
  (%rpc-send "buffer_load" (id buffer) uri))

(declaim (ftype (function (buffer string &key (:callback function))) rpc-buffer-evaluate-javascript))
@export
(defun rpc-buffer-evaluate-javascript (buffer javascript &key callback)
  (let ((callback-id
          (%rpc-send "buffer_evaluate_javascript" (id buffer) javascript)))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

(declaim (ftype (function (window string &key (:callback function))) rpc-minibuffer-evaluate-javascript))
@export
(defun rpc-minibuffer-evaluate-javascript (window javascript &key callback)
  ;; JS example: document.body.innerHTML = 'hello'
  (let ((callback-id
          (%rpc-send "minibuffer_evaluate_javascript" (id window) javascript)))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

(declaim (ftype (function (window key-chord)) rpc-generate-input-event))
@export
(defun rpc-generate-input-event (window event)
  "For now, we only generate keyboard events.
In the future, we could also support other input device events such as mouse
events."
  (log:debug "Generate input ~a for window ~a"
             (list
              (key-chord-key-code event)
              (key-chord-modifiers event)
              (key-chord-low-level-data event)
              (key-chord-position event))
             (id window))
  (%rpc-send "generate_input_event"
             (id window)
             (key-chord-key-code event)
             (or (key-chord-modifiers event) (list ""))
             (key-chord-low-level-data event)
             (float (or (first (key-chord-position event)) -1.0))
             (float (or (second (key-chord-position event)) -1.0))))

(declaim (ftype (function (buffer &optional string list)) rpc-set-proxy))
@export
(defun rpc-set-proxy (buffer &optional (proxy-uri "") (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
For the user-level interface, see `proxy-mode'.

Note: WebKit supports three proxy \"modes\": default (the system proxy),
custom (the specified proxy) and none.
TODO: We don't use \"none\" here, but it could be useful to expose it to the
user."
  (%rpc-send "set_proxy" (list (id buffer))
             (if (string= proxy-uri "")
                 "default"
                 "custom")
             proxy-uri ignore-hosts))

(declaim (ftype (function (buffer)) rpc-get-proxy))
@export
(defun rpc-get-proxy (buffer)
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy configuration.
MODE is one of \"default\" (use system configuration), \"custom\" or \"none\".
ADDRESS is in the form PROTOCOL://HOST:PORT."
  (%rpc-send "get_proxy" (id buffer)))

(declaim (ftype (function (buffer string boolean)) rpc-buffer-set))
@export
(defun rpc-buffer-set (buffer setting value)
  "Set SETTING to VALUE for BUFFER.
The valid SETTINGs are specified by the platform, e.g. for WebKitGTK it is
https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html.

TODO: Only booleans are supported for now."
  (%rpc-send "buffer_set" (id buffer) setting value))
