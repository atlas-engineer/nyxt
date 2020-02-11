;;; interprocess-communication.lisp --- IPC for creating interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

(defclass gtk-interface (interface) ())

(defmethod initialize ((interface gtk-interface))
  (log:debug "Initializing GTK Interface")
  (gtk:gtk-main))

(defclass gtk-window (window)
  ((gtk-object :accessor gtk-object)
   (box :accessor box)))

(defmethod initialize-instance :after ((window gtk-window) &key)
  (with-slots (gtk-object id) window
    (setf id (get-unique-window-identifier *interface*))
    (setf gtk-object (make-instance 'gtk:gtk-window
                                    :type :toplevel
                                    :default-width 1024
                                    :default-height 768))
    (gtk:gtk-widget-show-all gtk-object)))

(defclass gtk-buffer (buffer)
  ((gtk-object :accessor gtk-object)))

(defmethod initialize-instance :after ((buffer gtk-buffer) &key)
  (next-hooks:run-hook (buffer-before-make-hook *interface*) buffer)
  (setf (id buffer) (get-unique-buffer-identifier *interface*))
  (setf (gtk-object buffer) (make-instance 'cl-webkit2:webkit-web-view))
  ;; Modes might require that buffer exists, so we need to initialize them
  ;; after it has been created on the platform port.
  (initialize-modes buffer))

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
  ;; GTK CODE
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
(defmethod ipc-window-set-active-buffer ((window gtk-window) (buffer buffer))
  "Set INTERFACE's WINDOW buffer to BUFFER.
   Run WINDOW's `window-set-active-buffer-hook' over WINDOW and BUFFER before
   proceeding."
  (next-hooks:run-hook (window-set-active-buffer-hook window) window buffer)
  ; GTK CODE
  ; (%rpc-send "window_set_active_buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer)
  (setf (last-active-buffer *interface*) buffer)
  buffer)

@export
(defmethod ipc-window-set-minibuffer-height ((window gtk-window) height)
  ; GTK CODE
  ; (%rpc-send "window_set_minibuffer_height" (id window) height)
  )

@export
(defmethod ipc-buffer-make ((interface gtk-interface) &key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
   Run `*interface*'s `buffer-make-hook' over the created buffer before returning it.
   If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (declare (ignore dead-buffer)) ;; TODO: Dead Buffer
  (let* ((buffer (apply #'make-instance 'gtk-buffer
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
(defun rpc-buffer-delete (buffer)
  "Delete BUFFER from `*interface*'.
   Run BUFFER's `buffer-delete-hook' over BUFFER before deleting it."
  (next-hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer)
                                (ipc-buffer-make *interface*))))
    ; (%rpc-send "buffer_delete" (id buffer))
    (when parent-window
      (window-set-active-buffer parent-window replacement-buffer))
    (remhash (id buffer) (buffers *interface*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *interface*)
      ((guard f f)
       (when *use-session*
         (funcall f))))))

@export
(defun rpc-buffer-load (buffer uri)
  ;(%rpc-send "buffer_load" (id buffer) uri)
  )

@export
(defun rpc-buffer-evaluate-javascript (buffer javascript &key callback)
  (let ((callback-id 0
          ; (%rpc-send "buffer_evaluate_javascript" (id buffer) javascript)
          ))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

@export
(defun rpc-minibuffer-evaluate-javascript (window javascript &key callback)
  (let ((callback-id 0
          ;(%rpc-send "minibuffer_evaluate_javascript" (id window) javascript)
          ))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

@export
(defun rpc-set-proxy (buffer &optional (proxy-uri "") (ignore-hosts (list nil)))
  "Redirect network connections of BUFFER to proxy server PROXY-URI.
   Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
   For the user-level interface, see `proxy-mode'.

   Note: WebKit supports three proxy \"modes\": default (the system proxy),
   custom (the specified proxy) and none.
   TODO: We don't use \"none\" here, but it could be useful to expose it to the
   user."
  ;; (%rpc-send "set_proxy" (list (id buffer))
  ;;            (if (string= proxy-uri "")
  ;;                "default"
  ;;                "custom")
  ;;            proxy-uri ignore-hosts)
  )

@export
(defun rpc-get-proxy (buffer)
  "Return (MODE ADDRESS WHITELISTED-ADDRESSES...) of the active proxy
   configuration.
   MODE is one of \"default\" (use system configuration), \"custom\"
   or \"none\".
   ADDRESS is in the form PROTOCOL://HOST:PORT."
  ;(%rpc-send "get_proxy" (id buffer))
  )

@export
(defun rpc-buffer-set (buffer setting value)
  "Set SETTING to VALUE for BUFFER.
   Specification:
   https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html."
  ;(%rpc-send "buffer_set" (id buffer) setting value)
  )
