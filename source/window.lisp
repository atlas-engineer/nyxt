;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type window-buffer (function (window buffer)))

(export-always 'renderer-window)
(defclass renderer-window ()
  ()
  (:metaclass interface-class))

(define-class window (renderer-window)
  ((id
    (new-id)
    :type unsigned-byte
    :documentation "Unique identifier for a window.")
   (titler
    'window-default-title
    :type (or function function-symbol)
    :documentation "Return the title of the window.
It's a function of the window argument that returns the title as a string.")
   (active-buffer :accessor nil :reader active-buffer :export nil)
   (active-prompt-buffers
    '()
    :export nil
    :documentation "The stack of current prompt buffers.")
   (panel-buffers
    (list)
    :export nil
    :documentation "A list of panel buffers appearing on the window.")
   (prompt-buffer-channel
    (make-channel) ; TODO: Rename `prompt-buffer-ready-channel'?
    :export nil
    :documentation "A channel one may listen to if waiting
for the prompt buffer to be available.
You should not rely on the value of this channel.
The channel is popped when a prompt buffer is hidden.")
   (key-stack
    '()
    :documentation "A stack of the key chords a user has pressed.")
   (last-key
    nil
    :export nil
    :type (or null keymap:key)
    :documentation "Last pressed key.  Useful for `self-insert'.")
   (fullscreen-p
    nil
    :export nil
    :type boolean
    :documentation "Whether the window is displayed in fullscreen.")
   ;; TODO: each frame should have a status buffer, not each window
   (status-buffer
    (make-instance 'status-buffer)
    :type status-buffer
    :documentation "The `status-buffer' instance for this window.

To modify the status buffer appearance and behaviour, subclass it and specialize
the generic functions on `status-buffer'.  Finally set the `window'
`status-buffer' slot to an instance of this subclass.")
   (message-buffer-height
    16
    :documentation "The height of the message buffer in pixels.")
   (message-buffer-style
    (theme:themed-css (theme *browser*)
      (body
       :background-color theme:background
       :color theme:on-background
       :font-size "12px"
       :padding 0
       :padding-left "4px"
       :margin 0)))
   (prompt-buffer-open-height
    256
    :documentation "The height of the prompt buffer when open.")
   (input-dispatcher
    'dispatch-input-event
    :type (or function-symbol function)
    :documentation "Function to process input events.
It takes EVENT, BUFFER, WINDOW and PRINTABLE-P parameters.
Cannot be null.")
   (command-dispatcher
    #'dispatch-command
    :type function
    :documentation "Function to process the command processed in `input-dispatcher'.
Takes the function/command as the only argument.")
   (input-skip-dispatcher
    #'dispatch-input-skip
    :type function
    :documentation "Function to process the skipped input event.
It runs when the pressed keybinding has no associated command.
The only argument is a string representation of the pressed key.")
   (window-set-buffer-hook
    (make-instance 'hook-window-buffer)
    :type hook-window-buffer
    :documentation "Hook run before `window-set-buffer' takes effect.
The handlers take the window and the buffer as argument.")
   (window-delete-hook
    (make-instance 'hook-window)
    :type hook-window
    :documentation "Hook run after `ffi-window-delete' takes effect.
The handlers take the window as argument."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A window is a view where buffers are displayed.")
  (:metaclass user-class))

(defmethod initialize-instance :after ((window window) &key (browser *browser*)
                                       &allow-other-keys)
  "Initialize some required slots like ID and status-buffer."
  (setf (window (status-buffer window)) window)
  (when browser
    (setf (id window) (new-id))
    (setf (slot-value browser 'last-active-window) window))
  window)

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type t :identity t)
    (format stream "~a" (id window))))

(defmethod window-add-panel-buffer ((window window) (buffer panel-buffer) side)
  "Add a panel buffer to a window. Side can either be :right or :left."
  (pushnew buffer (panel-buffers window))
  (ffi-window-add-panel-buffer window buffer side)
  buffer)

(defmethod window-delete-panel-buffer ((window window) (buffer panel-buffer))
  "Remove a panel buffer from a window."
  (setf (panel-buffers window)
        (remove buffer (panel-buffers window)))
  (ffi-window-delete-panel-buffer window buffer))

(defmethod (setf active-buffer) (buffer (window window))
  (setf (slot-value window 'active-buffer) buffer))

(defun print-status (&optional (window (current-window)))
  (when (and window (status-buffer window))
    (ffi-print-status
     window
     (format-status (status-buffer window)))))

(hooks:define-hook-type window (function (window)))

(export-always 'window-make)
(defun window-make (browser)
  (let* ((window (ffi-window-make browser)))
    (setf (gethash (id window) (windows browser)) window)
    (unless (slot-value browser 'last-active-window)
      (setf (slot-value browser 'last-active-window) window))
    (hooks:run-hook (window-make-hook browser) window)
    window))

(-> window-delete (window &key (:force-p boolean)) *)
(defun window-delete (window &key force-p)
  "Remove WINDOW from list of known windows.
Quit the browser if it's the last window.

This function must be called by the renderer when a window is deleted.

With FORCE-P, only destroy the WINDOW widget and remove from known windows; do
not try to quit the browser."
  (when (gethash (id window) (windows *browser*)) ;; To avoid nested `window-delete' calls.
    (unless force-p
      (hooks:run-hook (window-delete-hook window) window))
    ;; Remove window from list after the hook, so that the window remains
    ;; enlisted on hook error.
    (remhash (id window) (windows *browser*))
    (ffi-window-delete window)
    (when (and (not force-p)
               (zerop (hash-table-count (windows *browser*))))
      (quit))))

(define-class window-source (prompter:source)
  ((prompter:name "Windows")
   (prompter:multi-selection-p t)
   (prompter:constructor (window-list))
   (prompter:return-actions (list (lambda-mapped-command window-delete)))))

(defmethod prompter:object-attributes ((window window) (source window-source))
  (declare (ignore source))
  `(("ID" ,(id window))
    ("Active buffer" ,(title (active-buffer window)))))

(define-command delete-window ()
  "Delete the queried window(s)."
  (prompt
   :prompt "Delete window(s)"
   :sources (make-instance 'window-source)))

(define-command delete-current-window (&optional (window (current-window)))
  "Delete WINDOW, or the current window, when omitted."
  (let ((window-count (hash-table-count (windows *browser*))))
    (cond ((and window (> window-count 1))
           (ffi-window-delete window))
          (window
           (echo "Can't delete sole window.")))))

(define-command make-window (&optional buffer)
  "Create a new window."
  (let ((window (window-make *browser*))
        (buffer (or buffer (make-buffer))))
    (window-set-buffer window buffer)
    (values window buffer)))

(define-command toggle-fullscreen (&key (window (current-window))
                                   skip-renderer-resize)
  "Fullscreen WINDOW, or the current window, when omitted.
When `skip-renderer-resize' is non-nil, don't ask the renderer to "
  (let ((fullscreen (fullscreen-p window)))
    (unless skip-renderer-resize
      (if fullscreen
          (ffi-window-unfullscreen window)
          (ffi-window-fullscreen window)))
    (toggle-status-buffer :show-p (not fullscreen))
    (toggle-message-buffer :show-p (not fullscreen))))

(defun enable-status-buffer (&optional (window (current-window)))
  (setf (ffi-window-status-buffer-height window) (height (status-buffer window))))

(defun disable-status-buffer (&optional (window (current-window)))
  (setf (ffi-window-status-buffer-height window) 0))

(defun enable-message-buffer (&optional (window (current-window)))
  (setf (ffi-window-message-buffer-height window) (message-buffer-height window)))

(defun disable-message-buffer (&optional (window (current-window)))
  (setf (ffi-window-message-buffer-height window) 0))

(define-command toggle-toolbars (&optional (window (current-window)))
  "Toggle the visibility of the message and status buffer areas."
  (toggle-status-buffer :window window)
  (toggle-message-buffer :window window))

(define-command toggle-status-buffer (&key (window (current-window))
                                      (show-p nil show-provided-p))
  "Toggle the visibility of the status buffer.

If SHOW-P is provided:
- If SHOW-P is T, then `status-buffer' is always enabled;
- Otherwise, it is always disabled."
  (cond ((and show-provided-p show-p)
         (enable-status-buffer window))
        ((and (not show-provided-p)
              (zerop (ffi-window-status-buffer-height window)))
         (enable-status-buffer window))
        (t (disable-status-buffer window))))

(define-command toggle-message-buffer (&key (window (current-window))
                                       (show-p nil show-provided-p))
  "Toggle the visibility of the message buffer.

If SHOW-P is provided:
- If SHOW-P is T, then `message-buffer' is always enabled;
- Otherwise, it is always disabled."
  (cond ((and show-provided-p show-p)
         (enable-message-buffer window))
        ((and (not show-provided-p)
              (zerop (ffi-window-message-buffer-height window)))
         (enable-message-buffer window))
        (t (disable-message-buffer window))))
