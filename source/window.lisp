;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type window-buffer (function (window buffer))
  "Hook acting on `window' and `buffer'.")

(export-always 'renderer-window)
(defclass renderer-window ()
  ()
  (:metaclass interface-class)
  (:documentation "Renderer-specific window widget.
Should be redefined by the renderer."))

(define-class window (renderer-window)
  ((id
    (new-id)
    :type unsigned-byte
    :documentation "Unique identifier for a window.")
   (titler
    'window-default-title
    :type (or function sym:function-symbol)
    :documentation "Return the title of the window.
It's a function of the window argument that returns the title as a string.")
   (active-buffer
    :reader active-buffer
    :export nil
    :documentation "The current buffer of the window.
Not to be confused with `current-buffer'.")
   (active-prompt-buffers
    '()
    :export nil
    :documentation "The stack of current prompt buffers.")
   (panel-buffers
    (list)
    :export nil
    :documentation "A list of panel buffers appearing on the window.")
   (prompt-buffer-channel
    (make-channel)                 ; TODO: Rename `prompt-buffer-ready-channel'?
    :export nil
    :documentation "A channel one may listen to if waiting
for the prompt buffer to be available.
You should not rely on the value of this channel.
The channel is popped when a prompt buffer is hidden.")
   (fullscreen-p
    nil
    :export nil
    :type boolean
    :documentation "Whether the window is displayed in fullscreen.")
   ;; TODO: each frame should have a status buffer, not each window
   (maximized-p
    nil
    :export nil
    :type boolean
    :documentation "Whether the window is maximized.")
   (status-buffer
    (make-instance 'status-buffer)
    :type status-buffer
    :documentation "The `status-buffer' instance for this window.

To modify the status buffer appearance and behavior, subclass it and specialize
the generic functions on `status-buffer'.  Finally set the `window'
`status-buffer' slot to an instance of this subclass.")
   (message-buffer
    (make-instance 'message-buffer)
    :type message-buffer
    :documentation "The `message-buffer' instance for this window.")
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
  (:documentation "A window is a view where buffers are displayed.")
  (:metaclass user-class))

(defmethod initialize-instance :after ((window window) &key (browser *browser*)
                                       &allow-other-keys)
  (setf (window (status-buffer window)) window)
  (setf (window (message-buffer window)) window)
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
  (with-slots (status-buffer) window
    (when (and window status-buffer)
      (ffi-print-status window (format-status status-buffer)))))

(hooks:define-hook-type window (function (window))
  "Hook acting on `window's.")

(export-always 'window-make)
(defun window-make (browser)
  "Create a new window in BROWSER."
  (let ((window (ffi-window-make browser)))
    (setf (gethash (id window) (windows browser)) window)
    (unless (slot-value browser 'last-active-window)
      (setf (slot-value browser 'last-active-window) window))
    (hooks:run-hook (window-make-hook browser) window)
    window))

(export-always 'window-delete)
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
   (prompter:enable-marks-p t)
   (prompter:constructor (window-list))
   (prompter:actions-on-return (lambda-mapped-command window-delete))))

(defmethod prompter:object-attributes ((window window) (source window-source))
  (declare (ignore source))
  `(("ID" ,(id window) (:width 1))
    ("Active buffer" ,(title (active-buffer window)) (:width 3))))

(define-command delete-window ()
  "Delete the queried window(s)."
  (prompt :prompt "Delete window(s)" :sources 'window-source))

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

(define-command toggle-fullscreen (&key (window (current-window)))
  "Fullscreen WINDOW, or the current window, when omitted."
  (let ((fullscreen (fullscreen-p window)))
    (if fullscreen
        (ffi-window-unfullscreen window)
        (ffi-window-fullscreen window))))

(define-command toggle-maximize (&key (window (current-window)))
  "Maximize WINDOW, or the current window, when omitted."
  (let ((maximized (maximized-p window)))
    (if maximized
        (ffi-window-unmaximize window)
        (ffi-window-maximize window))))

(defun enable-status-buffer (&optional (window (current-window)))
  (setf (ffi-height (status-buffer window)) (height (status-buffer window))))

(defun disable-status-buffer (&optional (window (current-window)))
  (setf (ffi-height (status-buffer window)) 0))

(defun enable-message-buffer (&optional (window (current-window)))
  (setf (ffi-height (message-buffer window)) (height (message-buffer window))))

(defun disable-message-buffer (&optional (window (current-window)))
  (setf (ffi-height (message-buffer window)) 0))

(define-command toggle-toolbars (&optional (window (current-window)))
  "Toggle the visibility of the message and status buffers."
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
              (zerop (ffi-height (status-buffer window))))
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
              (zerop (height (message-buffer window))))
         (enable-message-buffer window))
        (t (disable-message-buffer window))))

(defun toggle-toolbars-on-fullscreen (&key (window (current-window)))
  "Toggle toolbar when fullscreen state is changed. Should be called
from the renderer."
  (let ((fullscreen (fullscreen-p window)))
    (toggle-message-buffer :window window :show-p (not fullscreen))
    (toggle-status-buffer  :window window :show-p (not fullscreen))))
(export 'toggle-toolbars-on-fullscreen)
