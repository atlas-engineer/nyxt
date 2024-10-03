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
   (active-buffer
    (make-instance 'buffer)
    :reader active-buffer
    :export nil
    :documentation "The current buffer of the window.
Not to be confused with `current-buffer' or `focused-buffer'.")
   (active-prompt-buffers
    '()
    :export nil
    :documentation "The stack of current prompt buffers.")
   (prompt-buffer-ready-channel
    (make-channel)
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
    :documentation "Hook run before `ffi-window-set-buffer' takes effect.
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
  (print-unreadable-object (window stream :type t)
    (format stream "~a ~a" (id window) (titler window))))

(defmethod titler ((window window))
  "Return the title of WINDOW."
  (str:concat (title (active-buffer window)) " － Nyxt"))

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
    (ffi-window-set-buffer window buffer)
    (values window buffer)))

(define-command toggle-fullscreen (&optional (window (current-window)))
  "Toggle fullscreen state of window."
  (if (fullscreen-p window)
      (ffi-window-unfullscreen window)
      (ffi-window-fullscreen window)))

(define-command toggle-maximize (&optional (window (current-window)))
  "Toggle maximized state of window."
  (if (maximized-p window)
      (ffi-window-unmaximize window)
      (ffi-window-maximize window)))

(export-always 'enable-status-buffer)
(defun enable-status-buffer (&optional (window (current-window)))
  (setf (ffi-height (status-buffer window)) (height (status-buffer window))))

(export-always 'disable-status-buffer)
(defun disable-status-buffer (&optional (window (current-window)))
  (setf (ffi-height (status-buffer window)) 0))

(export-always 'enable-message-buffer)
(defun enable-message-buffer (&optional (window (current-window)))
  (setf (ffi-height (message-buffer window)) (height (message-buffer window))))

(export-always 'disable-message-buffer)
(defun disable-message-buffer (&optional (window (current-window)))
  (setf (ffi-height (message-buffer window)) 0))

(define-command toggle-toolbars (&optional (window (current-window)))
  "Toggle the visibility of the message and status buffers."
  (toggle-status-buffer window)
  (toggle-message-buffer window))

(define-command toggle-status-buffer (&optional (window (current-window)))
  "Toggle the visibility of the status buffer."
  (if (zerop (ffi-height (status-buffer window)))
      (enable-status-buffer window)
      (disable-status-buffer window)))

(define-command toggle-message-buffer (&optional (window (current-window)))
  "Toggle the visibility of the message buffer."
  (if (zerop (ffi-height (message-buffer window)))
      (enable-message-buffer window)
      (disable-message-buffer window)))
