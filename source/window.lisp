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
   (buffer
    (make-instance 'buffer)
    :documentation "The buffer displayed in the window.")
   (active-prompt-buffers
    '()
    :export nil
    :documentation "The stack of current prompt buffers.")
   (keyboard-buffer
    nil
    :type (or null buffer)
    :export nil
    :documentation "The buffer whose view has keyboard focus.
`on-signal-focus' sets it from the renderer. Showing a buffer does too.")
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
   (panel-buffers
    '()
    :type list
    :documentation "Panel buffers beside the page.
Each stays as tall as the page. Its `side' is `:left' or `:right'.")
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
  (str:concat (title (buffer window)) " － Nyxt"))

(defgeneric window-shows-buffer-p (window buffer)
  (:method ((window window) buffer)
    (not (null
          (and buffer
               (or (find buffer (active-prompt-buffers window))
                   (eq buffer (buffer window))
                   (eq buffer (status-buffer window))
                   (eq buffer (message-buffer window))
                   (find buffer (panel-buffers window)))))))
  (:documentation "Return non-nil when BUFFER is a view WINDOW is showing."))

(defmethod (setf buffer) :around (value (window window))
  (declare (ignore value))
  (call-next-method)
  (set-window-title window))

(hooks:define-hook-type window (function (window))
  "Hook acting on `window's.")

(export-always 'window-make)
(defun window-make (browser)
  "Create a new window in BROWSER."
  (let ((window (make-instance 'window)))
    (setf (gethash (id window) (windows browser)) window)
    (unless (slot-value browser 'last-active-window)
      (setf (slot-value browser 'last-active-window) window))
    (hooks:run-hook (window-make-hook browser) window)
    window))

(define-class window-source (prompter:source)
  ((prompter:name "Windows")
   (prompter:enable-marks-p t)
   (prompter:constructor (window-list))
   (prompter:actions-on-return (lambda-mapped-command ffi-window-delete))))

(defmethod prompter:object-attributes ((window window) (source window-source))
  (declare (ignore source))
  `(("ID" ,(id window) (:width 1))
    ("Buffer" ,(title (buffer window)) (:width 3))))

(define-command delete-window ()
  "Delete the queried window(s)."
  (prompt :prompt "Delete window(s)" :sources 'window-source))

(define-command delete-current-window (window)
  "Delete WINDOW. Quit if it is the last one."
  (ffi-window-delete window))

(define-command make-window (&optional buffer)
  "Create a new window."
  (let ((window (window-make *browser*))
        (buffer (or buffer (make-buffer))))
    (ffi-window-set-buffer window buffer)
    (values window buffer)))

(define-command toggle-fullscreen (window)
  "Toggle fullscreen state of WINDOW."
  (if (fullscreen-p window)
      (ffi-window-unfullscreen window)
      (ffi-window-fullscreen window)))

(define-command toggle-maximize (window)
  "Toggle maximized state of WINDOW."
  (if (maximized-p window)
      (ffi-window-unmaximize window)
      (ffi-window-maximize window)))

(export-always 'enable-status-buffer)
(defun enable-status-buffer (window)
  (setf (ffi-height (status-buffer window)) (height (status-buffer window))))

(export-always 'disable-status-buffer)
(defun disable-status-buffer (window)
  (setf (ffi-height (status-buffer window)) 0))

(export-always 'enable-message-buffer)
(defun enable-message-buffer (window)
  (setf (ffi-height (message-buffer window)) (height (message-buffer window))))

(export-always 'disable-message-buffer)
(defun disable-message-buffer (window)
  (setf (ffi-height (message-buffer window)) 0))

(define-command toggle-toolbars (window)
  "Toggle the visibility of the message and status buffers in WINDOW."
  (toggle-status-buffer window)
  (toggle-message-buffer window))

(define-command toggle-status-buffer (window)
  "Toggle the visibility of the status buffer in WINDOW."
  (if (zerop (ffi-height (status-buffer window)))
      (enable-status-buffer window)
      (disable-status-buffer window)))

(define-command toggle-message-buffer (window)
  "Toggle the visibility of the message buffer in WINDOW."
  (if (zerop (ffi-height (message-buffer window)))
      (enable-message-buffer window)
      (disable-message-buffer window)))
