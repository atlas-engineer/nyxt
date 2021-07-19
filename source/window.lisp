;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type window-buffer (function (window buffer)))

(define-class window ()
  ((id "")
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
   (status-buffer
    :export nil)
   (message-buffer-height
    16
    :documentation "The height of the message buffer in pixels.")
   (message-buffer-style
    (cl-css:css
     '((body
        :font-size "12px"
        :padding 0
        :padding-left "4px"
        :margin 0))))
   (prompt-buffer-open-height
    256
    :documentation "The height of the prompt buffer when open.")
   (input-dispatcher
    #'dispatch-input-event
    :documentation "Function to process input events.
It takes EVENT, BUFFER, WINDOW and PRINTABLE-P parameters.
Cannot be null.")
   (window-set-buffer-hook
    (make-hook-window-buffer)
    :type hook-window-buffer
    :documentation "Hook run before `window-set-buffer' takes effect.
The handlers take the window and the buffer as argument.")
   (status-formatter
    #'format-status
    :type (function (window) string)
    :documentation "Function of a window argument that returns
a string to be printed in the status view.
Cannot be null.")
   (window-delete-hook (make-hook-window)
                       :type hook-window
                       :documentation "Hook run after `ffi-window-delete' takes effect.
The handlers take the window as argument."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A window is a view where buffers are displayed."))

(define-user-class window)

(defmethod window-add-panel-buffer ((window window) (buffer panel-buffer) side)
  "Add a panel buffer to a window. Side can either be :right or :left."
  (pushnew buffer (panel-buffers window))
  (ffi-window-add-panel-buffer window buffer side))

(defmethod window-delete-panel-buffer ((window window) (buffer panel-buffer))
  "Remove a panel buffer from a window."
  (setf (panel-buffers window)
        (remove buffer (panel-buffers window)))
  (ffi-window-delete-panel-buffer window buffer))

(define-class panel-buffer-source (prompter:source)
  ((prompter:name "Panel buffers")
   (window :accessor window :initarg :window)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (panel-buffers (window source))))))

(define-command-global delete-panel-buffer (&key (window (current-window))
                                            panels)
  "Prompt the user to delete a panel buffer."
  (let ((panels (or panels
                    (prompt
                     :prompt "Delete a panel buffer:"
                     :sources (make-instance 'panel-buffer-source
                                             :window window)))))
    (mapc (lambda (i) (window-delete-panel-buffer window i)) panels)))

(defmethod (setf active-buffer) (buffer (window window))
  (setf (slot-value window 'active-buffer) buffer)
  (print-status))

(defun print-status (&optional status window)
  (let ((window (or window (current-window))))
    (when (and window (status-buffer window))
      (ffi-print-status
       window
       (or status
           (funcall (status-formatter window) window))))))

(hooks:define-hook-type window (function (window)))

(declaim (ftype (function (browser)) window-make))
(export-always 'window-make)
(defun window-make (browser)
  (let* ((window (ffi-window-make browser)))
    (setf (gethash (id window) (windows browser)) window)
    (unless (slot-value browser 'last-active-window)
      (setf (slot-value browser 'last-active-window) window))
    (hooks:run-hook (window-make-hook browser) window)
    window))

(declaim (ftype (function (window)) window-delete))
(defun window-delete (window)
  "This function must be called by the renderer when a window is deleted."
  (ffi-window-delete window)
  (hooks:run-hook (window-delete-hook window) window)
  (remhash (id window) (windows *browser*))
  (when (zerop (hash-table-count (windows *browser*)))
    (quit)))

(defmethod prompter:object-attributes ((window window))
  `(("ID" ,(id window))
    ("Active buffer" ,(title (active-buffer window)))))

(define-class window-source (prompter:source)
  ((prompter:name "Windows")
   (prompter:multi-selection-p t)
   (prompter:constructor (window-list))
   (prompter:actions (list (make-mapped-command window-delete)))))

(define-command delete-window ()
  "Delete the queried window(s)."
  (prompt
   :prompt "Delete window(s):"
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

(define-command toggle-fullscreen (&optional (window (current-window)))
  "Fullscreen WINDOW, or the current window, when omitted."
  (if (fullscreen-p window)
      (ffi-window-unfullscreen window)
      (ffi-window-fullscreen window)))

(define-command toggle-toolbars (&optional (window (current-window)))
  "Toggle the visibility of the message and status buffer areas."
  (if (= 0
         (ffi-window-get-status-buffer-height window)
         (ffi-window-get-message-buffer-height window))
      (unpresent-current-window window)
      (present-current-window window)))

(defun present-current-window (&optional (window (current-window)))
  "Hide everything but the current buffer."
  (ffi-window-set-status-buffer-height window 0)
  (ffi-window-set-message-buffer-height window 0))

(defun unpresent-current-window (&optional (window (current-window)))
  "Unhide everything but the current buffer."
  (ffi-window-set-status-buffer-height window (height (status-buffer window)))
  (ffi-window-set-message-buffer-height window (message-buffer-height window)))
