;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)
;; TODO: which-key: List all bindings with some prefix.
;; TODO: Make sure it's easy enough to set global bindings.

(-> binding-keys (sym:function-symbol &key (:modes list)) *)
(defun binding-keys (fn &key (modes (if (current-buffer)
                                        (modes (current-buffer))
                                        (mapcar #'make-instance (default-modes nil)))))
  ;; We can't use `(modes (make-instance 'buffer))' because modes are only
  ;; instantiated after the buffer web view, which is not possible if there is
  ;; no *browser*.
  (let* ((current-buffer (current-buffer))
         (buffer (or (current-buffer)
                     (make-instance 'input-buffer)))
         (keymaps (cons (override-map buffer)
                        (delete nil (mapcar #'keymap modes)))))
    (unwind-protect
         (or (first (keymaps:pretty-binding-keys fn keymaps :print-style (keymaps:name (keyscheme buffer))))
             "unbound")
      (unless current-buffer
        (buffer-delete buffer)))))

(-> make-keymap (string &rest keymaps:keymap) keymaps:keymap)
(export-always 'make-keymap)
(defun make-keymap (name &rest parents)
  "Like `keymaps:make-keymap' but only allow binding function symbols, commands
or keymaps.

Example:

\(defvar *my-keymap* (make-keymap \"my-map\")
  \"My keymap.\")"
  (let ((keymap (apply #'keymaps:make-keymap name parents)))
    (setf (keymaps:bound-type keymap) 'keyscheme:nyxt-keymap-value)
    keymap))

(export-always 'current-keymaps)
(defun current-keymaps (&optional (buffer (let ((prompt-buffer (current-prompt-buffer)))
                                            (if (and prompt-buffer (ffi-focused-p prompt-buffer))
                                                prompt-buffer
                                                (current-buffer)))))
  "Return the list of `keymap' for the current buffer, ordered by priority.
If non-empty, return the result of BUFFER's `current-keymaps-hook' instead."
  (let ((keymaps
          (when (input-buffer-p buffer)
            (cons (override-map buffer)
                  (delete nil (mapcar #'keymap (modes buffer)))))))
    (if (and (input-buffer-p buffer) (current-keymaps-hook buffer))
        (hooks:run-hook (current-keymaps-hook buffer) keymaps buffer)
        keymaps)))

(defun all-keymaps (&optional (window (current-window)))
  "Return all keymaps for WINDOW, including the buffer keymaps and the
prompt buffer keymaps."
  (let ((buffer (active-buffer window)))
    (when buffer
      (apply #'append (list (override-map buffer))
             (mapcar
              (lambda (buffer-or-prompt-buffer)
                (delete nil (mapcar #'keymap (modes buffer-or-prompt-buffer))))
              (delete nil (list buffer (current-prompt-buffer))))))))

(-> pointer-event-p (keymaps:key) boolean)
(defun pointer-event-p (key)
  "Return non-nil if key-chord is a pointer event, e.g. a mouton button click."
  (coerce (str:starts-with? "button" (keymaps:key-value key))
          'boolean))

(defun keyspecs-without-keycode (keys)
  (keymaps:keys->keyspecs
   (mapcar (lambda (key) (keymaps:copy-key key :code 0))
           keys)))

(export-always 'keyspecs-with-optional-keycode)
(defun keyspecs-with-optional-keycode (keys) ; TODO: Remove "optional" from name.
  "Like `keymaps:keyspecs' but displays keys with keycodes like this:
KEYCODE-LESS-DISPLAY (KEYCODE-DISPLAY)."
  (let ((no-code-specs (keyspecs-without-keycode keys)))
    (if (find-if (complement #'zerop) keys :key #'keymaps:key-code)
        (format nil "~s [~a]" no-code-specs (keymaps:keys->keyspecs keys))
        (format nil "~s" no-code-specs))))

(-> dispatch-command ((or sym:function-symbol function)) *)
(export-always 'dispatch-command)
(defun dispatch-command (function)
  "Default `command-dispatcher'. Runs FUNCTION asynchronously."
  (echo-dismiss)                        ; Clean up message-view on command.
  ;; TODO: Instead of hard-coding these ignored-commands, we could add a boolean
  ;; slot to the `command' class.
  (let ((ignored-commands '(execute-command
                            execute-predicted-command
                            next-suggestion
                            previous-suggestion
                            next-source
                            previous-source))
        (function-function (typecase function
                             (symbol (symbol-function function))
                             (function function))))
    (unless (find (name function-function)
                  ignored-commands
                  :test (lambda (x y) (search (symbol-name x) (symbol-name y))))
      (analysis:add-record (command-model *browser*)
                           (list (last-command *browser*) function))
      (setf (last-command *browser*) function-function))
    (run-async function)))

(export-always 'dispatch-input-skip)
(defun dispatch-input-skip (keyspecs)
  "Default `input-scrip-dispatcher'. Logs the skipped key."
  (log:debug "Skipping input event key ~s" keyspecs))

(export-always 'dispatch-input-event)
(defun dispatch-input-event (event buffer window)
  "Dispatch keys in WINDOW `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
  (with-accessors ((key-stack key-stack)) window
    (labels ((keyspecs (key &optional translated-key)
               (if translated-key
                   (let ((specs (keyspecs key))
                         (translated-specs (keyspecs translated-key)))
                     (if (string= specs translated-specs)
                         (format nil "~a" specs)
                         (format nil "~a (translated from ~a)"
                                 translated-specs
                                 specs)))
                   (keyspecs-with-optional-keycode key))))
      (when (input-buffer-p buffer)
        (setf (last-event buffer) event))
      (cond
        ((ffi-generated-input-event-p window event)
         (log:debug "Forward generated event ~a" (keyspecs key-stack))
         (setf key-stack nil)
         nil)

        (t
         (multiple-value-bind (bound-function matching-keymap translated-key)
             (the keyscheme:nyxt-keymap-value
                  (keymaps:lookup-key key-stack (current-keymaps)))
           (declare (ignore matching-keymap))
           (cond
             ((keymaps:keymap-p bound-function)
              (echo "Pressed keys: ~a" (keyspecs-without-keycode key-stack))
              (log:debug "Prefix binding ~a" (keyspecs key-stack translated-key))
              t)

             ((typep bound-function '(and (not null) (or symbol command)))
              (let ((command (typecase bound-function
                               (symbol (symbol-function (resolve-user-symbol bound-function :command)))
                               (command bound-function))))
                (check-type command command)
                (log:debug "Found key binding ~a to ~a" (keyspecs key-stack translated-key) bound-function)
                ;; We save the last key separately to keep it available to the
                ;; command even after key-stack has been reset in the other
                ;; thread.
                (setf (last-key window) (first key-stack))
                (unwind-protect
                     (funcall (command-dispatcher window) command)
                  ;; We must reset the key-stack on errors or else all subsequent
                  ;; keypresses will keep triggering the same erroring command.
                  (setf key-stack nil))
                t))

             ((or (and (input-buffer-p buffer) (forward-input-events-p buffer))
                  (pointer-event-p (first (last key-stack))))
              (log:debug "Forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil)

             ((and (input-buffer-p buffer) (not (forward-input-events-p buffer)))
              ;; After checking `pointer-event-p', otherwise pointer events
              ;; might not be forwarded.
              (funcall (input-skip-dispatcher window) (keyspecs key-stack))
              (setf key-stack nil)
              t)

             (t
              (log:debug "Fallback forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil))))))))
