;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(-> binding-keys (sym:function-symbol &key (:modes list)) *)
(defun binding-keys (fn &key (modes (if (current-buffer)
                                        (enabled-modes (current-buffer))
                                        (mapcar #'make-instance (default-modes nil)))))
  ;; We can't use `(modes (make-instance 'buffer))' because modes are only
  ;; instantiated after the buffer web view, which is not possible if there is
  ;; no *browser*.
  (let* ((current-buffer (current-buffer))
         (buffer (or (current-buffer)
                     (make-instance 'input-buffer)))
         (keymaps (delete nil (mapcar #'keymap modes))))
    (unwind-protect
         (or (first (keymaps:pretty-binding-keys fn keymaps :print-style (keymaps:name (keyscheme buffer))))
             "unbound")
      (unless current-buffer
        (buffer-delete buffer)))))

(export-always 'current-keymaps)
(defun current-keymaps (&optional (buffer (let ((prompt-buffer (current-prompt-buffer)))
                                            (if (and prompt-buffer (ffi-focused-p prompt-buffer))
                                                prompt-buffer
                                                (current-buffer)))))
  "Return the list of `keymap' for the current buffer, ordered by priority.
If non-empty, return the result of BUFFER's `current-keymaps-hook' instead."
  (let ((keymaps
          (when (input-buffer-p buffer)
            (delete nil (mapcar #'keymap (enabled-modes buffer))))))
    (if (and (input-buffer-p buffer) (current-keymaps-hook buffer))
        (hooks:run-hook (current-keymaps-hook buffer) keymaps buffer)
        keymaps)))

(defun all-keymaps (&optional (window (current-window)))
  "Return all keymaps for WINDOW, including the buffer keymaps and the
prompt buffer keymaps."
  (when-let ((buffer (active-buffer window)))
    (delete nil
            (mapcar #'keymap
                    (append (enabled-modes buffer)
                            (ignore-errors (enabled-modes (current-prompt-buffer))))))))

(-> pointer-event-p (keymaps:key) boolean)
(defun pointer-event-p (key)
  "Return non-nil if KEY is a pointer event."
  (coerce (str:starts-with? "button" (keymaps:key-value key))
          'boolean))

(defun keyspecs-without-keycode (keys)
  (keymaps:keys->keyspecs
   (mapcar (lambda (key) (keymaps:copy-key key :code 0))
           keys)))

(export-always 'keyspecs-with-keycode)
(defun keyspecs-with-keycode (keys)
  "Like `keymaps:keys->keyspecs' but display keycodes as well."
  (let ((no-code-specs (keyspecs-without-keycode keys)))
    (if (find-if (complement #'zerop) keys :key #'keymaps:key-code)
        (format nil "~s [~a]" no-code-specs (keymaps:keys->keyspecs keys))
        (format nil "~s" no-code-specs))))

(-> dispatch-command ((or sym:function-symbol function)) *)
(export-always 'dispatch-command)
(defun dispatch-command (function)
  "Run FUNCTION asynchronously."
  (echo-dismiss)
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

(export-always 'dispatch-input-event)
(defun dispatch-input-event (event buffer)
  "Dispatch keys in BUFFER `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
  (with-accessors ((key-stack key-stack)) buffer
    (labels ((keyspecs (key &optional translated-key)
               (if translated-key
                   (let ((specs (keyspecs key))
                         (translated-specs (keyspecs translated-key)))
                     (if (string= specs translated-specs)
                         (format nil "~a" specs)
                         (format nil "~a (translated from ~a)"
                                 translated-specs
                                 specs)))
                   (keyspecs-with-keycode key))))
      (when (input-buffer-p buffer)
        (setf (last-event buffer) event))
      (when (prompt-buffer-p buffer)
        (run-thread "update-prompt-buffer"
          (update-prompt-input buffer
                               (ps-eval :buffer buffer
                                 (ps:chain (nyxt/ps:qs document "#input") value)))))
      (multiple-value-bind (bound-function matching-keymap translated-key)
          (the keyscheme:nyxt-keymap-value
               (keymaps:lookup-key key-stack (current-keymaps buffer)))
        (declare (ignore matching-keymap))
        (cond
          ((keymaps:keymap-p bound-function)
           (log:debug "Prefix binding ~a." (keyspecs key-stack translated-key))
           t)
          ((typep bound-function '(and (not null) (or symbol command)))
           (let ((command (typecase bound-function
                            (symbol (symbol-function (resolve-user-symbol bound-function :command)))
                            (command bound-function))))
             (log:debug "Found key binding ~a to ~a." (keyspecs key-stack translated-key) bound-function)
             (setf (last-key buffer) (first key-stack))
             (run-thread "run-command"
               (unwind-protect (funcall (command-dispatcher *browser*) command)
                 (setf key-stack nil)))
             t))
          ((or (and (input-buffer-p buffer) (forward-input-events-p buffer))
               (pointer-event-p (first (last key-stack))))
           (log:debug "Forward key ~s." (keyspecs key-stack))
           (setf key-stack nil)
           nil)
          (t
           (log:debug "Fallback forward key ~s." (keyspecs key-stack))
           (setf key-stack nil)
           nil))))))
