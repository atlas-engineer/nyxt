;; TODO: which-key: List all bindings with some prefix.
;; TODO: Make sure it's easy enough to set global bindings.

(in-package :next)

(defmacro command-markup (fn)
  "Print FN in HTML followed its bindings in parentheses."
  ;; Warning: We should not use markup:markup here because too much use of it
  ;; inside another `markup:markup' takes forever to expand.
  `(markup:raw
    (format nil "<span><code>~a</code> (<code>~a</code>)</span>"
            (string-downcase (symbol-name ,fn))
            (binding-keys ,fn))))

(declaim (ftype (function (string &rest keymap:keymap)
                          keymap:keymap)
                make-keymap))
(export-always 'make-keymap)
(defun make-keymap (name &rest parents)
  "Like `keymap:make-keymap' but only allow binding function symbols.

Example:

\(defvar *my-keymap* (make-keymap \"my-map\")
  \"My keymap.\")"
  (let ((keymap (apply #'keymap:make-keymap name parents)))
    (setf (keymap:bound-type keymap) '(or keymap:keymap function-symbol))
    keymap))

(export-always 'current-keymaps)
(defun current-keymaps (&optional (buffer (if (active-minibuffers (current-window))
                                              (current-minibuffer)
                                              (current-buffer))))
  "Return the list of `keymap' for the current buffer, ordered by priority."
  (let ((keymaps
          (when buffer
            (cons (override-map buffer)
                  (delete nil (mapcar #'keymap (modes buffer)))))))
    (if (current-keymaps-hook buffer)
        (hooks:run-hook (current-keymaps-hook buffer) keymaps buffer)
        keymaps)))

(defun all-keymaps (&optional (window (current-window)))
  "Return all keymaps for WINDOW, including the buffer keymaps and the
minibuffer keymaps."
  (let ((buffer (active-buffer window)))
    (when buffer
      (apply #'append (list (override-map buffer))
             (mapcar
              (lambda (buffer-or-minibuffer)
                (delete nil (mapcar #'keymap (modes buffer-or-minibuffer))))
              (delete nil (list buffer (current-minibuffer))))))))

(declaim (ftype (function (keymap:key) boolean) pointer-event-p))
(defun pointer-event-p (key)
  "Return non-nil if key-chord is a pointer event, e.g. a mouton button click."
  (coerce (str:starts-with? "button" (keymap:key-value key))
          'boolean))

(export-always 'keyspecs-with-optional-keycode)
(defun keyspecs-with-optional-keycode (keys) ; TODO: Remove "optional" from name.
  "Like `keymap:keyspecs' but displayes keys with keycodes like this:
KEYCODE-LESS-DISPLAY (KEYCODE-DISPLAY)."
  (let ((no-code-specs (keymap:keys->keyspecs
                        (mapcar (lambda (key) (keymap:copy-key key :code 0))
                                keys))))
    (if (find-if (complement #'zerop) keys :key #'keymap:key-code)
        (format nil "~a (~a)" no-code-specs (keymap:keys->keyspecs keys))
        (format nil "~a" no-code-specs))))

(export-always 'dispatch-input-event)
(defun dispatch-input-event (event buffer window printable-p)
  "Dispatch keys in `browser's `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
  (echo-dismiss) ; Clean up message-view on keypress.
  (with-accessors ((key-stack key-stack)) *browser*
    (labels ((keyspecs (key &optional translated-key)
               (if translated-key
                   (format nil "~a (translated from ~a)"
                           (keyspecs translated-key)
                           (keyspecs key))
                   (keyspecs-with-optional-keycode key))))
      (when buffer
        (setf (last-event buffer) event))
      (cond
        ((ffi-generated-input-event-p window event)
         (log:debug "Forward generated event ~a" (keyspecs key-stack))
         nil)

        (t
         (multiple-value-bind (bound-function matching-keymap translated-key)
             (keymap:lookup-key key-stack (current-keymaps))
           (declare (ignore matching-keymap))
           (cond
             ((keymap:keymap-p bound-function)
              (log:debug "Prefix binding ~a" (keyspecs key-stack translated-key))
              t)

             ((typep bound-function 'function-symbol)
              (log:debug "Key sequence ~a" (keyspecs key-stack translated-key))
              (funcall-safely bound-function)
              (setf key-stack nil)
              t)

             ((active-minibuffers window)
              (when printable-p
                (dolist (key key-stack)
                  (let ((value (keymap:key-value key)))
                    (log:debug "Insert ~s in minibuffer" value)
                    (insert value))))
              (setf key-stack nil)
              t)

             ((or (and buffer (forward-input-events-p buffer))
                  (pointer-event-p (first (last key-stack))))
              (log:debug "Forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil)

             (t
              (log:debug "Fallback forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil))))))))
