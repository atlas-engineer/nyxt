;; TODO: Move scheme support to library?
;; TODO: Use CUA scheme by default.
;; TODO: Make Emacs / VI schemes inherit from each other.
;; TODO: which-key: List all bindings with some prefix.
;; TODO: Make sure it's easy enough to set global bindings.
;; TODO: Implement `C-h b`.

(in-package :next)

(defun current-keymaps (window)
  "Return the list of `keymap' for the current buffer, ordered by priority."
  (let ((buffer (active-buffer window)))
    (when buffer
      (cons (override-map buffer)
            (delete nil (mapcar #'keymap (modes (if (active-minibuffers window)
                                                    (current-minibuffer)
                                                    buffer))))))))

(defun all-keymaps (&optional (window (ipc-window-active *browser*)))
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

(declaim (ftype (function (keymap:key) boolean) printable-p))
(defun printable-p (key)
  "Return non-nil if key-chord is printable.
   Letters are printable, while function keys or backspace are not."
  (not (str:starts-with? "dead" (keymap:key-value key))))

(defun keyspecs-with-optional-keycode (keys)
  "Like `keymap:keyspecs' but displayes keys with keycodes like this:
KEYCODE-LESS-DISPLAY (KEYCODE-DISPLAY)."
  (let ((no-code-specs (keymap:keys->keyspecs
                        (mapcar (lambda (key) (keymap:copy-key key :code 0))
                                keys))))
    (if (find-if (complement #'zerop) keys :key #'keymap:key-code)
        (format nil "~a (~a)" no-code-specs (keymap:keys->keyspecs keys))
        (format nil "~a" no-code-specs))))

(defun dispatch-input-event (event buffer window)
  "Dispatch keys in `browser's `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
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
        ((ipc-generated-input-event-p window event)
         (log:debug "Forward generated event ~a" (keyspecs key-stack))
         nil)

        (t
         (multiple-value-bind (bound-function matching-keymap translated-key)
             (keymap:lookup-key key-stack
                                (current-keymaps window))
           (declare (ignore matching-keymap))
           (cond
             ((keymap:keymap-p bound-function)
              (log:debug "Prefix binding ~a" (keyspecs key-stack translated-key))
              t)

             ((functionp bound-function)
              (log:debug "Key sequence ~a" (keyspecs key-stack translated-key))
              (funcall-safely bound-function)
              (setf key-stack nil)
              t)

             ((active-minibuffers window)
              (when (printable-p (first key-stack))
                (let ((value (keymap:key-value (first key-stack))))
                  (log:debug "Insert ~s in minibuffer" value)
                  (insert value)))
              (setf key-stack nil)
              t)

             ((or (and buffer (forward-input-events-p buffer))
                  (pointer-event-p (first key-stack)))
              (log:debug "Forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil)

             (t
              (log:debug "Fallback forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil))))))))
