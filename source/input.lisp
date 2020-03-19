;;; keymap.lisp --- lisp subroutines for key binding detection

;; TODO: Fix dead keys in the minibuffer.
;; TODO: Support self-insertable keys.  Make sure dead keys work.
;; TODO: Make modifiers customizable.
;; TODO: Move scheme support to library?
;; TODO: Use CUA scheme by default.
;; TODO: Make Emacs / VI schemes inherit from each other.
;; TODO: Support key codes.  Make sure it's dynamic, i.e. when the keyboard
;; layout changes, the binding should remain on the same hardware keys.
;; TODO: which-key: List all bindings with some prefix.
;; TODO: List command bindings (find Emacs equivalent name).  Make sure it's
;; dynamic, e.g. that it's updated when the keymap scheme is updated.
;; TODO: Make sure it's easy enough to set global bindings.
;; TODO: Implement `C-h k`: documentation for keypresses.  Make sure it tells
;; which keymap it's defined in.

(in-package :next)
(annot:enable-annot-syntax)

(defun current-keymaps (window)
  "Return the list of `keymap' for the current buffer, ordered by priority."
  (let ((buffer (active-buffer window)))
    (when buffer
      (cons (override-map buffer)
            (delete-if #'null (mapcar #'keymap (modes (if (active-minibuffers window)
                                                          (current-minibuffer)
                                                          (active-buffer window)))))))))

(declaim (ftype (function (keymap:key) boolean) pointer-event-p))
(defun pointer-event-p (key)
  "Return non-nil if key-chord is a pointer event, e.g. a mouton button click."
  (coerce (str:starts-with? "button" (keymap:key-value key))
          'boolean))

(declaim (ftype (function (keymap:key) boolean) printable-p))
(defun printable-p (key)
  "Return non-nil if key-chord is printable.
   Letters are printable, while function keys or backspace are not."
  ;; TODO: Implement printable-p?
  (declare (ignore key))
  t)

(defun dispatch-input-event (event buffer window)
  "Dispatch keys in `browser's `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
  (with-accessors ((key-stack key-stack)) *browser*
    (let ((keyspecs (let ((specs (keymap:keys->keyspecs key-stack))
                          (no-code-specs (keymap:keys->keyspecs
                                          (mapcar (lambda (key) (keymap:copy-key key :code 0))
                                                  key-stack))))
                      (if (find-if (complement #'zerop) key-stack :key #'keymap:key-code)
                          (format nil "~a (~a)" no-code-specs specs)
                          (format nil "~a" no-code-specs)))))
      (when buffer
        (setf (last-event buffer) event))
      (cond
        ((ipc-generated-input-event-p window event)
         (log:debug "Forward generated event ~a" keyspecs)
         nil)

        (t
         (let ((bound-function (keymap:lookup-key (reverse key-stack)
                                                  (current-keymaps window))))
           (cond
             ((keymap:keymap-p bound-function)
              (log:debug "Prefix binding ~a" keyspecs)
              t)

             ((functionp bound-function)
              (log:debug "Key sequence ~a bound to:" keyspecs)
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
              (log:debug "Forward key ~s" keyspecs)
              (setf key-stack nil)
              nil)

             (t
              (log:debug "Fallback forward key ~s" keyspecs)
              (setf key-stack nil)
              nil))))))))
