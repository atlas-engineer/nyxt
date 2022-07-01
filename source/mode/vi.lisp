;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/vi-mode
  (:documentation "VI-style bindings."))
(in-package :nyxt/vi-mode)

(define-mode vi-normal-mode (nyxt/keyscheme-mode:keyscheme-mode)
  "Enable VI-style modal bindings (normal mode).
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(vi-normal-mode) %slot-default%))))

In `vi-insert-mode', CUA bindings are still available unless
`passthrough-mode-p' is non-nil in `vi-insert-mode'.
You can also enable `passthrough-mode' manually to forward all keybindings to
the web page.

See also `vi-insert-mode'."
  ((glyph "vi:N")
   (nyxt/keyscheme-mode:keyscheme keyscheme:vi-normal)
   (keymap-scheme
    (define-keyscheme-map "vi" ()
      keyscheme:vi-normal
      (list
       "i" 'vi-insert-mode
       "button1" 'vi-button1)))))


;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode (nyxt/keyscheme-mode:keyscheme-mode)
  "Enable VI-style modal bindings (insert mode).
See `vi-normal-mode'."
  ;; We could inherit from vi-normal-mode to save the declaration of this slot
  ;; but then (find-submode ... 'vi-normal-mode) would match vi-insert-mode.
  ((glyph "vi:I")
   (nyxt/keyscheme-mode:keyscheme keyscheme:vi-insert)
   (previous-vi-normal-mode nil
    :type (or vi-normal-mode null)
    :documentation "The `vi-normal-mode' that this insert mode is tied to.")
   (keymap-scheme
    (define-keyscheme-map "vi" ()
      keyscheme:vi-insert
      (list
       "C-z" 'nyxt/passthrough-mode:passthrough-mode
       "escape" 'switch-to-vi-normal-mode)))
   (passthrough-mode-p nil
                       :type boolean
                       :documentation "Whether to default to `passthrough-mode'
                       when entering `vi-insert-mode'.")))

(defmethod enable ((mode vi-normal-mode) &key)
  (with-accessors ((buffer buffer)) mode
    (let ((vi-insert (find-submode 'vi-insert-mode buffer)))
      (setf (nyxt/keyscheme-mode:previous-keyscheme mode)
            (if vi-insert
                (nyxt/keyscheme-mode:previous-keyscheme vi-insert)
                (keyscheme buffer)))
      (when vi-insert
        ;; Destroy vi-normal mode after setting previous-keyscheme, or else we
        ;; can't save the previous keyscheme.
        (disable vi-insert)))
    (call-next-method)
    (setf (forward-input-events-p buffer) nil)))

(defmethod disable ((mode vi-normal-mode) &key)
  (call-next-method)
  (setf (forward-input-events-p (buffer mode)) t))

(define-command switch-to-vi-normal-mode (&optional (mode (find-submode 'vi-insert-mode
                                                              (or (current-prompt-buffer) (current-buffer)))))
  "Switch to the mode remembered to be the matching VI-normal one for this MODE.
See also `vi-normal-mode' and `vi-insert-mode'."
  (when mode
    (enable-modes (list (or (and (previous-vi-normal-mode mode)
                                 (sera:class-name-of (previous-vi-normal-mode mode)))
                            'vi-normal-mode))
                  (buffer mode))))

(defmethod enable ((mode vi-insert-mode) &key)
  (with-accessors ((buffer buffer)) mode
    (let ((vi-normal (find-submode 'vi-normal-mode buffer)))
      (setf (nyxt/keyscheme-mode:previous-keyscheme mode)
            (if vi-normal
                (nyxt/keyscheme-mode:previous-keyscheme vi-normal)
                (keyscheme buffer))
            (previous-vi-normal-mode mode)
            vi-normal)
      (when vi-normal
        (disable vi-normal)))
    (call-next-method)
    (when (passthrough-mode-p mode)
      (enable-modes '(nyxt/passthrough-mode:passthrough-mode)
                    buffer))))

(define-command vi-button1 (&optional (buffer (focused-buffer)))
  "Enable VI insert mode when focus is on an input element on the web page.
See also `vi-normal-mode' and `vi-insert-mode'."
  ;; First we generate a button1 event so that the web view element is clicked
  ;; (e.g. a text field gets focus).
  (forward-to-renderer :window (current-window) :buffer buffer)
  (let ((response (nyxt/document-mode:%clicked-in-input? buffer)))
    (when (and (nyxt/document-mode:input-tag-p response)
               (find-submode 'vi-normal-mode buffer))
      (enable-modes '(vi-insert-mode) buffer))))

(defmethod on-signal-load-finished ((mode vi-insert-mode) url)
  (declare (ignore url))
  (enable-modes '(vi-normal-mode)))

(defmethod nyxt/document-mode:element-focused ((mode vi-normal-mode))
  (enable-modes '(vi-insert-mode)))

(defmethod nyxt:mode-status ((status status-buffer) (vi-normal vi-normal-mode))
  (spinneret:with-html-string
    (:button :type "button"
             :title "vi-normal-mode"
             :onclick (ps:ps (nyxt/ps:lisp-eval (:title "vi-insert-mode") (nyxt/vi-mode:vi-insert-mode)))
             (:code "N"))))

(defmethod nyxt:mode-status ((status status-buffer) (vi-normal vi-insert-mode))
  (spinneret:with-html-string
    (:button :type "button"
             :title "vi-insert-mode"
             :onclick (ps:ps (nyxt/ps:lisp-eval (:title "vi-normal-mode") (nyxt/vi-mode:vi-normal-mode)))
             ;; Note: We use :code to make it monospaced, so that it's more clickable.
             (:code "I"))))
