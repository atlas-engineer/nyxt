;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/vi-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "VI-style bindings."))
(in-package :nyxt/vi-mode)
(use-nyxt-package-nicknames)

(define-mode vi-normal-mode ()
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
   (rememberable-p nil)
   (previous-keymap-scheme-name nil
    :type (or keymap:scheme-name null)
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (keymap-scheme
    (define-scheme "vi"
      scheme:vi-normal
      (list
        "i" 'vi-insert-mode
        "button1" 'vi-button1)))))


;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
  "Enable VI-style modal bindings (insert mode).
See `vi-normal-mode'."
  ;; We could inherit from vi-normal-mode to save the declaration of this slot
  ;; but then (find-submode ... 'vi-normal-mode) would match vi-insert-mode.
  ((glyph "vi:I")
   (rememberable-p nil)
   (previous-keymap-scheme-name nil
    :type (or keymap:scheme-name null)
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (previous-vi-normal-mode nil
    :type (or vi-normal-mode null)
    :documentation "The `vi-normal-mode' that this insert mode is tied to.")
   (keymap-scheme
    (define-scheme "vi"
      scheme:vi-insert
      (list
       "C-z" 'nyxt/passthrough-mode:passthrough-mode
       "escape" 'switch-to-vi-normal-mode
       "button1" 'vi-button1)))
   (passthrough-mode-p nil
                       :type boolean
                       :documentation "Whether to default to `passthrough-mode'
                       when entering `vi-insert-mode'.")))

(defmethod enable ((mode vi-normal-mode) &key)
  (with-accessors ((buffer buffer)) mode
    (let ((vi-insert (find-submode 'vi-insert-mode buffer)))
      (setf (previous-keymap-scheme-name mode)
            (if vi-insert
                (previous-keymap-scheme-name vi-insert)
                (keymap-scheme-name buffer)))
      (when vi-insert
        ;; Destroy vi-normal mode after setting previous-keymap-scheme-name, or
        ;; else we can't save the previous keymap scheme.
        (disable vi-insert)))
    (setf (keymap-scheme-name buffer) scheme:vi-normal)
    (setf (forward-input-events-p buffer) nil)))

(defmethod disable ((mode vi-normal-mode) &key)
  (setf (keymap-scheme-name (buffer mode))
        (previous-keymap-scheme-name mode))
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
      (setf (previous-keymap-scheme-name mode)
            (if vi-normal
                (previous-keymap-scheme-name vi-normal)
                (keymap-scheme-name buffer))
            (previous-vi-normal-mode mode)
            vi-normal)
      (when vi-normal
        (disable vi-normal)))
    (setf (keymap-scheme-name buffer) scheme:vi-insert)
    (when (passthrough-mode-p mode)
      (enable-modes '(nyxt/passthrough-mode:passthrough-mode)
                    buffer))))

(defmethod disable ((mode vi-insert-mode) &key)
  (setf (keymap-scheme-name (buffer mode))
        (previous-keymap-scheme-name mode)))

(define-command vi-button1 (&optional (buffer (or (current-prompt-buffer)
                                                  (current-buffer))))
  "Enable VI insert mode when focus is on an input element on the web page.
See also `vi-normal-mode' and `vi-insert-mode'."
  ;; First we generate a button1 event so that the web view element is clicked
  ;; (e.g. a text field gets focus).
  (forward-to-renderer :window (current-window) :buffer buffer)
  (let ((response (nyxt/document-mode:%clicked-in-input? buffer)))
    (cond
      ((and (nyxt/document-mode:input-tag-p response)
            (find-submode 'vi-normal-mode buffer))
       (enable-modes '(vi-insert-mode) buffer))
      ((and (not (nyxt/document-mode:input-tag-p response))
            (find-submode 'nyxt/vi-mode:vi-insert-mode buffer))
       (enable-modes '(vi-normal-mode) buffer)))))

(defmethod on-signal-load-finished ((mode vi-insert-mode) url)
  (declare (ignore url))
  (enable-modes '(vi-normal-mode)))

(defmethod nyxt/document-mode:element-focused ((mode vi-normal-mode))
  (enable-modes '(vi-insert-mode)))
