;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/vi-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "VI-style bindings."))
(in-package :nyxt/vi-mode)

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
   (previous-keymap-scheme-name nil
    :type (or keymap:scheme-name null)
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (keymap-scheme
    (define-scheme "vi"
      scheme:vi-normal
      (list
        "i" 'vi-insert-mode
        "button1" 'vi-button1)))
   (destructor
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))
      (setf (forward-input-events-p (buffer mode)) t)))
   (constructor
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (let ((vi-insert (find-submode buffer 'vi-insert-mode)))
          (setf (previous-keymap-scheme-name mode)
                (if vi-insert
                    (previous-keymap-scheme-name vi-insert)
                    (keymap-scheme-name buffer))))
        ;; Destroy vi-normal mode after setting previous-keymap-scheme-name, or
        ;; else we can't save the previous keymap scheme.
        (vi-insert-mode :activate nil :buffer buffer)
        (setf (keymap-scheme-name buffer) scheme:vi-normal)
        (setf (forward-input-events-p buffer) nil))))))

(define-command switch-to-vi-normal-mode (&optional (mode (find-submode (or (current-prompt-buffer) (current-buffer))
                                                                        'vi-insert-mode)))
  "Switch to the mode remembered to be the matching VI-normal one for this MODE.
See also `vi-normal-mode' and `vi-insert-mode'."
  (when mode
    (enable-modes (list (or (and (previous-vi-normal-mode mode)
                                 (mode-name (previous-vi-normal-mode mode)))
                            'vi-normal-mode))
                  (buffer mode))))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
  "Enable VI-style modal bindings (insert mode).
See `vi-normal-mode'."
  ;; We could inherit from vi-normal-mode to save the declaration of this slot
  ;; but then (find-submode ... 'vi-normal-mode) would match vi-insert-mode.
  ((glyph "vi:I")
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
                       when entering `vi-insert-mode'.")
   (destructor
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))))
   (constructor
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (let ((vi-normal (find-submode buffer 'vi-normal-mode)))
          (setf (previous-keymap-scheme-name mode)
                (if vi-normal
                    (previous-keymap-scheme-name vi-normal)
                    (keymap-scheme-name buffer))
                (previous-vi-normal-mode mode)
                vi-normal))
        (vi-normal-mode :activate nil :buffer buffer)
        (setf (keymap-scheme-name buffer) scheme:vi-insert)
        (when (passthrough-mode-p mode)
          (nyxt/passthrough-mode:passthrough-mode :activate t)))))))

(define-command vi-button1 (&optional (buffer (or (current-prompt-buffer)
                                                  (current-buffer))))
  "Enable VI insert mode when focus is on an input element on the web page.
See also `vi-normal-mode' and `vi-insert-mode'."
  ;; First we generate a button1 event so that the web view element is clicked
  ;; (e.g. a text field gets focus).
  (forward-to-renderer :window (current-window) :buffer buffer)
  (let ((response (nyxt/web-mode:%clicked-in-input? buffer)))
    (cond
      ((and (nyxt/web-mode:input-tag-p response)
            (find-submode buffer 'vi-normal-mode))
       (vi-insert-mode))
      ((and (not (nyxt/web-mode:input-tag-p response))
            (find-submode buffer 'vi-insert-mode))
       (vi-normal-mode)))))

(defmethod on-signal-load-finished ((mode vi-insert-mode) url)
  (declare (ignore url))
  (vi-normal-mode))

(defmethod nyxt/web-mode:element-focused ((mode vi-normal-mode))
  (vi-insert-mode))
