;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/keymap-scheme-mode
  (:documentation "All modes that set `keymap-scheme-name' should inherit from
this mode.
Ensures that a single keybindings mode, such as `nyxt/emacs-mode', is enabled."))
(in-package :nyxt/keymap-scheme-mode)

(define-mode keymap-scheme-mode ()
  "All modes that set `keymap-scheme-name' should inherit from this mode.
Ensures that a single keybindings mode, such as `nyxt/emacs-mode', is enabled."
  ((rememberable-p nil)
   (scheme-name
    keyscheme:cua
    :documentation "The scheme to enable.")
   (previous-keymap-scheme-name
    nil
    :type (or keymaps:keyscheme null)
    :documentation "The previous keymap scheme that will be used when ending
this mode."))
  (:toggler-command-p nil))

(defmethod enable :before ((mode keymap-scheme-mode) &key)
  (setf (previous-keymap-scheme-name mode) (keymap-scheme-name (buffer mode)))
  (mapc #'disable
        (delete mode
                (sera:filter #'keymap-scheme-mode-p (modes (buffer mode))))))

(defmethod enable ((mode keymap-scheme-mode) &key)
  (setf (keymap-scheme-name (buffer mode)) (scheme-name mode)))

(defmethod disable ((mode keymap-scheme-mode) &key)
  (setf (keymap-scheme-name (buffer mode))
        (previous-keymap-scheme-name mode)))
