;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/keyscheme-mode
  (:documentation "All modes that set `keymap-scheme-name' should inherit from
this mode.
Ensures that a single keybindings mode, such as `nyxt/emacs-mode', is enabled."))
(in-package :nyxt/keyscheme-mode)

  "All modes that set `keymap-scheme-name' should inherit from this mode.
(define-mode keyscheme-mode ()
Ensures that a single keybindings mode, such as `nyxt/emacs-mode', is enabled."
  ((rememberable-p nil)
   (keyscheme
    keyscheme:cua
    :type keymaps:keyscheme
    :documentation "The `keymaps:keyscheme' to enable.")
   (previous-keyscheme
    nil
    :type (or keymaps:keyscheme null)
    :documentation "The active `keymaps:keyscheme' when disabling this mode."))
  (:toggler-command-p nil))

  (setf (previous-keyscheme mode) (keymap-scheme-name (buffer mode)))
(defmethod enable :before ((mode keyscheme-mode) &key)
  (mapc #'disable
        (delete mode
                (sera:filter #'keymap-scheme-mode-p (modes (buffer mode))))))

  (setf (keymap-scheme-name (buffer mode)) (keyscheme mode)))
(defmethod enable ((mode keyscheme-mode) &key)

  (setf (keymap-scheme-name (buffer mode)) (previous-keyscheme mode)))
(defmethod disable ((mode keyscheme-mode) &key)
