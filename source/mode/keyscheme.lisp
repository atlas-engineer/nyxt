;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/keyscheme
  (:documentation "All modes that set `keyscheme' should inherit from this mode.
Ensures that a single keybindings mode, such as `nyxt/mode/emacs', is enabled."))
(in-package :nyxt/mode/keyscheme)

(define-mode keyscheme-mode ()
  "All modes that set `keyscheme' should inherit from this mode.
Ensures that a single keybindings mode, such as `nyxt/mode/emacs', is enabled."
  ((rememberable-p nil)
   (keyscheme                           ; This specialized `nyxt:keyscheme'.
    keyscheme:cua
    :documentation "The `keymaps:keyscheme' to enable.")
   (previous-keyscheme
    nil
    :type (or keymaps:keyscheme null)
    :documentation "The active `keymaps:keyscheme' when disabling this mode."))
  (:toggler-command-p nil))

(defmethod enable :before ((mode keyscheme-mode) &key)
  (setf (previous-keyscheme mode) (keyscheme (buffer mode)))
  (mapc #'disable
        (delete mode
                (sera:filter #'keyscheme-mode-p (modes (buffer mode))))))

(defmethod enable ((mode keyscheme-mode) &key)
  (setf (keyscheme (buffer mode)) (keyscheme mode)))

(defmethod disable ((mode keyscheme-mode) &key)
  (setf (keyscheme (buffer mode)) (previous-keyscheme mode)))
