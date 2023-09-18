;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/keyscheme
  (:documentation "Package for `keyscheme-mode' that all modes setting `keyscheme' should inherit from.
Ensures that a single keybindings mode, such as `nyxt/mode/emacs', is enabled."))
(in-package :nyxt/mode/keyscheme)

(define-mode keyscheme-mode ()
  "All modes that set `keyscheme' should inherit from this mode.
Ensures that a single keyscheme mode, such as `nyxt/mode/emacs', is enabled.

Example of defining a keyscheme mode:

;; Beware that this may raise package locks condition on SBCL.
(defvar keyscheme:my-keyscheme-mode
        (keyscheme:make-keyscheme \"my-keyscheme-mode\" keyscheme:default))

(define-mode my-keyscheme-mode (nyxt/mode/keyscheme:keyscheme-mode)
  ((keyscheme keyscheme:my-keyscheme-mode)))"
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
