;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/passthrough
    (:documentation "Forward all keybindings to the web view except those in the `override-map'."))
(in-package :nyxt/mode/passthrough)

;;; Moving modes out of the `modes' slot is a bad idea: too many parts rely on
;;; the presence of the `modes' slot. Instead, use a hook to temporarily override
;;; the keymaps of all modes (except the override-map).

(define-mode passthrough-mode ()
  "Mode that forwards all keys to the renderer.
See the mode `keyscheme-map' for special bindings."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "passthrough-mode" ()
      keyscheme:default
      (list
       "C-M-z" 'passthrough-mode)))))

(defmethod enable ((mode passthrough-mode) &key)
  (hooks:add-hook (current-keymaps-hook (buffer mode)) 'keep-override-map)
  (echo "passthrough-mode enabled."))

(defmethod disable ((mode passthrough-mode) &key)
  (hooks:remove-hook (current-keymaps-hook (buffer mode))
                     'keep-override-map)
  (echo "passthrough-mode disabled."))

(-> keep-override-map ((list-of keymaps:keymap) buffer) (values (list-of keymaps:keymap) buffer))
(defun keep-override-map (keymaps buffer)
  (if (nyxt::active-prompt-buffers (current-window))
      (values keymaps buffer)
      (values (list (override-map buffer)
                    (keymap (find-submode 'passthrough-mode buffer)))
              buffer)))
