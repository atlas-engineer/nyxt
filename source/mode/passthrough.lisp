;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/passthrough
  (:documentation "Package for `passthrough-mode', mode to forward all keybindings to the page.

Keybindings in the `override-map' are an exception to the passthrough.

Utilizes the `nyxt/keyscheme' API, `nyxt/mode/keyscheme' APIs, and
`current-keymaps-hook' or `input-buffer'."))
(in-package :nyxt/mode/passthrough)

(define-mode passthrough-mode ()
  "Mode that forwards all keys to the page.

See the mode `keyscheme-map' for special bindings and `nyxt/mode/passthrough'
package documentation for implementation details and internal programming APIs."
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
