;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/passthrough
  (:documentation "Package for `passthrough-mode', mode to forward all keybindings to the page.

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
