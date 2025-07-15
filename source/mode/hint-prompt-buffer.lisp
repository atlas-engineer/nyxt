;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/hint-prompt-buffer
  (:documentation "Package for `hint-prompt-buffer-mode', prompt buffer mode for element hints."))
(in-package :nyxt/mode/hint-prompt-buffer)

(define-command scroll-to-hint (&key (buffer (current-buffer)))
  "Display the current hint and center it in BUFFER."
  (with-current-buffer buffer
    (nyxt/mode/hint:highlight-current-hint :element (current-suggestion-value)
                                           :scroll t)))

(define-mode hint-prompt-buffer-mode (nyxt/mode/prompt-buffer:prompt-buffer-mode)
  "`prompt-buffer' mode for element hinting.

Provides keybindings `toggle-hints-transparency' and `scroll-to-hint'."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "hint-prompt-buffer-mode" ()
      keyscheme:default
      (list
       "C-l" 'scroll-to-hint)))))
