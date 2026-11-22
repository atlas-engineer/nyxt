;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/hint-prompt-buffer
  (:documentation "Package for `hint-prompt-buffer-mode', prompt buffer mode for element hints."))
(in-package :nyxt/mode/hint-prompt-buffer)

(define-command scroll-to-hint ((prompt-buffer prompt-buffer))
  "Display the current hint and center it in PROMPT-BUFFER's target buffer."
  (when-let* ((source (current-source prompt-buffer))
              (page (and (typep source 'nyxt/mode/hint::hint-source)
                         (buffer source)))
              (element (current-suggestion-value prompt-buffer)))
    (nyxt/mode/hint:highlight-current-hint page
                                           :element element
                                           :scroll t)))

(define-mode hint-prompt-buffer-mode (nyxt/mode/prompt-buffer:prompt-buffer-mode)
  "`prompt-buffer' mode for element hinting.

Provides keybindings `scroll-to-hint'."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "hint-prompt-buffer-mode" ()
      keyscheme:default
      (list
       "C-l" 'scroll-to-hint)))))
