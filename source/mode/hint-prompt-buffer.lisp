;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/hint-prompt-buffer
  (:documentation "Package for `hint-prompt-buffer-mode', prompt buffer mode for element hints."))
(in-package :nyxt/mode/hint-prompt-buffer)

(define-command toggle-hints-transparency (&key (buffer (current-buffer)))
  "Toggle the transparency of all element hints in BUFFER."
  (ps-eval :buffer buffer
    (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
      (if (or (= (ps:@ element style opacity) "1")
              (= (ps:@ element style opacity) ""))
          (setf (ps:@ element style opacity) "0.2")
          (setf (ps:@ element style opacity) "1.0")))))

(define-command scroll-to-hint (&key (buffer (current-buffer)))
  "Display the current hint and center it in BUFFER."
  (with-current-buffer buffer
    (nyxt/mode/hint:highlight-selected-hint
     :element (current-suggestion-value)
     :scroll t)))

(define-mode hint-prompt-buffer-mode (nyxt/mode/prompt-buffer:prompt-buffer-mode)
  "`prompt-buffer' mode for element hinting.

Provides keybindings `toggle-hints-transparency' and `scroll-to-hint'."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "hint-prompt-buffer-mode" ()
      keyscheme:default
      (list
       "M-i" 'toggle-hints-transparency
       "C-l" 'scroll-to-hint)))))
