;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/hint-prompt-buffer-mode
    (:documentation "Prompt-buffer mode for element hints."))
(in-package :nyxt/hint-prompt-buffer-mode)

(define-command toggle-hints-transparency (&key (buffer (current-buffer)))
  "Toggle the on-screen element hints transparency."
  (with-current-buffer buffer
    (peval (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
             (if (or (= (ps:@ element style opacity) "1")
                     (= (ps:@ element style opacity) ""))
                 (setf (ps:@ element style opacity) "0.2")
                 (setf (ps:@ element style opacity) "1.0"))))))

(define-command scroll-to-hint (&key (buffer (current-buffer)))
  "Show the selected hint on screen."
  (with-current-buffer buffer
    (nyxt/hint-mode::highlight-selected-hint :element (current-suggestion-value) :scroll t)))

(define-mode hint-prompt-buffer-mode (nyxt/prompt-buffer-mode:prompt-buffer-mode)
  "Prompt buffer mode for element hinting."
  ((visible-in-status-p nil)
   (keymap-scheme
    (define-scheme "hint"
      scheme:cua
      (list
       "M-i" 'toggle-hints-transparency
       "C-l" 'scroll-to-hint)))))
