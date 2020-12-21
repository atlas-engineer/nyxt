;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/prompt-buffer-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(define-mode prompt-buffer-mode ()
  "Mode for the prompt buffer."
  ((keymap-scheme
    (define-scheme "prompt-buffer"
      scheme:cua
      (list
       "C-n" 'select-next
       "return" 'return-selection))
    ;; TODO: We could have VI bindings for the minibuffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(define-command return-selection (&optional (prompt-buffer (current-prompt-buffer)))
  "Have the PROMT-BUFFER return the selection, then quit."
  (prompter:return-selection (nyxt:prompter prompt-buffer))
  (hide-prompt-buffer prompt-buffer))

(define-command cancel-input (&optional (prompt-buffer (current-prompt-buffer)))
  "Close the prompt-buffer without further action."
  (prompter:destructor (nyxt:prompter prompt-buffer))
  (hide-prompt-buffer prompt-buffer))
