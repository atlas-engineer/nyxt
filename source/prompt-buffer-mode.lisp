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
       "hyphen" 'self-insert-minibuffer
       "space" 'self-insert-minibuffer
       "C-f" 'cursor-forwards
       "M-f" 'cursor-forwards-word
       "C-right" 'cursor-forwards-word
       "C-b" 'cursor-backwards
       "M-b" 'cursor-backwards-word
       "C-left" 'cursor-backwards-word
       "M-d" 'delete-forwards-word
       "M-backspace" 'delete-backwards-word
       "right" 'cursor-forwards
       "left" 'cursor-backwards
       "C-d" 'delete-forwards
       "delete" 'delete-forwards
       "backspace" 'delete-backwards
       "C-a" 'cursor-beginning
       "home" 'cursor-beginning
       "C-e" 'cursor-end
       "end" 'cursor-end
       "C-k" 'kill-line
       "return" 'return-selection
       "C-return" 'return-input
       "C-g" 'cancel-input
       "escape" 'cancel-input
       "C-n" 'select-next
       "C-p" 'select-previous
       "button4" 'select-previous
       "button5" 'select-next
       "down" 'select-next
       "up" 'select-previous
       "C-v" 'minibuffer-paste
       "C-y" 'minibuffer-paste
       "C-w" 'copy-suggestion
       "C-c" 'copy-suggestion
       "tab" 'insert-suggestion
       "M-h" 'minibuffer-history
       "C-space" 'minibuffer-toggle-mark
       "shift-space" 'minibuffer-toggle-mark-backwards
       "M-space" 'minibuffer-toggle-mark
       "M-a" 'minibuffer-mark-all
       "M-u" 'minibuffer-unmark-all
       "M-m" 'minibuffer-toggle-mark-all))
    ;; TODO: We could have VI bindings for the minibuffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(define-command return-selection (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer selection."
  (let ((result))
    (with-slots (must-match-p nyxt::suggestions nyxt::suggestion-cursor
                 invisible-input-p
                 multi-selection-p nyxt::marked-suggestions)
        minibuffer
      (match (or nyxt::marked-suggestions
                 (and nyxt::suggestions
                      (list (nth nyxt::suggestion-cursor nyxt::suggestions)))
                 (and (not must-match-p)
                      (list (input-buffer minibuffer))))
        ((guard nyxt::suggestions nyxt::suggestions)
         ;; Note that "immediate input" is also in suggestions, so it's caught here.
         (setf nyxt::suggestions
               (mapcar (lambda (suggestion) (if (stringp suggestion)
                                                (str:replace-all " " " " suggestion)
                                                suggestion))
                       nyxt::suggestions))
         (setf result (if multi-selection-p
                          nyxt::suggestions
                          (first nyxt::suggestions))))
        (nil (when invisible-input-p
               (setf result (input-buffer minibuffer))))))
    (quit-minibuffer minibuffer)
    (calispel:! (channel minibuffer) result)))
