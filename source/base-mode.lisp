(in-package :next)

;; (uiop:define-package :next/base-mode
;;   (:use :common-lisp :trivia :next)
;;   (:import-from #:keymap #:define-key #:define-scheme)
;;   (:documentation "Mode for general-purpose bindings"))
;; (in-package :next/base-mode)

(define-mode base-mode ()        ; TODO: Move to separate package?
  "Mode that does nothing but bind the general-purpose key bindings."
  ((keymap-scheme :accessor keymap-scheme :initarg :keymap-scheme :type keymap:scheme
                  :initform (define-scheme "base"
                              scheme:cua
                              (list
                               "C-q" 'quit
                               "C-[" 'switch-buffer-previous
                               "C-]" 'switch-buffer-next
                               "C-x b" 'switch-buffer
                               "C-x k" 'delete-buffer
                               "C-w" 'delete-current-buffer
                               "C-x C-k" 'delete-current-buffer
                               "C-shift-tab" 'switch-buffer-previous
                               "C-tab" 'switch-buffer-next
                               "C-pageup" 'switch-buffer-previous
                               "C-pagedown" 'switch-buffer-next
                               "C-l" 'set-url-from-current-url
                               "M-l" 'set-url-new-buffer
                               "C-m k" 'bookmark-delete
                               "C-t" 'make-buffer-focus
                               "C-m u" 'bookmark-url
                               "C-b" 'show-bookmarks
                               "f1 f1" 'help
                               "f1 t" 'tutorial
                               "f1 v" 'describe-variable
                               "f1 f" 'describe-function
                               "f1 c" 'describe-command
                               "f1 C" 'describe-class
                               "f1 s" 'describe-slot
                               "f1 k" 'describe-key
                               "f1 b" 'describe-bindings
                               "f11" 'fullscreen-current-window
                               "C-o" 'load-file
                               "C-i" 'autofill
                               "C-j" 'download-list
                               "C-space" 'execute-command
                               "M-:" 'command-evaluate
                               "C-n" 'make-window
                               "C-shift-W" 'delete-current-window
                               "C-W" 'delete-current-window
                               "M-w" 'delete-window
                               "C-/" 'reopen-buffer
                               "C-shift-t" 'reopen-buffer
                               "C-T" 'reopen-buffer
                               "C-p" 'print-buffer
                               "C-x C-f" 'open-file)
                              scheme:emacs
                              (list
                               "C-x C-c" 'quit
                               "C-[" 'switch-buffer-previous
                               "C-]" 'switch-buffer-next
                               "C-x b" 'switch-buffer
                               "C-x k" 'delete-buffer ; Emacs' default behaviour is to query.
                               "C-x C-k" 'delete-current-buffer
                               "C-x left" 'switch-buffer-previous
                               "C-x right" 'switch-buffer-next
                               "C-pageup" 'switch-buffer-previous
                               "C-pagedown" 'switch-buffer-next
                               "C-l" 'set-url
                               "M-l" 'set-url-new-buffer
                               "C-m k" 'bookmark-delete
                               "C-t" 'make-buffer-focus
                               "C-m u" 'bookmark-url
                               "C-h C-h" 'help
                               "C-h h" 'help
                               "C-h t" 'tutorial
                               "C-h v" 'describe-variable
                               "C-h f" 'describe-function
                               "C-h c" 'describe-command
                               "C-h C" 'describe-class
                               "C-h s" 'describe-slot
                               "C-h k" 'describe-key
                               "C-h b" 'describe-bindings
                               "C-o" 'load-file
                               "C-i" 'autofill
                               "M-x" 'execute-command
                               "M-:" 'command-evaluate
                               "C-x 5 2" 'make-window
                               "C-x 5 0" 'delete-current-window
                               "C-x 5 1" 'delete-window
                               "C-/" 'reopen-buffer
                               "C-x C-f" 'open-file)
                              scheme:vi-normal
                              (list
                               "Z Z" 'quit
                               "[" 'switch-buffer-previous
                               "]" 'switch-buffer-next
                               "C-pageup" 'switch-buffer-previous
                               "C-pagedown" 'switch-buffer-next
                               "g b" 'switch-buffer
                               "d" 'delete-buffer
                               "D" 'delete-current-buffer
                               "B" 'make-buffer-focus
                               "o" 'set-url
                               "O" 'set-url-new-buffer
                               "m u" 'bookmark-url
                               "m d" 'bookmark-delete
                               "C-o" 'load-file
                               ;; TODO: Use "f1 *" instead?
                               "C-h C-h" 'help
                               "C-h h" 'help
                               "C-h t" 'tutorial
                               "C-h v" 'describe-variable
                               "C-h f" 'describe-function
                               "C-h c" 'describe-command
                               "C-h C" 'describe-class
                               "C-h s" 'describe-slot
                               "C-h k" 'describe-key
                               "C-h b" 'describe-bindings
                               ":" 'execute-command
                               "M-:" 'command-evaluate
                               "W" 'make-window
                               "C-w C-w" 'make-window
                               "C-w q" 'delete-current-window
                               "C-w C-q" 'delete-window
                               "u" 'reopen-buffer
                               "C-x C-f" 'open-file)))))
