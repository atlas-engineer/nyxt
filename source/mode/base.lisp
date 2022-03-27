;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro make-repeat (times)
  (let ((name (gensym (format nil "repeat-~a-" times))))
    `(make-command ,name ()
       ,(format nil "Repeat the command associated to the pressed key ~R times." times)
       ;; FIXME: A dirty hack.
       (funcall (read-from-string "nyxt/repeat-mode:repeat-key") :times ,times))))

(define-mode base-mode ()
  "Bind general-purpose commands defined by `define-command'.
This mode is a good candidate to be passed to `make-buffer'."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keymap-scheme
    (define-scheme "base"
      scheme:cua
      (list
       "C-q" 'quit
       "C-[" 'switch-buffer-previous
       "C-]" 'switch-buffer-next
       "M-down" 'switch-buffer
       "C-w" 'delete-current-buffer
       "C-shift-tab" 'switch-buffer-previous
       "C-tab" 'switch-buffer-next
       "C-`" 'switch-buffer-last
       "C-pageup" 'switch-buffer-previous
       "C-pagedown" 'switch-buffer-next
       "C-l" 'set-url
       "M-l" 'set-url-new-buffer
       "C-u M-l" 'set-url-new-nosave-buffer
       "f5" 'reload-current-buffer
       "C-r" 'reload-current-buffer
       "C-R" 'reload-buffers
       "C-m o" 'set-url-from-bookmark
       "C-m s" 'bookmark-current-url
       "C-d" 'bookmark-current-url
       "C-m C-s" 'bookmark-buffer-url
       "C-m k" 'delete-bookmark
       "C-t" 'make-buffer-focus
       "C-m l" 'bookmark-url
       "C-b" 'list-bookmarks
       "M-c l" 'copy-url
       "M-c t" 'copy-title
       "f1 f1" 'help
       "f1 t" 'tutorial
       "f1 r" 'manual
       "f1 v" 'describe-variable
       "f1 f" 'describe-function
       "f1 c" 'describe-command
       "f1 C" 'describe-class
       "f1 s" 'describe-slot
       "f1 k" 'describe-key
       "f1 b" 'describe-bindings
       "f11" 'toggle-fullscreen
       "C-O" 'load-file
       "C-j" 'list-downloads
       "C-space" 'execute-command
       "C-M-space" 'execute-extended-command
       "M-space" 'resume-prompt
       "C-n" 'make-window
       "C-shift-W" 'delete-current-window
       "C-W" 'delete-current-window
       "M-w" 'delete-window
       "C-/" 'reopen-buffer
       "C-shift-t" 'reopen-buffer
       "C-T" 'reopen-buffer
       "C-p" 'print-buffer
       "C-o" 'nyxt/file-manager-mode:open-file)

      scheme:emacs
      (list
       "C-x C-c" 'quit
       "C-x k" 'delete-buffer
       "C-x C-k" 'delete-current-buffer
       "C-x left" 'switch-buffer-previous
       "C-x right" 'switch-buffer-next
       "C-x b" 'switch-buffer
       "C-x C-b" 'nyxt/buffer-listing-mode::list-buffers
       "C-M-l" 'copy-url
       "C-M-t" 'copy-title
       "C-h C-h" 'help
       "C-h h" 'help
       "C-h t" 'tutorial
       "C-h r" 'manual
       "C-h v" 'describe-variable
       "C-h f" 'describe-function
       "C-h c" 'describe-command
       "C-h C" 'describe-class
       "C-h s" 'describe-slot
       "C-h k" 'describe-key
       "C-h b" 'describe-bindings
       "C-d" 'list-downloads
       "M-x" 'execute-command
       "C-M-x" 'execute-extended-command
       "M-1" (make-repeat 1)
       "M-2" (make-repeat 2)
       "M-3" (make-repeat 3)
       "M-4" (make-repeat 4)
       "M-5" (make-repeat 5)
       "M-6" (make-repeat 6)
       "M-7" (make-repeat 7)
       "M-8" (make-repeat 8)
       "M-9" (make-repeat 9)
       "C-x r j" 'set-url-from-bookmark
       "C-x r M" 'bookmark-current-url
       "C-x r m" 'bookmark-buffer-url
       "C-x r k" 'delete-bookmark
       "C-x r l" 'bookmark-url
       "C-x 5 2" 'make-window
       "C-x 5 0" 'delete-current-window
       "C-x 5 1" 'delete-window
       "C-x C-f" 'nyxt/file-manager-mode:open-file)

      scheme:vi-normal
      (list
       "Z Z" 'quit
       "[" 'switch-buffer-previous
       "]" 'switch-buffer-next
       "g b" 'switch-buffer
       "d" 'delete-buffer
       "D" 'delete-current-buffer
       "B" 'make-buffer-focus
       "o" 'set-url
       "O" 'set-url-new-buffer
       "g o" 'set-url-new-nosave-buffer
       "m u" 'bookmark-url
       "m d" 'delete-bookmark
       "R" 'reload-current-buffer
       "r" 'reload-buffers
       "m o" 'set-url-from-bookmark
       "m m" 'bookmark-buffer-url
       "m M" 'bookmark-current-url
       "m l" 'list-bookmarks
       "y u" 'copy-url
       "y t" 'copy-title
       "C-h C-h" 'help
       "C-h h" 'help
       "C-h t" 'tutorial
       "C-h r" 'manual
       "C-h v" 'describe-variable
       "C-h f" 'describe-function
       "C-h c" 'describe-command
       "C-h C" 'describe-class
       "C-h s" 'describe-slot
       "C-h k" 'describe-key
       "C-h b" 'describe-bindings
       ":" 'execute-command
       "1" (make-repeat 1)
       "2" (make-repeat 2)
       "3" (make-repeat 3)
       "4" (make-repeat 4)
       "5" (make-repeat 5)
       "6" (make-repeat 6)
       "7" (make-repeat 7)
       "8" (make-repeat 8)
       "9" (make-repeat 9)
       "W" 'make-window
       "C-w C-w" 'make-window
       "C-w q" 'delete-current-window
       "C-w C-q" 'delete-window
       "u" 'reopen-buffer))
    :type keymap:scheme)))
