;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode base-mode ()
  "Mode that does nothing but bind the general-purpose key bindings."
  ((keymap-scheme (define-scheme "base"
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
                     "C-l" 'set-url
                     "M-l" 'set-url-new-buffer
                     "f5" 'reload-current-buffer
                     "C-r" 'reload-current-buffer
                     "C-R" 'reload-buffer
                     "C-m o" 'set-url-from-bookmark
                     "C-m C-o" 'set-url-from-bookmark-new-buffer
                     "C-m s" 'bookmark-current-page
                     "C-d" 'bookmark-current-page
                     "C-m C-s" 'bookmark-page
                     "C-m k" 'bookmark-delete
                     "C-t" 'make-buffer-focus
                     "C-m u" 'bookmark-url
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
                     "f11" 'fullscreen-current-window
                     "C-o" 'load-file
                     "C-j" 'list-downloads
                     "C-space" 'execute-command
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
                     "C-t" 'make-buffer-focus
                     "C-r" 'reload-current-buffer
                     "C-R" 'reload-buffer
                     "C-m o" 'set-url-from-bookmark
                     "C-m C-o" 'set-url-from-bookmark-new-buffer
                     "C-m s" 'bookmark-current-page
                     "C-m C-s" 'bookmark-page
                     "C-m k" 'bookmark-delete
                     "C-m u" 'bookmark-url
                     "C-M-l" 'copy-url
                     "C-M-i" 'copy-title
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
                     "C-o" 'load-file
                     "M-x" 'execute-command
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
                     "R" 'reload-current-buffer
                     "r" 'reload-buffer
                     "m o" 'set-url-from-bookmark
                     "m O" 'set-url-from-bookmark-new-buffer
                     "m m" 'bookmark-page
                     "m M" 'bookmark-current-page
                     "y u" 'copy-url
                     "y t" 'copy-title
                     ;; TODO: Use "f1 *" instead?
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
                     "C-o" 'load-file
                     ":" 'execute-command
                     "W" 'make-window
                     "C-w C-w" 'make-window
                     "C-w q" 'delete-current-window
                     "C-w C-q" 'delete-window
                     "u" 'reopen-buffer
                     "C-x C-f" 'open-file))
                  :type keymap:scheme)))

(define-command list-buffers ()
  "Show the *Buffers* buffer."
  (let ((buffer (or (find-buffer 'buffer-listing-mode)
                    (nyxt/buffer-listing-mode:buffer-listing-mode
                     :activate t
                     :buffer (make-internal-buffer :title "*Buffers*")))))
    (let* ((content
             (markup:markup
              (:style (style buffer))
              (:h1 "Buffers")
              (:a :class "button"
                  :href (lisp-url '(nyxt::list-buffers)) "Update")
              (:br "")
              (:div
               (loop for buffer in (buffer-list)
                     collect (markup:markup (:p
                                             (:a :class "button"
                                                 :href (lisp-url `(nyxt::delete-buffer :id ,(id buffer))) "X")
                                             (:a :class "button"
                                                 :href (lisp-url `(nyxt::switch-buffer :id ,(id buffer))) "→")
                                             (:span (title buffer) " - "(quri:render-uri (url buffer)))))))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))
