;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/application-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:->)
  (:documentation "Forward all keybindings to the web view except those in the `override-map'."))
(in-package :nyxt/application-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-nyxt-package-nicknames))

;;; Moving modes out of the `modes' slot is a bad idea: too many parts rely on
;;; the presence of the `modes' slot. Instead, use a hook to temporarily override
;;; the keymaps of all modes (except the override-map).

(define-mode application-mode ()
  "Mode that forwards all keys to the renderer.
See the mode `keymap-scheme' for special bindings."
  ((keymap-scheme
    (define-scheme "application"
      scheme:cua
      (list
       "C-z" 'application-mode)))
   (destructor
    (lambda (mode)
      (hooks:remove-hook (current-keymaps-hook (buffer mode))
                         'keep-override-map)
      (echo "Application-mode disabled.")))
   (constructor
    (lambda (mode)
      (hooks:add-hook (current-keymaps-hook (buffer mode))
                      (make-handler-keymaps-buffer #'keep-override-map))
      (echo "Application-mode enabled.")))))

(-> keep-override-map (list-of-keymaps buffer) (values list-of-keymaps buffer))
(defun keep-override-map (keymaps buffer)
  (if (nyxt::active-prompt-buffers (current-window))
      (values keymaps buffer)
      (values (list (override-map buffer)
                    (keymap (find-submode buffer 'application-mode)))
              buffer)))
