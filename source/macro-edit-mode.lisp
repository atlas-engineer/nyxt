;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/macro-edit-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for editing macros."))
(in-package :nyxt/macro-edit-mode)

(define-mode macro-edit-mode ()
  "Mode for creating and editing macros."
  ((rememberable-p nil)))

(define-command-global edit-macro ()
  "Edit a macro."
  (with-current-html-buffer (buffer "*Macro edit*" 'nyxt/macro-edit-mode:macro-edit-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Macro edit")
     (:p "Hello world!"))))
