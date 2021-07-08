;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/macro-edit-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for editing macros."))
(in-package :nyxt/macro-edit-mode)

(define-mode macro-edit-mode ()
  "Mode for creating and editing macros."
  ((functions
    (make-hash-table)
    :documentation "A list of functions the user has added to their macro.")))

(define-command-global edit-macro ()
  "Edit a macro."
  (with-current-html-buffer (buffer "*Macro edit*" 'nyxt/macro-edit-mode:macro-edit-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Macro edit")
     (:p "Name")
     (:input :type "text")
     (:p "Commands")
     (:p (:a :class "button"
             :href "xyz" "+ Add command"))
     (:br)
     (:hr)
     (:a :class "button"
         :href "xyz" "Save macro"))))
