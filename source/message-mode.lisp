;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/message-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for messages and logs"))
(in-package :nyxt/message-mode)

(define-mode message-mode ()
  "Mode for log and message listing."
  ())

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (nyxt::messages-content *browser*) '())
  (echo "Messages cleared."))

(define-command nyxt::list-messages ()
  "Show the *Messages* buffer."
  (with-current-html-buffer (buffer "*Messages*" 'nyxt/message-mode:message-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Messages")
     (:a :class "button"
         :href (lisp-url '(nyxt::list-messages)) "Update")
     (:a :class "button"
         :href (lisp-url '(nyxt/message-mode:clear-messages)
                         '(nyxt::list-messages))
         "Clear")
     (:ul
      (loop for message in (reverse (nyxt:messages-content *browser*))
            collect (markup:markup (:li message)))))))
