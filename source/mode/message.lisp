;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/message
  (:documentation "Package for `message-mode', a mode for messages and logs display.
All the APIs of the `message-mode' are exported, no implementation details."))
(in-package :nyxt/mode/message)

(define-mode message-mode ()
  "Mode for log and message listing.
Mainly used on `list-messages' page."
  ((visible-in-status-p nil))
  (:toggler-command-p nil))

(define-command clear-messages ()
  "Clear the *Messages* buffer and the underlying message data."
  (setf (slot-value *browser* 'messages-content) '())
  (echo "Messages cleared."))

(define-internal-page-command-global list-messages ()
    (buffer "*Messages*" 'nyxt/mode/message:message-mode)
  "Show the *Messages* buffer."
  (spinneret:with-html-string
    (:h1 "Messages")
    (:p
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:lisp-eval (:title "clear-messages")
                                                 (clear-messages)
                                                 (nyxt:reload-buffer buffer)))
              "Clear")
     ; TODO: Fix load order so that `nyxt:manual' is defined here.
     (:button :class "button" :onclick (ps:ps (nyxt/ps:lisp-eval (:title "manual") (funcall (sym:resolve-symbol :manual :function)))) "Manual"))
    (:p (:small "See the troubleshooting section of the manual if you need help diagnosing warnings and errors."))
    (:ul
     (loop for message in (reverse (nyxt:messages-content *browser*))
           collect (if (html-string-p message)
                       (:li (:raw message))
                       (:li (:pre message)))))))
