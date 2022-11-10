;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/message-mode
    (:documentation "Mode for messages and logs"))
(in-package :nyxt/message-mode)

(define-mode message-mode ()
  "Mode for log and message listing."
  ((visible-in-status-p nil))
  (:toggler-command-p nil))

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (slot-value *browser* 'messages-content) '())
  (echo "Messages cleared."))

(define-internal-page-command-global list-messages ()
    (buffer "*Messages*" 'nyxt/message-mode:message-mode)
  "Show the *Messages* buffer."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Messages")
    (:p (:button :class "button" :onclick (ps:ps (nyxt/ps:lisp-eval (:title "manual") (funcall (resolve-symbol :manual :function)))) "Manual") ; TODO: Fix load order so that `nyxt:manual' is defined here.
        (:small "See the troubleshooting section of the manual if you need help diagnosing warnings and errors."))
    (:p
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:lisp-eval (:title "clear-messages")
                                                  (clear-messages)
                                                  (nyxt:reload-buffer buffer)))
              "Clear"))
    (:ul
     (loop for message in (reverse (nyxt:messages-content *browser*))
             collect (if (html-string-p message)
                         (:li (:raw message))
                         (:li (:pre message)))))))
