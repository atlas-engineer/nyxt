;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/message-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for messages and logs"))
(in-package :nyxt/message-mode)

(define-mode message-mode ()
  "Mode for log and message listing."
  ((rememberable-p nil)))

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (slot-value *browser* 'messages-content) '())
  (echo "Messages cleared."))

(nyxt::define-internal-page-command list-messages ()
    (buffer "*Messages*" 'nyxt/message-mode:message-mode)
  "Show the *Messages* buffer."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Messages")
    (:p (:button :class "button" :onclick (ps:ps (nyxt/ps:send-lisp-url `(nyxt::manual))) "Manual")
        "See the troubleshooting section of the manual if you need help diagnose some warnings and errors.")
    (:p
     (:button :class "button"
              :onclick (ps:ps (nyxt/ps:send-lisp-url `(progn
                                                        (nyxt/message-mode:clear-messages)
                                                        (nyxt::reload-buffers
                                                         (nyxt::buffers-get ,(id buffer))))))
              "Clear"))
    (:ul
     (loop for message in (reverse (nyxt:messages-content *browser*))
             collect (if (html-string-p message)
                         (:li (:raw message))
                         (:li (:pre message)))))))
