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
