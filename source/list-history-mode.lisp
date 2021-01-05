;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/list-history-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for listing history."))
(in-package :nyxt/list-history-mode)

(define-mode list-history-mode ()
  "Mode for listing history."
  ())
