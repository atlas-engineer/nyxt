;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/help-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for help pages"))
(in-package :nyxt/help-mode)

(define-mode help-mode ()
  "Mode for displaying documentation."
  ((rememberable-p nil)
   (target nil
           :type t
           :documentation "The value being inspected.")))
