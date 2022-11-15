;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :user-interface
  (:use :common-lisp)
  (:export #:id
           #:buffer
           #:update
           #:text
           #:action
           #:to-html-string
           #:paragraph
           #:progress-bar
           #:button
           #:percentage
           #:connect))
