;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/tests
  (:use #:common-lisp #:trivia)
  (:use #:nyxt)
  (:shadowing-import-from #:prove #:*debug-on-error*)
  (:use #:prove)
  (:import-from #:class-star #:define-class))
