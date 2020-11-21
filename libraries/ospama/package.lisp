;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package ospama
  (:use #:common-lisp)
  (:use #:trivia)
  (:import-from #:class* #:define-class)
  (:import-from #:serapeum #:export-always))

(defvar ospama::scheme-syntax nil)
