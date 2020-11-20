;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :text-analysis
  (:use :common-lisp)
  (:export
   #:document
   #:keywords
   #:summarize-text))
