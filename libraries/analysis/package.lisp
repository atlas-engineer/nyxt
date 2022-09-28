;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :analysis
  (:use :common-lisp)
  (:export
   #:document
   #:keywords
   #:extract-keywords
   #:extract-sections
   #:summarize-text
   #:sequence-model
   #:add-record
   #:predict))
