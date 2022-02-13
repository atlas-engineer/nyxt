;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package theme
  (:use #:common-lisp)
  (:import-from #:serapeum
                #:export-always)
  (:import-from #:class-star
                #:define-class)
  (:export
   #:theme
   #:background
   #:text
   #:primary
   #:secondary
   #:tertiary
   #:quaternary
   #:accent
   #:font-family
   #:with-theme
   #:themed-css))
