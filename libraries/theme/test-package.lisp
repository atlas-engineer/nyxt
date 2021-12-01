;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)

(uiop:define-package :theme/tests
  (:use #:common-lisp
        #:prove)
  (:import-from #:theme))
