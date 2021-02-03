;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package history-tree
  (:nicknames #:htree)
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class))
