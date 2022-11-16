;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :password
  (:use :cl)
  (:import-from :class-star #:define-class)
  (:import-from :serapeum #:export-always))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :password))
