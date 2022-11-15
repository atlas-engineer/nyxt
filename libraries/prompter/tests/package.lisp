;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter/tests
  (:use :cl :lisp-unit2)
  (:import-from :class-star #:define-class)
  (:import-from :prompter))

(setf lparallel:*kernel* (lparallel:make-kernel (or (serapeum:count-cpus) 1)))
