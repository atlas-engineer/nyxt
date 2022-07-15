;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package prompter/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:prompter))

(setf lparallel:*kernel* (lparallel:make-kernel (or (serapeum:count-cpus) 1)))
