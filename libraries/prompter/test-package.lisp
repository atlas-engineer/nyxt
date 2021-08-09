;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)

(uiop:define-package prompter/tests
  (:use #:common-lisp
        #:prove)
  (:import-from #:prompter))

(unless lparallel:*kernel*
  (setf lparallel:*kernel* (lparallel:make-kernel worker-count)))
