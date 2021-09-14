;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)

(uiop:define-package prompter/tests
  (:use #:common-lisp
        #:prove)
  (:import-from #:prompter))

(defun initialize-lparallel-kernel (&key (worker-count (serapeum:count-cpus)))
  "Initialize the lparallel kernel with WORKER-COUNT, if not supplied set it to
the amount of CPU cores.."
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel worker-count))))

(initialize-lparallel-kernel)
