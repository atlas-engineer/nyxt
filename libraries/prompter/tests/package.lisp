;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter/tests
  (:use :common-lisp :lisp-unit2)
  (:import-from :prompter))

(setf lparallel:*kernel* (lparallel:make-kernel (or (serapeum:count-cpus) 1)))
