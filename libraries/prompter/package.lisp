;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)

(uiop:define-package prompter
  (:use #:common-lisp)
  (:import-from #:class* #:define-class)
  (:import-from #:serapeum #:export-always))

(in-package prompter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(defmacro define-function (name args &body body)
  "Eval ARGS then define function over the resulting lambda list.
All ARGS are declared as `ignorable'."
  (let ((evaluated-args (eval args)))
    `(defun ,name  ,evaluated-args
       (declare (ignorable ,@(set-difference (mapcar (lambda (arg) (if (listp arg) (first arg) arg))
                                                     evaluated-args)
                                             lambda-list-keywords)))
       ,@body)))

(defun initargs (class-specifier)
  "Return CLASS-SPECIFIER initargs as symbols (not keywords)."
  (mapcar (lambda (slot)
            (intern
             (symbol-name
              (first (getf (mopu:slot-properties class-specifier slot) :initargs)))
             (symbol-package class-specifier)))
          (mopu:slot-names class-specifier)))
