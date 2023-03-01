;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :prompter
  (:use :cl)
  (:import-from :class-star #:define-class)
  (:import-from :serapeum #:export-always))
(in-package prompter)

(unless lparallel:*kernel* (setf lparallel:*kernel*
                                 (lparallel:make-kernel (or (serapeum:count-cpus) 1))))

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
  (delete nil
          (mapcar (lambda (slot)
                    (intern
                     (symbol-name
                      (first (getf (mopu:slot-properties class-specifier slot) :initargs)))
                     (symbol-package class-specifier)))
                  ;; TODO: `slot-names' or `direct-slot-names'?
                  #-ecl
                  (mopu:slot-names class-specifier)
                  #+ecl
                  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (find-class 'prompter))))))

(defun exported-p (sym)
  (eq :external
      (nth-value 1 (find-symbol (string sym) (symbol-package sym)))))

(defun public-initargs (class-specifier)
  "Return CLASS-SPECIFIER initargs as symbols (not keywords)."
  (delete-if (complement #'exported-p) (initargs class-specifier)))

(export-always '*debug-on-error*)
(defvar *debug-on-error* nil
  "When non-nil, the Lisp debugger is invoked when a condition is raised.
Otherwise all errors occuring in threads are demoted to warnings.")
