;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class autofill ()
  ((name
    ""
    :type string
    :accessor autofill-name
    :documentation "Displayable name of the autofill.
Is especially useful for function autofills as `autofill-fill' doesn't tell
anything meaningful for these.")
   (fill
    ""
    :type (or string function)
    :accessor autofill-fill
    :documentation "The text that autofill will paste.
Can be:
- a string that will be pasted as is, or
- a zero-argument function that will generate the text to paste.

Please note that this accessor cannot be renamed to `fill' because
it will be in conflict with common-lisp:fill."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(export-always 'make-autofill)
(defun make-autofill (&rest args)
  (apply #'make-instance 'autofill args))

(defmethod prompter:object-attributes ((autofill autofill))
  `(("Name" ,(autofill-name autofill))
    ("Fill" ,(let ((f (autofill-fill autofill)))
               (typecase f
                 (string (write-to-string f))
                 (t "function"))))))
