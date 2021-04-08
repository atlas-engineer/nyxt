;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class autofill ()
  ((key ""
        :accessor autofill-key ; TODO: Maybe use non-prefixed version instead?
        :documentation "Unique and short key to identify the autofill by.")
   (name ""
         :type string
         :accessor autofill-name
         :documentation "Displayable name of the autofill.
Is especially useful for function autofills as `autofill-fill' doesn't tell
anything meaningful for these.")
   (fill ""
         :type (or string function)
         :accessor autofill-fill
         :documentation "The text that autofill will paste.
Can be:
- a string that will be pasted as is, or
- a zero-argument function that will generate the text to paste."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t) ; TODO: Do we need predicate?
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(export-always 'make-autofill)
(defun make-autofill (&rest args)
  (apply #'make-instance 'autofill args))

(defmethod object-string ((autofill autofill))
  (autofill-key autofill))

(defmethod prompter:object-attributes ((autofill autofill))
  `(("Key" ,(autofill-key autofill))
    ("Name" ,(autofill-name autofill))
    ("Fill" ,(autofill-fill autofill))))

(defmethod object-display ((autofill autofill))
  (format nil "~a:  ~a" (autofill-key autofill)
          (cond ((stringp (autofill-fill autofill))
                 (autofill-fill autofill))
                ((functionp (autofill-fill autofill))
                 (or (autofill-name autofill) "Function")))))
