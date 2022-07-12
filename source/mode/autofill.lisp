;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/autofill-mode
    (:documentation "Mode to fill forms more rapidly."))
(in-package :nyxt/autofill-mode)

(export-always 'make-autofill)
(defun make-autofill (&rest args)
  (apply #'make-instance 'autofill args))

(define-mode autofill-mode ()
  "Mode to fill forms more rapidly."
  ((visible-in-status-p nil)
   (rememberable-p t)
   (autofills
    (list (make-autofill :name "Name" :fill "My Name")
          (make-autofill :name "Hello Printer"
                         :fill (lambda () (sera:fmt "hello!"))))
    :documentation "To autofill run the command `autofill'.
Use this slot to customize the autofill values available.

The fill can be a string value or a function.  The latter allows you to provide
content dynamic to the context.")
   (keyscheme-map
    (define-keyscheme-map "autofill" ()
      keyscheme:default
      (list
       "C-i" 'autofill)))))

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
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class autofill-source (prompter:source)
  ((prompter:name "Autofills")
   (prompter:constructor (autofills (find-submode 'autofill-mode)))
   (prompter:return-actions
    (list (lambda-command autofill* (autofills)
            (let ((selected-fill (first autofills)))
              (cond ((stringp (autofill-fill selected-fill))
                     (%paste :input-text (autofill-fill selected-fill)))
                    ((functionp (autofill-fill selected-fill))
                     (%paste :input-text (funcall (autofill-fill selected-fill))))))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((autofill autofill) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(autofill-name autofill))
    ("Fill" ,(let ((f (autofill-fill autofill)))
               (typecase f
                 (string (write-to-string f))
                 (t "function"))))))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (prompt
   :prompt "Autofill"
   :sources (make-instance 'autofill-source)))
