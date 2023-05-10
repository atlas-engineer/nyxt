;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/autofill
  (:documentation "Mode to fill forms more rapidly.

The whole API is centered around `autofill' class and its slots:
- `name' as the descriptive name of the autofill. Accessed with `autofill-name'.
- and `fill' as the actual content-designating thing (string or a
  function). Accessed with `autofill-fill'.

`autofill' is funcallable. When funcallable, returns the string produced by the
autofill.

Then, the command eponymously called `autofill' actually fills (with
`ffi-buffer-paste') the contents into the page.

See the `autofill-mode' for the external user-facing APIs."))
(in-package :nyxt/mode/autofill)

(export-always 'make-autofill)
(defun make-autofill (&rest args)
  (apply #'make-instance 'autofill args))

(define-mode autofill-mode ()
  "Mode to fill forms more rapidly.

See `nyxt/mode/autofill' package documentation for implementation details and
internal programming APIs."
  ((visible-in-status-p nil)
   (rememberable-p t)
   (autofills
    (list (make-autofill :name "Name" :fill "My Name")
          (make-autofill :name "Hello Printer"
                         :fill (lambda () (format nil "hello!"))))
    :documentation "To autofill run the command `autofill'.
Use this slot to customize the autofill values available.

The fill can be a string value or a function.  The latter allows you to provide
content dynamic to the context.")
   (keyscheme-map
    (define-keyscheme-map "autofill-mode" ()
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
  (:metaclass closer-mop:funcallable-standard-class)
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t))

(defmethod initialize-instance :after ((autofill autofill) &key &allow-other-keys)
  (closer-mop:set-funcallable-instance-function
   autofill (typecase (autofill-fill autofill)
              (string (lambda () (autofill-fill autofill)))
              (function (autofill-fill autofill)))))

(define-class autofill-source (prompter:source)
  ((prompter:name "Autofills")
   (prompter:constructor (autofills (find-submode 'autofill-mode)))
   (prompter:actions-on-return
    (lambda-command autofill* (autofills)
      (ffi-buffer-paste (current-buffer)
                        (funcall (first autofills))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod prompter:object-attributes ((autofill autofill) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(autofill-name autofill))
    ("Fill"
     ,(let ((f (autofill-fill autofill)))
        (typecase f
          (string (write-to-string f))
          (t "function")))
     nil 3)))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (prompt :prompt "Autofill"
          :sources 'autofill-source))
