;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :keymap)

;; trivial-types:proper-list doesn't check its element type.

(defun list-of-type-p (list typep)
  "Return non-ni if LIST contains only elements of the given TYPEP predicate."
  (and (listp list)
       (every (lambda (x) (typep x typep)) list)))

(defmacro define-list-type (type &optional name)
  "Define type `list-of-TYPEs'.
If type is not a simple symbol, NAME will be used to define `list-of-NAMEs'.
Example:
  (define-list-type 'string)"
  (let* ((name (string-upcase (string (or name (eval type)))))
         (predicate (intern (format nil "LIST-OF-~aS-P" name))))
    `(progn
       (defun ,predicate (list)
         (list-of-type-p list ,type))
       (deftype ,(intern (format nil "LIST-OF-~aS" name)) ()
         '(satisfies ,predicate)))))

(define-list-type 'string)
(define-list-type 'key)
(define-list-type 'keymap)
(define-list-type 'scheme-name)
