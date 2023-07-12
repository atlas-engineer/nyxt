;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; `uiop:define-package' instead of `nyxt:define-package' since it does not
;; depend on Nyxt.
(uiop:define-package :nyxt/types
  (:use :cl)
  (:import-from :serapeum #:export-always)
  (:documentation "Package for types.
It's useful to have a separate package because some types may need to generate
functions for the `satisfies' type condition."))
(in-package :nyxt/types)

;; types:proper-list doesn't check its element type.
(defun list-of-p (list type)
  "Return non-nil if LIST contains only elements of the given TYPE."
  (and (listp list)
       (every (lambda (x) (typep x type)) list)))

(export-always 'list-of)
(deftype list-of (type)
  "The empty list or a proper list of TYPE elements.
Unlike `(cons TYPE *)', it checks all the elements.
`(cons TYPE *)' does not accept the empty list."
  (unless (trivial-types:type-specifier-p type)
    (error "Invalid type specifier: ~a" type))
  (let ((predicate-name (intern
                         (string-upcase
                          (uiop:strcat "LIST-OF-"
                                       (remove-if (complement #'alphanumericp)
                                                  (princ-to-string type))
                                       "-P"))
                         (find-package :nyxt/types))))
    (unless (fboundp predicate-name)
      (setf (fdefinition predicate-name)
            (lambda (object)
              (list-of-p object type))))
    `(and list (satisfies ,predicate-name))))

(export-always 'alist-of)
(deftype alist-of (key-type &optional value-type)
  `(list-of (cons ,key-type ,(or value-type key-type))))

(export-always 'cookie-policy)
(deftype cookie-policy ()
  `(member :always :never :no-third-party))

;; The following types represent the positional arguments documented at
;; https://developer.mozilla.org/en-US/docs/Web/API/Selection/modify#parameters

(export-always 'selection-action)
(deftype selection-action ()
  "The type of change to apply."
  '(member :move :extend))

(export-always 'selection-direction)
(deftype selection-direction ()
  "The direction in which to adjust the current selection."
  '(member :forward :backward))

(export-always 'selection-scale)
(deftype selection-scale ()
  "The distance to adjust the current selection or cursor position."
  '(member :character :word :sentence :line :paragraph :lineboundary
    :sentenceboundary :paragraphboundary :documentboundary))

(export-always 'html-string-p)
(defun html-string-p (string)
  (serapeum:and-let*
      ((string string)
       (-p (stringp string))
       (trimmed (string-trim serapeum:whitespace string))
       (has-closing-tag (ppcre:scan "</\\w+>$" trimmed))
       (html (ignore-errors (plump:parse trimmed)))
       (single-child (serapeum:single (plump:children html)))
       (child (elt (plump:children html) 0)))
    (plump:element-p child)))

(export-always 'maybe)
(deftype maybe (&rest types)
  `(or null ,@types))

(export-always 'maybe*)
(deftype maybe* (&rest types)
  `(or null (array * (0)) ,@types))
