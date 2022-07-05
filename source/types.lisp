;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; trivial-types:proper-list doesn't check its element type.

(export-always 'function-symbol)
(deftype function-symbol ()
  `(and symbol (satisfies fboundp)))

(defun list-of-type-p (list typep)
  "Return non-nil if LIST contains only elements of the given TYPEP predicate."
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
         '(and list (satisfies ,predicate))))))

(export-always 'list-of-symbols)
(define-list-type 'symbol)
(export-always 'list-of-characters)
(define-list-type 'character)
(export-always 'list-of-numbers)
(define-list-type 'number)
(export-always 'list-of-strings)
(define-list-type 'string)
(export-always 'list-of-keymaps)
(define-list-type 'keymaps:keymap)

(defun alist-of-strings-p (alist)
  "Return t if ALIST is an alist whose keys and values are strings."
  (and (consp alist)
       ;; trivial types accepts nil.
       (trivial-types:association-list-p alist)
       (every (lambda (it)
                (and (stringp (car it))
                     (stringp (cdr it))))
              alist)))

(export-always 'alist-of-strings)
(deftype alist-of-strings ()
  `(and list (satisfies alist-of-strings-p)))

#+doctest
(progn
  (assert (not (typep '(("rst" . :rst)) 'alist-of-strings)))
  (assert (not (typep '((:rst . "rst")) 'alist-of-strings)))
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (not (typep '() 'alist-of-strings)))
  (assert (not (typep nil 'alist-of-strings))))

(deftype cookie-policy ()
  `(member :always :never :no-third-party))

;; The following types represent the positional arguments documented at
;; https://developer.mozilla.org/en-US/docs/Web/API/Selection/modify#parameters

(deftype selection-action ()
  "The type of change to apply."
  '(member :move :extend))

(deftype selection-direction ()
  "The direction in which to adjust the current selection."
  '(member :forward :backward))

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
       (single-child (sera:single (plump:children html)))
       (child (elt (plump:children html) 0)))
    (plump:element-p child)))

(export-always 'maybe)
(deftype maybe (&rest types)
  `(or null ,@types))

(export-always 'maybe*)
(deftype maybe* (&rest types)
  `(or null (array * (0)) ,@types))
