(in-package :nyxt)

;; trivial-types:proper-list doesn't check its element type.

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
         '(satisfies ,predicate)))))

(export-always 'list-of-symbols)
(define-list-type 'symbol)
(export-always 'list-of-characters)
(define-list-type 'character)
(export-always 'list-of-strings)
(define-list-type 'string)
(export-always 'list-of-keymaps)
(define-list-type 'keymap:keymap)
(export-always 'list-of-tags)
(define-list-type 'tag)
(export-always 'list-of-search-engines)
(define-list-type 'search-engine)

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
  `(satisfies alist-of-strings-p))

#+doctest
(progn
  (assert (not (typep '(("rst" . :rst)) 'alist-of-strings)))
  (assert (not (typep '((:rst . "rst")) 'alist-of-strings)))
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (not (typep '() 'alist-of-strings)))
  (assert (not (typep nil 'alist-of-strings))))

(deftype cookie-policy ()
  `(or (eql :always)
       (eql :never)
       (eql :no-third-party)))
