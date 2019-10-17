;;; types.lisp --- types used in Next

(in-package :next)

(export '(list-of-strings
          alist-of-strings))

(defun list-of-strings-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'stringp list)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

#+doctest
(progn
  (assert (typep '("foo") 'list-of-strings))
  (assert (not (typep '() 'list-of-strings)))
  (assert (not (typep '("foo" ("bar")) 'list-of-strings))))

(defun alist-of-strings-p (alist)
  "Return t if ALIST is an alist whose keys and values are strings."
  (and (consp alist)
       ;; trivial types accepts nil.
       (trivial-types:association-list-p alist)
       (every (lambda (it)
                (and (stringp (car it))
                     (stringp (cdr it))))
              alist)))

(deftype alist-of-strings ()
  `(satisfies alist-of-strings-p))

#+doctest
(progn
  (assert (not (typep '(("rst" . :rst)) 'alist-of-strings)))
  (assert (not (typep '((:rst . "rst")) 'alist-of-strings)))
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (not (typep '() 'alist-of-strings)))
  (assert (not (typep nil 'alist-of-strings))))
