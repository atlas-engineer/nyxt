;;; types.lisp --- types used in Next

(in-package :next)

(export '(list-of-strings))

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
