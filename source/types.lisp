;;; types.lisp --- types used in Next

(in-package :next)

(export '(list-of-strings
          list-of-characters
          alist-of-strings
          alist-of-3tuples-strings))

(defun list-of-type-p (list typep)
  "Return t if LIST contains only elements of the given TYPEP predicate (stringp ,integerp etc)."
  ;; trivial-types:proper-list doesn't check its element type.
  (and (consp list)
       (every typep list)))


#+doctest
(progn
  (assert (list-of-type-p '("me") 'stringp))
  (assert (not (list-of-type-p '(:foo) 'stringp))))

(defun list-of-strings-p (list)
  "Return t if LIST is non-nil and contains only strings."
  (list-of-type-p list 'stringp))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))


(defun list-of-characters-p (list)
  "Return t if LIST is non-nil and contains only characters."
  (list-of-type-p list 'characterp))

(deftype list-of-characters ()
  `(satisfies list-of-characters-p))

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
  (assert (typep '(("rst" . "rst")) 'alist-of-strings))
  (assert (not (typep '() 'alist-of-strings)))
  (assert (not (typep nil 'alist-of-strings))))


(defun alist-of-3tuples-strings-p (alist)
  "Return t if ALIST is an association list composed of 3-tuples, made only of strings."
  (and (trivial-types:association-list-p alist)
       (every (lambda (it)
                (and
                 (= 3 (length it))
                 (every #'stringp it)))
              alist)))

(deftype alist-of-3tuples-strings ()
  `(satisfies alist-of-3tuples-strings-p))

#+doctest
(progn
  (assert (typep '(("default" "https://duckduckgo.com/?q=~a" "https://duckduckgo.com/")
                   ("wiki" "https://en.wikipedia.org/w/index.php?search=~a"
                    "https://en.wikipedia.org/"))
                 'alist-of-3tuples-strings))
  (assert (not (typep '(("default" "two" "three" "four")
                        ("wiki" "https://en.wikipedia.org/w/index.php?search=~a"
                         "https://en.wikipedia.org/"))
                      'alist-of-3tuples-strings)))
  (assert (not (typep '(("default" "two" "three")
                        ("wiki" :foo "https://en.wikipedia.org/"))
                      'alist-of-3tuples-strings))))
