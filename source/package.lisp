;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package next
    (:use :common-lisp :trivia :annot.class))

(uiop:define-package next-user
    (:use :common-lisp :trivia :next)
  (:documentation "
Package left for the user to fiddgle with.
It's recommended to use this package in the Next configuration file, instead of
`next' itself."))
