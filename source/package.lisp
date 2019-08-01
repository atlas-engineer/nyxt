;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

(defpackage next
  (:use :common-lisp)
  (:use :trivia)
  (:export
   #:start
   #:entry-point
   #:fuzzy-match
   #:open-file))
