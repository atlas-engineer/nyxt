;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

(defpackage next
  (:use :common-lisp)
  (:export
   #:start
   #:start-with-port))
