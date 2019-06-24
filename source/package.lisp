;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

(defpackage next
  (:use :common-lisp)
  (:import-from :trivia :match)
  (:export
   #:start
   #:start-with-port))
