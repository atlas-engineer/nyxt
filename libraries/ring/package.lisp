(in-package :cl-user)

(defpackage :ring
  (:use :common-lisp)
  (:export
   #:ring
   #:insert
   #:recent-list
   #:ref
   #:copy
   #:make
   #:pop-most-recent
   #:item-count))

