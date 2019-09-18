(in-package :cl-user)

(defpackage :ring
  (:use :common-lisp)
  (:export
   #:ring
   #:insert
   #:delete-index
   #:index-if
   #:recent-list
   #:ref
   #:copy
   #:make
   #:pop-most-recent
   #:item-count))
