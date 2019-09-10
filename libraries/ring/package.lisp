(in-package :cl-user)

(defpackage :ring
  (:use :common-lisp)
  (:export
   #:ring-insert
   #:ring-recent-list
   #:ring-insert-clipboard
   #:ring-ref
   #:make-ring))
