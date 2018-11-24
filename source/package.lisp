;;; package.lisp --- definition of package used by next

(in-package :cl-user)

(defpackage :next
  (:use :common-lisp)
  (:export
   :start
   :*character-conversion-table*))

(defpackage :port
  (:use :common-lisp :next)
  (:export
   :set-conversion-table
   :run-loop
   :run-program))
