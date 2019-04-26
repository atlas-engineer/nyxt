;;; package.lisp --- definition of package used by next

(in-package :cl-user)

(defpackage next
  (:use :common-lisp)
  (:export
   #:start
   #:start-with-port))

(defpackage :next/engine
  (:use :common-lisp)
  (:nicknames :next/engine/http)
  (:export
   #:cache
   #:resolve))

#+(or)
(defpackage :next/engine/http
  (:use :common-lisp)
  (:export
   #:
   #:locally-cache
   #:Downloads/))

   



