;;; package.lisp --- Definition of packages used by Next.

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
   #:resolved-uri
   #:requested-uri
   #:header
   #:file
   #:downloaded-bytes
   #:total-bytes
   #:progress
   #:cache
   #:resolve))

#+(or)
(defpackage :next/engine/http
  (:use :common-lisp)
  (:export
   #:
   #:locally-cache
   #:Downloads/))
