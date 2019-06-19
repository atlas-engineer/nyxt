(in-package :cl-user)

(defpackage :download-manager
  (:use :common-lisp)
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
