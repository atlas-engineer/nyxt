(in-package :cl-user)

(defpackage :download-manager
  (:use :common-lisp)
  (:export
   #:notifications
   #:download
   #:resolved-uri
   #:requested-uri
   #:header
   #:file
   #:downloaded-bytes
   #:total-bytes
   #:progress
   #:finished-p
   #:cache
   #:resolve))
