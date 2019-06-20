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
   #:bytes-fetched
   #:bytes-total
   #:progress
   #:finished-p
   #:cache
   #:resolve))
