(in-package :cl-user)

(defpackage :download-manager
  (:use :common-lisp)
  (:export
   #:*notifications*
   #:default-download-directory
   #:download
   #:resolved-uri
   #:requested-uri
   #:header
   #:file
   #:bytes-fetched
   #:bytes-total
   #:progress
   #:finished-p
   #:last-update-speed
   #:cache
   #:resolve))
