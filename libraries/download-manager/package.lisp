(in-package :cl-user)

(defpackage :download-manager
  (:use :common-lisp)
  (:export
   #:init
   #:*notifications*
   #:default-download-directory
   #:download
   #:resolved-uri
   #:requested-uri
   #:header
   #:file
   #:filename
   #:bytes-fetched
   #:bytes-total
   #:progress
   #:finished-p
   #:last-update-speed
   #:cache
   #:resolve))
