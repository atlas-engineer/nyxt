(in-package :cl-user)

;; TODO: Use cl-annot or similar to export at definition.
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
   #:bytes-fetched
   #:bytes-total
   #:progress
   #:finished-p
   #:last-update-speed
   #:cache
   #:resolve))
