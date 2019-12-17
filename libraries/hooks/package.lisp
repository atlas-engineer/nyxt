(in-package :cl-user)

(defpackage :next-hooks
  (:use :common-lisp)
  (:export
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook
   #:run-hook-with-args
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
   #:handler
   #:name
   #:description
   #:handler-type
   #:place
   #:value
   #:make-handler
   #:equals
   #:hook
   #:default-combine-hook
   #:combine-hook-until-failure
   #:combine-hook-until-success
   #:combine-composed-hook
   #:remove-hook
   #:disable-hook
   #:enable-hook
   #:define-hook
   #:find-hook
   #:define-hook-type
   ;; Pre-generated types
   ;; TODO: Add the rest.
   #:make-handler-string->string
   #:handler-string->string
   #:hook-string->string
   #:make-handler-void
   #:handler-void
   #:hook-void))
