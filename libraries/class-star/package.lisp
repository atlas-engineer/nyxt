(in-package :cl-user)

(defpackage :class-star
  (:nicknames class*)
  (:use :common-lisp)
  (:import-from :hu.dwim.defclass-star)
  (:export #:with-class
           #:replace-class
           #:original-class
           #:define-class
           #:name-identity
           #:*type-zero-function*
           #:default-type-zero-function
           #:no-unbound-type-zero-function
           #:nil-fallback-type-zero-function))
