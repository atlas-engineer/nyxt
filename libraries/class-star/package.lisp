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
           #:*initform-inference*
           #:type-zero-initform-inference
           #:no-unbound-initform-inference
           #:nil-fallback-initform-inference
           #:*type-inference*
           #:basic-type-inference))
