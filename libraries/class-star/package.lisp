(in-package :cl-user)

(defpackage :class-star
  (:nicknames class*)
  (:use :common-lisp)
  (:import-from :hu.dwim.defclass-star)
  (:export #:define-class
           #:name-identity))
