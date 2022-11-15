;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(defpackage :class-star
  (:use :cl)
  (:import-from :hu.dwim.defclass-star)
  (:export #:define-class
           #:name-identity
           #:*initform-inference*
           #:type-zero-initform-inference
           #:no-unbound-initform-inference
           #:nil-fallback-initform-inference
           #:*type-inference*
           #:basic-type-inference
           #:*predicate-name-transformer*
           #:*export-predicate-name-p*))
