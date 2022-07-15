;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package download-manager/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:download-manager))
(in-package :download-manager/tests)

(define-test simple-http/https-downloads ()
  (let ((urls '("https://abcl.org"
                "http://en.wikipedia.org/wiki/Main_Page"
                "https://duckduckgo.com"
                "https://nyxt.atlas.engineer")))
    (dolist (url urls)
      (assert-no-error t
                       (download-manager:resolve (quri:uri url))))))
