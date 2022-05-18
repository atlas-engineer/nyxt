;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package download-manager/tests
  (:use #:common-lisp #:prove)
  (:import-from #:download-manager))
(in-package :download-manager/tests)

(plan nil)

(subtest "Simple HTTP/HTTPS downloads"
  (let ((urls '("https://abcl.org"
                "http://en.wikipedia.org/wiki/Main_Page"
                "https://duckduckgo.com"
                "https://nyxt.atlas.engineer")))
    (dolist (url urls)
      (ok
       (download-manager:resolve (quri:uri url))
       (format nil "Able to download <~a>~%" url)))))

(finalize)
