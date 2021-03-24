;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package download-manager/tests
  (:use #:common-lisp #:prove)
  (:import-from #:download-manager))
(in-package :download-manager/tests)

(plan nil)

(subtest "Simple HTTP/HTTPS downloads"
  (let ((uris '("https://abcl.org"
                "http://en.wikipedia.org/wiki/Main_Page"
                "https://duckduckgo.com"
                "https://atlas.engineer")))
    (dolist (uri uris)
      (ok
       (download-manager:resolve (quri:uri uri))
       (format nil "Able to download <~a>~%" uri)))))

(finalize)
