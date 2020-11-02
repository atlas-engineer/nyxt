;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(prove:plan nil)

(prove:subtest "Simple HTTP/HTTPS downloads"
  (let ((uris '("https://abcl.org"
                "http://en.wikipedia.org/wiki/Main_Page"
                "https://duckduckgo.com"
                "https://atlas.engineer")))
    (dolist (uri uris)
      (prove:ok
       (download-manager:resolve (quri:uri uri))
       (format nil "Able to download <~a>~%" uri)))))

(prove:finalize)
