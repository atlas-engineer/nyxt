;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test search-engine-urls ()
  (let ((ddg (make-instance 'ddg-search-engine))
        (wiki (make-instance 'wikipedia-search-engine)))
    (assert-string= "https://duckduckgo.com/?q=hello%20world"
                    (nyxt::format-url "hello world" ddg))
    (assert-string= "ddg hello world"
                    (nyxt::format-query "hello world" ddg))
    (assert-string= "https://en.wikipedia.org/w/index.php?search=nyxt"
                    (nyxt::format-url "nyxt" wiki))
    (assert-string= "wiki nyxt"
                    (nyxt::format-query "nyxt" wiki))
    (assert-string= "DuckDuckGo" (name ddg))
    (assert-string= "ddg" (shortcut ddg))
    (assert-string= "Wikipedia" (name wiki))
    (assert-string= "wiki" (shortcut wiki))))
