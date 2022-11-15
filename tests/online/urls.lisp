;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test parse-url (:tags :online)
  (let* ((*browser* (make-instance 'browser)))
    ;; "full URL"
    (assert-equality #'quri:uri=
                     (quri:uri "https://nyxt.atlas.engineer")
                     (url (make-instance 'nyxt:new-url-query :query "https://nyxt.atlas.engineer")))
    ;; "URL without protocol"
    (assert-equality #'quri:uri=
                     (quri:uri "https://nyxt.atlas.engineer")
                     (url (first (nyxt::input->queries "nyxt.atlas.engineer"))))
    ;; "search engine"
    (assert-equality #'quri:uri=
                     (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
                     (url (first (nyxt::input->queries "wiki wikipedia"))))
    ;; "search engine with special characters"
    (assert-equality #'quri:uri=
                     (quri:uri "https://en.wikipedia.org/w/index.php?search=wiki%2Bpédia")
                     (url (first (nyxt::input->queries "wiki wiki+pédia"))))
    ;; "default search engine"
    (assert-equality #'quri:uri=
                     (quri:uri "https://duckduckgo.com/?q=nyxt browser")
                     (url (first (nyxt::input->queries "nyxt browser"))))
    ;; "wiki search engine"
    (assert-equality #'quri:uri=
                     (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
                     (url (first (nyxt::input->queries "wiki wikipedia"))))
    ;; "local file"
    (assert-equality #'quri:uri=
                     (quri:uri "file:///readme.org")
                     (url (first (nyxt::input->queries "file:///readme.org"))))
    ;; "empty domain"
    (assert-equality #'quri:uri=
                     (quri:uri "https://duckduckgo.com/?q=foo")
                     (url (first (nyxt::input->queries "foo"))))
    ;; "same domain and TLD"
    (assert-equality #'quri:uri=
                     (quri:uri "https://duckduckgo.com/?q=algo")
                     (url (first (nyxt::input->queries "algo"))))
    ;; "localhost"
    (assert-equality #'quri:uri=
                     (quri:uri "http://localhost:8080")
                     (url (first (nyxt::input->queries "http://localhost:8080"))))
    ;; "ignore wildcards"
    (assert-equality #'quri:uri=
                     (quri:uri "https://duckduckgo.com/?q=*spurious*")
                     (url (first (nyxt::input->queries "*spurious*"))))
    ;; "about:blank"
    (assert-equality #'quri:uri=
                     (quri:uri "about:blank")
                     (url (first (nyxt::input->queries "about:blank"))))
    ;; "valid syntax but unknown scheme"
    (assert-equality #'quri:uri=
                     (quri:uri "https://duckduckgo.com/?q=foo:blank")
                     (url (first (nyxt::input->queries "foo:blank"))))))

(define-test url-processing (:tags :online)
  ;; "Invalid URL (empty host)"
  (assert-false (valid-url-p "http://foo"))
  ;; "Invalid URL (TLD == host)"
  (assert-false (valid-url-p "http://algo"))
  ;; "Valid URL"
  (assert-no-error t (valid-url-p "http://example.org/foo/bar?query=baz#qux"))
  ;; "Valid IP URL"
  (assert-no-error t (valid-url-p "http://192.168.1.1"))
  ;;"Valid IP URL with path"
  (assert-no-error t (valid-url-p "http://192.168.1.1/foo"))
  ;; "same schemeless URLs"
  (assert-true (nyxt::url-equal (quri:uri "http://example.org")
                                (quri:uri "https://example.org/")))
  ;; "different schemeless URLs"
  (assert-false (nyxt::url-equal (quri:uri "https://example.org")
                                 (quri:uri "https://example.org/foo")))
  ;; "schemeless URL"
  (assert-string= (nyxt::schemeless-url
                   (quri:uri "http://example.org/foo/bar?query=baz#qux"))
                  "example.org/foo/bar?query=baz#qux")
  ;; "comparing same URL"
  (assert-false (nyxt::url< (quri:uri "http://example.org")
                            (quri:uri "http://example.org")))
  ;; "comparing same URL but for trailing slash"
  (assert-false (nyxt::url< (quri:uri "http://example.org")
                            (quri:uri "http://example.org/")))
  ;; "comparing same URL but for scheme"
  (assert-false (nyxt::url< (quri:uri "https://example.org")
                            (quri:uri "http://example.org")))
  ;; "comparing same URL but for scheme and trailing slash"
  (assert-false (nyxt::url< (quri:uri "https://example.org")
                            (quri:uri "http://example.org/")))
  ;; "comparing different URLs (HTTPS first)"
  (assert-true (nyxt::url< (quri:uri "https://example.org/a")
                           (quri:uri "http://example.org/b")))
  ;; "comparing different URLs (HTTP first)"
  (assert-true (nyxt::url< (quri:uri "http://example.org/a")
                           (quri:uri "https://example.org/b"))))
