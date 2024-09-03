;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test parse-set-url-input ()
  (let ((*browser* (make-instance 'browser)))
    (flet ((make-data (data) (make-instance 'nyxt:url-or-query :data data)))
      (assert-equality #'quri:uri=
                       (quri:uri "https://github.com/atlas-engineer")
                       (url (make-data "github.com/atlas-engineer")))
      ;; Fails when omitting http://, acceptable?
      (assert-equality #'quri:uri=
                       (quri:uri "http://localhost:8080")
                       (url (make-data "http://localhost:8080")))
      (assert-equality #'quri:uri=
                       (quri:uri "https://127.0.0.1")
                       (url (make-data "127.0.0.1")))
      (assert-equality #'quri:uri=
                       (quri:uri "about:blank")
                       (url (make-data "about:blank")))
      (assert-equality #'quri:uri=
                       (quri:uri "nyxt:new")
                       (url (make-data "nyxt:new")))
      (assert-eq :search-query
                 (kind (make-data "foo:blank")))
      (assert-equality #'quri:uri=
                       (quri:uri "file:///readme.org")
                       (url (make-data "file:///readme.org")))
      (assert-eq :url
                 (kind (make-data (namestring
                                   (asdf:system-relative-pathname :nyxt
                                                                  "source/browser.lisp")))))
      (assert-equality #'quri:uri=
                       (quri:uri "https://en.wikipedia.org/w/index.php?search=foo")
                       (url (make-data "wiki foo")))
      (let ((data1 (make-data "ddg foo"))
            (data2 (make-data "foo")))
        (with-slots ((d1 data) (k1 kind) (e1 search-engine) (q1 search-query)) data1
          (with-slots ((d2 data) (k2 kind) (e2 search-engine) (q2 search-query)) data2
            (assert-equal d1 d2)
            (assert-equal k1 k2)
            (assert-equal e1 e2)
            (assert-equal q1 q2)
            (assert-equality #'quri:uri= (url data1) (url data2)))))
      ;; When engine doesn't compute suggestions, return the identity query.
      (let* ((data-query (make-data "searx foo"))
             (suggestions (search-suggestions data-query)))
        (assert-equal 1 (length suggestions))
        (assert-equal (data data-query) (data (first suggestions)))))))

(define-test nyxt-urls ()
  (assert-error 'simple-error
                (nyxt-url 'undefined-nyxt-command :param1 "foo" :param2 "bar"))
  (assert-false (internal-page-name "foo:new"))
  (assert-false (internal-page-name "foo://new"))
  (assert-equal 'new
                (internal-page-name "nyxt:new"))
  (assert-equal 'new
                (internal-page-name "nyxt://new")))

(define-test url-processing ()
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
