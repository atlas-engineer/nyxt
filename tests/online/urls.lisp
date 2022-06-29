;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(plan nil)

(subtest "Parse URL"
  (let* ((*browser* (make-instance 'browser)))
    (is (url (make-instance 'nyxt:new-url-query :query "https://nyxt.atlas.engineer"))
        (quri:uri "https://nyxt.atlas.engineer")
        :test #'quri:uri=
        "full URL")
    (is (url (first (nyxt::input->queries "nyxt.atlas.engineer")))
        (quri:uri "https://nyxt.atlas.engineer")
        :test #'quri:uri=
        "URL without protocol")
    (is (url (first (nyxt::input->queries "wiki wikipedia")))
        (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
        :test #'quri:uri=
        "search engine")
    (is (url (first (nyxt::input->queries "wiki wiki+pédia")))
        (quri:uri "https://en.wikipedia.org/w/index.php?search=wiki%2Bpédia")
        :test #'quri:uri=
        "search engine with special characters")
    (is (url (first (nyxt::input->queries "nyxt browser")))
        (quri:uri "https://duckduckgo.com/?q=nyxt browser")
        :test #'quri:uri=
        "default search engine")
    (is (url (first (nyxt::input->queries "wiki wikipedia")))
        (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
        :test #'quri:uri=
        "wiki search engine")
    (is (url (first (nyxt::input->queries "file:///readme.org")))
        (quri:uri "file:///readme.org")
        :test #'quri:uri=
        "local file")
    (is (url (first (nyxt::input->queries "foo")))
        (quri:uri "https://duckduckgo.com/?q=foo")
        :test #'quri:uri=
        "empty domain")
    (is (url (first (nyxt::input->queries "algo")))
        (quri:uri "https://duckduckgo.com/?q=algo")
        :test #'quri:uri=
        "same domain and TLD")
    (is (url (first (nyxt::input->queries "http://localhost:8080")))
        (quri:uri "http://localhost:8080")
        :test #'quri:uri=
        "localhost")
    (is (url (first (nyxt::input->queries "*spurious*")))
        (quri:uri "https://duckduckgo.com/?q=*spurious*")
        :test #'quri:uri=
        "ignore wildcards")
    (is (url (first (nyxt::input->queries "about:blank")))
        (quri:uri "about:blank")
        :test #'quri:uri=
        "about:blank")
    (is (url (first (nyxt::input->queries "foo:blank")))
        (quri:uri "https://duckduckgo.com/?q=foo:blank")
        :test #'quri:uri=
        "valid syntax but unknown scheme")))

(subtest "URL processing"
  (is (valid-url-p "http://foo")
      nil
      "Invalid URL (empty host)")
  (is (valid-url-p "http://algo")
      nil
      "Invalid URL (TLD == host)")
  (ok (valid-url-p "http://example.org/foo/bar?query=baz#qux")
      "Valid URL")
  (ok (valid-url-p "http://192.168.1.1")
      "Valid IP URL")
  (ok (valid-url-p "http://192.168.1.1/foo")
      "Valid IP URL with path")
  (is (nyxt::url-equal (quri:uri "http://example.org")
                       (quri:uri "https://example.org/"))
      t
      "same schemeless URLs")
  (is (nyxt::url-equal (quri:uri "https://example.org")
                       (quri:uri "https://example.org/foo"))
      nil
      "different schemeless URLs")
  (is (nyxt::schemeless-url (quri:uri "http://example.org/foo/bar?query=baz#qux"))
      "example.org/foo/bar?query=baz#qux"
      "schemeless URL")
  (is (nyxt::url< (quri:uri "http://example.org")
                  (quri:uri "http://example.org"))
      nil
      "comparing same URL")
  (is (nyxt::url< (quri:uri "http://example.org")
                  (quri:uri "http://example.org/"))
      nil
      "comparing same URL but for trailing slash")
  (is (nyxt::url< (quri:uri "https://example.org")
                  (quri:uri "http://example.org"))
      nil
      "comparing same URL but for scheme")
  (is (nyxt::url< (quri:uri "https://example.org")
                  (quri:uri "http://example.org/"))
      nil
      "comparing same URL but for scheme and trailing slash")
  (isnt (nyxt::url< (quri:uri "https://example.org/a")
                    (quri:uri "http://example.org/b"))
        t
      "comparing different URLs (HTTPS first)")
  (isnt (nyxt::url< (quri:uri "http://example.org/a")
                    (quri:uri "https://example.org/b"))
        t
      "comparing different URLs (HTTP first)"))

(finalize)
