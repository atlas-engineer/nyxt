(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(setf *browser* (make-instance *browser-class*))

(subtest "Parse URL"
  (is (nyxt::parse-url "https://nyxt.atlas.engineer")
      (quri:uri "https://nyxt.atlas.engineer")
      :test #'quri:uri=
      "full URL")
  (is (nyxt::parse-url "nyxt.atlas.engineer")
      (quri:uri "https://nyxt.atlas.engineer")
      :test #'quri:uri=
      "URL without protocol")
  (is (nyxt::parse-url "wiki wikipedia")
      (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
      :test #'quri:uri=
      "search engine")
  (is (nyxt::parse-url "nyxt browser")
      (quri:uri "https://duckduckgo.com/?q=nyxt+browser")
      :test #'quri:uri=
      "default search engine")
  (is (nyxt::parse-url "wiki wikipedia")
      (quri:uri "https://en.wikipedia.org/w/index.php?search=wikipedia")
      :test #'quri:uri=
      "wiki search engine")
  (is (nyxt::parse-url "file:///readme.org")
      (quri:uri "file:///readme.org")
      :test #'quri:uri=
      "local file")
  (is (nyxt::parse-url "foo")
      (quri:uri "https://duckduckgo.com/?q=foo")
      :test #'quri:uri=
      "empty domain")
  (is (nyxt::parse-url "algo")
      (quri:uri "https://duckduckgo.com/?q=algo")
      :test #'quri:uri=
      "same domain and TLD")
  (is (nyxt::parse-url "*spurious*")
      (quri:uri "https://duckduckgo.com/?q=%2Aspurious%2A")
      :test #'quri:uri=
      "ignore wildcards"))

(subtest "URL processing"
  (is (valid-url-p "http://foo")
      nil
      "Invalid URL (empty domain)")
  (is (valid-url-p "http://algo")
      nil
      "Invalid URL (TLD == domain)")
  (is (valid-url-p "http://example.org/foo/bar?query=baz#qux")
      t
      "Valid URL")
  (is (nyxt::schemeless-uri= (quri:uri "http://example.org")
                             (quri:uri "https://example.org/"))
      t
      "same schemeless URIs")
  (is (nyxt::schemeless-uri= (quri:uri "https://example.org")
                             (quri:uri "https://example.org/foo"))
      nil
      "different schemeless URIs")
  (is (nyxt::schemeless-url (quri:uri "http://example.org/foo/bar?query=baz#qux"))
      "example.org/foo/bar?query=baz#qux"
      "schemeless URL")
  (is (nyxt::uri< (quri:uri "http://example.org")
                  (quri:uri "http://example.org"))
      nil
      "comparing same URL")
  (is (nyxt::uri< (quri:uri "http://example.org")
                  (quri:uri "http://example.org/"))
      nil
      "comparing same URL but for trailing slash")
  (is (nyxt::uri< (quri:uri "https://example.org")
                  (quri:uri "http://example.org"))
      nil
      "comparing same URL but for scheme")
  (is (nyxt::uri< (quri:uri "https://example.org")
                  (quri:uri "http://example.org/"))
      nil
      "comparing same URL but for scheme and trailing slash")
  (is (null (nyxt::uri< (quri:uri "https://example.org/a")
                        (quri:uri "http://example.org/b")))
      nil
      "comparing different URLs (HTTPS first)")
  (is (null (nyxt::uri< (quri:uri "http://example.org/a")
                        (quri:uri "https://example.org/b")))
      nil
      "comparing different URLs (HTTP first)"))

(finalize)
