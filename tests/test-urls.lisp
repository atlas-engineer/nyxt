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

(finalize)
