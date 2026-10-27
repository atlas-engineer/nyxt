;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test nyxt-urls ()
  (assert-error 'simple-error
                (nyxt-url 'undefined-nyxt-command :param1 "foo" :param2 "bar"))
  (assert-false (internal-page-name "foo:new"))
  (assert-false (internal-page-name "foo://new"))
  (assert-equal 'new
                (internal-page-name "nyxt:new"))
  (assert-equal 'new
                (internal-page-name "nyxt://new"))
  (assert-eq 'nyxt/mode/buffer-listing:list-buffers
             (internal-page-name "nyxt://nyxt/mode/buffer-listing:list-buffers"))
  ;; A standard scheme resolves `nyxt:name' against the current page.
  ;; `nyxt-url' emits an absolute URL so the link target stays the page.
  (assert-string= "nyxt://new"
                  (nyxt-url 'new))
  (assert-eq 'new
             (internal-page-name "nyxt://new/"))
  (assert-string= "nyxt://page/nyxt/mode/history:list-history"
                  (nyxt-url 'nyxt/mode/history:list-history))
  (assert-eq 'nyxt/mode/history:list-history
             (internal-page-name (nyxt-url 'nyxt/mode/history:list-history)))
  (assert-string= "nyxt://describe-value"
                  (nyxt-url 'nyxt::describe-value))
  (assert-eq 'nyxt::describe-value
             (internal-page-name (nyxt-url 'nyxt::describe-value)))
  (assert-string= "nyxt://page/nyxt/mode/document:show-url-qrcode"
                  (nyxt-url 'nyxt/mode/document:show-url-qrcode))
  (assert-eq 'nyxt/mode/document:show-url-qrcode
             (internal-page-name (nyxt-url 'nyxt/mode/document:show-url-qrcode)))
  (assert-eq 'nyxt/mode/document:show-url-qrcode
             (internal-page-name "nyxt://page/nyxt/mode/document::show-url-qrcode"))
  (assert-eq 'describe-function
             (internal-page-name (nyxt-url 'describe-function :fn 'new)))
  (assert-false
   (internal-page-name "nyxt://new/nyxt/mode/history:list-history"))
  (assert-string= "nyxt:new"
                  (nyxt::format-internal-tab-display-text (quri:uri "nyxt://new/")))
  (assert-string= "nyxt:nyxt/mode/history:list-history"
                  (nyxt::format-internal-tab-display-text
                   (quri:uri "nyxt://page/nyxt/mode/history:list-history")))
  ;; A standard-scheme parser turns lisp:897 into host 0.0.3.129.
  (assert-string= "897"
                  (nyxt::lisp-request-id
                   (quri:uri "lisp://0.0.3.129/?title=nbutton%20Set-URL&buffer=1")))
  (assert-string= "897"
                  (nyxt::lisp-request-id
                   (quri:uri "lisp:897?title=nbutton%20Set-URL&buffer=1")))
  ;; `lisp-url' emits a standard URL. The id stays the path.
  (assert-string= "897"
                  (nyxt::lisp-request-id
                   (quri:uri "lisp://callback/897?title=nbutton%20Set-URL&buffer=1"))))

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
