;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

;; TODO Add test checking the DOM state after adding spans.
;; Test for the case when elem-beg lies deeper than elem-end.

;; invariant: matches are collected from the back

(define-internal-page test-search-buffer (&key)
    (:title "*Search buffer test*")
  "TODO"
  (spinneret:with-html-string (:body (:p "re" (:i "su" (:b "lt"))))))

;; TODO rethink tests.
(define-test immutable-dom-after-search ()
  (labels ((get-page-body ()
             "TODO"
             (let ((dom (plump:parse (ffi-buffer-get-document (current-buffer)))))
               (plump:serialize (alex:first-elt (clss:select "body" dom)) nil)))
           (body-after-search (pattern)
             "TODO"
             (reload-buffer (current-buffer))
             (sleep 0.1)
             (nyxt/search-buffer-mode:search-document
              pattern
              :buffer (current-buffer)
              :node (alex:first-elt (clss:select "body"
                                      (document-model (current-buffer))))
              :hint-p t)
             (sleep 0.1)
             (nyxt/search-buffer-mode:remove-search-hints)
             (sleep 0.1)
             (get-page-body)))
    (nyxt:start :no-config t :no-auto-config t :headless t
                :socket "/tmp/nyxt-test.socket" :profile "test")
    (buffer-load-internal-page-focus 'test-search-buffer)
    (sleep 0.5)
    (let ((expected-body (get-page-body)))
      (assert-string= expected-body
                      (body-after-search "l"))
      (assert-string= expected-body
                      (body-after-search "ul"))
      (assert-string= expected-body
                      (body-after-search "sul"))
      (assert-string= expected-body
                      (body-after-search "esul"))
      (assert-string= expected-body
                      (body-after-search "result")))
    (nyxt:quit)))
