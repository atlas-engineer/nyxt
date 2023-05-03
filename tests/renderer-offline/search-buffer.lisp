;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(defvar *query* "match"
  "Default query pattern for search tests.")

(define-internal-page test-search-buffer (&key)
    (:title "*Search buffer test*")
  (let ((spinneret:*suppress-inserted-spaces* t))
    ;; Based on nyxt/tests/renderer-offline/search-buffer.lisp
    (spinneret:with-html-string (:body
                                 ;; non matches
                                 (:p "m") (:b "foo") (:i "atch")
                                 (:p "m" (:b "foo" (:i "atch")))
                                 ;; excluded nodes
                                 (:comment *query*) (:style *query*)
                                 (:script *query*) (:noscript  *query*)
                                 ;; matches
                                 (:p "foo match bar match baz")
                                 (:p (:b "foo") "match" (:b "bar") (:b "match"))
                                 (:p "m") (:b "a") (:i "t") (:b "c") (:i "h")
                                 (:p "foo") (:b "mat") (:i "ch") (:b "bar")
                                 (:p "m") (:b "") (:b "") (:b "atch")
                                 (:p "ma" (:b "tc" (:i "h")) "bar")
                                 (:p "foo" (:b "bar" (:i "ma") "tc") "h")
                                 (:p "m" (:b "" (:i "" (:b "atch"))))))))

(define-test search-buffer ()
  (nyxt:start :no-config t :no-auto-config t :headless t
              :socket "/tmp/nyxt-test.socket" :profile "test")
  (labels ((body (&optional (buffer (current-buffer)))
             ;; The following would be easier, but it's not the same since the
             ;; renderer inserts a CSS zoom rule in the DOM.
             ;; (plump:serialize (elt (clss:select "body" (document-model buffer)) 0)
             ;;                  nil)
             (let ((dom (plump:parse (ffi-buffer-get-document buffer))))
               (plump:serialize (alex:first-elt (clss:select "body" dom)) nil)))
           (count-matches (&optional (buffer (current-buffer)))
             ;; Multiple spans may correspond to a single match.
             (let ((matches (clss:select "span[nyxt-search-mark]"
                              (document-model buffer))))
               (parse-integer (plump:attribute (alex:last-elt matches)
                                               "nyxt-search-mark"))))
           (assert-match-count (pattern expected-count
                                &optional (buffer (current-buffer)))
             (reload-buffer buffer)
             (sleep 0.1)
             (nyxt/mode/search-buffer:search-document
              pattern
              :buffer buffer
              :node (elt (clss:select "body" (document-model buffer)) 0)
              :mark-p t)
             (sleep 0.1)
             (assert= expected-count (count-matches buffer)))
           (assert-dom-immutability (expected-dom-body
                                     &optional (buffer (current-buffer)))
             (nyxt/mode/search-buffer:remove-search-marks)
             (sleep 0.1)
             (assert-string= expected-dom-body (body buffer))))
    (buffer-load-internal-page-focus 'test-search-buffer)
    ;; Allow enough time to load the internal page.
    (sleep 0.5)
    (let ((initial-dom-body (body)))
      (assert-match-count "h" 12)
      (assert-dom-immutability initial-dom-body)
      (assert-match-count "ch" 12)
      (assert-dom-immutability initial-dom-body)
      (assert-match-count "tch" 12)
      (assert-dom-immutability initial-dom-body)
      (assert-match-count "atch" 12)
      (assert-dom-immutability initial-dom-body)
      (assert-match-count "match" 10)
      (assert-dom-immutability initial-dom-body)))
  (nyxt:quit))
