;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test named-html-parse-parents-and-body ()
  (let* ((root (nyxt/dom:named-html-parse
                "<html><body><p>  hello   world </p></body></html>"))
         (paragraph (elt (clss:select "p" root) 0)))
    (assert-true (typep paragraph 'nyxt/dom:p-element))
    (assert-string= "hello world" (nyxt/dom:body paragraph))
    (assert-true (typep (first (nyxt/dom:parents paragraph)) 'nyxt/dom:body-element))))

(define-test label-of-uses-for-attribute ()
  (let* ((root (nyxt/dom:named-html-parse
                "<label for=\"q\">Query</label><input name=\"q\">"))
         (input (elt (clss:select "input" root) 0)))
    (assert-true (typep input 'nyxt/dom:input-element))
    (assert-string= "Query" (nyxt/dom::label-of input))
    (assert-string= "Query" (nyxt/dom:body input))))
