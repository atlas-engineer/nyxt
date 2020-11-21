;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :named-readtables)

(defreadtable ospama::scheme-reader-syntax
  (:merge :standard)
  ;; TODO: While Scheme is case sensitive, preserving the case would mean we'd
  ;; have to upcase all return value symbols.  Or is there a smarter way to "do
  ;; what I mean"?
  ;; (:case :preserve)
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (read-delimited-list #\] stream)))
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\t #'(lambda (stream char1 char2)
                                    (declare (ignore stream char1 char2))
                                    T))
  (:dispatch-macro-char #\# #\f #'(lambda (stream char1 char2)
                                    (declare (ignore stream char1 char2))
                                    NIL))
  ;; `#:foo' is not a keyword in Common Lisp, read it as `:foo'.
  (:dispatch-macro-char #\# #\: #'(lambda (stream char1 char2)
                                    (declare (ignore char1 char2))
                                    (intern (string-upcase (string (read stream))) "KEYWORD"))))

(defreadtable ospama::scheme-writer-syntax
  (:merge :standard)
  ;; (:case :preserve)
  (:macro-char #\[ #'(lambda (stream char)
                       (declare (ignore char))
                       (read-delimited-list #\] stream)))
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\t #'(lambda (stream char1 char2)
                                    (declare (ignore stream char1 char2))
                                    'ospama::\#t))
  (:dispatch-macro-char #\# #\f #'(lambda (stream char1 char2)
                                    (declare (ignore stream char1 char2))
                                    'ospama::\#f))
  ;; SBCL seems OK without special #\: treatment, but not CCL.
  ;; uninterning does not work as it would break with:
  ;;   (let ((location 'foo)) (list #:location location))
  #+ccl
  (:dispatch-macro-char #\# #\: #'(lambda (stream char1 char2)
                                    (declare (ignore char1 char2))
                                    ;; (make-instance 'scheme-keyword :sym )
                                    (let ((s (intern (string-upcase (string (read stream))))))
                                      (unintern s)
                                      s))))
