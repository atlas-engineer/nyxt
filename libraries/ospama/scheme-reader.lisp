;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :named-readtables)

(defreadtable ospama::scheme-syntax
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
  (:dispatch-macro-char #\# #\: #'(lambda (stream char1 char2)
                                    (declare (ignore char1 char2))
                                    (intern (string-upcase (string (read stream))) "KEYWORD"))))
