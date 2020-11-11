;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :text-analysis)

(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t) (alphabeticp t))
  "Split a string into a list of words."
  (let* ((alpha-scanner (cl-ppcre:create-scanner "^[A-Za-z]*$"))
         (tokens (str:split " " (str:collapse-whitespaces string)))
         (tokens (if remove-stop-words
                     (delete-if (lambda (x) (gethash (string-downcase  x) (stop-words-lookup *language-data*))) tokens)
                     tokens))
         (tokens (if stem
                     (mapcar #'stem tokens)
                     tokens))
         (tokens (if down-case
                     (mapcar #'string-downcase tokens)
                     tokens))
         (tokens (if alphabeticp
                     (delete-if-not (lambda (x) (cl-ppcre:scan alpha-scanner x)) tokens)
                     tokens)))
    tokens))

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  (remove "" (mapcar #'str:trim (cl-ppcre:split "\\.|\\?|\\!" string)) :test #'equal))
