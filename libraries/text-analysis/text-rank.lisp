(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defun sentence-tokenizer (string)
  "Split apart a string into a list of sentences."
  (mapcar #'str:trim (cl-ppcre:split "\\.|\\?|\\!" string)))
