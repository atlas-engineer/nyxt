;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(defvar word-separator '(#\  #\Tab)
  "List of characters that are considered as word separators by
  `score-suggestion-string'.")

(defun score-suggestion-string (input suggestion-string)
  "Return a SUGGESTION's score for INPUT.
A higher score means the SUGGESTION-STRING comes first."
  (let ((i 0)
        (score 0.0)
        (lastchar #\ ))
    (declare (optimize (speed 3) (safety 0))
             (type single-float score) (type fixnum i) (type character lastchar)
             (type (simple-array character) input suggestion-string))
    (flet ((word-end ()
             ;; bonus for word ending early (shorter words at the beginning wins)
             (let ((next (position-if (lambda (c) (member c word-separator :test 'eq))
                                      suggestion-string :start i)))
               (unless next (setq next (length suggestion-string)))
               (incf score (/ 1.0 (+ 3.0 next)))
               (setq i 0))))
      ;; flex match, with higher weight for the beginning
      (loop for c across input do
        (if (member c word-separator :test 'eq)
            (word-end)
            (let ((next (position c suggestion-string :start i :test 'eq)))
              (if next
                  (progn
                    ;; bonus for continuous match
                    (when (and (> next 0)
                               (eq (aref suggestion-string (1- next)) lastchar))
                      (incf score 0.3))
                    (incf score (/ 1.0 (+ 3.0 next)))
                    (setq i next lastchar c))
                  (return)))))
      (when (> i 0) (word-end)))
    score))

(defvar score-threshold 0.0 ; TODO: Learn good value and enable low-score filtering.
  "The threshold under which suggestions are eleminated.")

(export-always 'score>)
(defun score> (suggestion1 suggestion2)
  "Suitable as a `source' `sort-predicate'."
  (> (score suggestion1)
     (score suggestion2)))

(export-always 'fuzzy-match)
(defun fuzzy-match (suggestion source input)
  "Score the SUGGESTION according to a fuzzy string distance to the INPUT."
  (setf (score suggestion)
        (score-suggestion-string input (ensure-match-data-string suggestion source)))
  suggestion)

(export-always 'submatches)
(defun submatches (suggestion source input)
  "Return SUGGESTION untouched if all INPUT strings are contained in it.

This is suitable as a prompter `filter'.
It probably makes little sense to use it together with the
`delete-inexact-matches' preprocessor."
  (let ((terms (delete-duplicates (str:split " " input :omit-nulls t)
                                  :test #'equal)))
    (when (funcall
           (apply #'alex:conjoin
                  (mapcar (lambda (term)
                            (lambda (suggestion-match-data)
                              (str:contains? term suggestion-match-data :ignore-case t)))
                          terms))
           (ensure-match-data-string suggestion source))
      suggestion)))
