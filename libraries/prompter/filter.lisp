;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(defvar word-separator '(#\  #\Tab)
  "List of characters that are considered as word separators by
  `score-suggestion-string'.")

(defvar start-bonus 0.3
  "Additional weight `score-suggestion-string' gives to beginning of
suggesting string.  Must be a positive single-float value.")

(defvar continuous-bonus 0.3
  "Bonus `score-suggestion-string' gives to continuous match.")

(defun score-suggestion-string (input suggestion-string)
  "Return a SUGGESTION's score for INPUT.
A higher score means the SUGGESTION-STRING comes first."
  (let ((i 0) (score 0.0) (lastchar #\ )
        (inverse-start-bonus (/ 1.0 start-bonus)))
    (declare (optimize (speed 3) (safety 0))
             (type single-float score) (type (or fixnum null) i)
             (type character lastchar)
             (type string input suggestion-string)
             ;; FIXME: Optimized arithmetic typing is hard to get right.  Can we fix it?
             #+sbcl
             (sb-ext:muffle-conditions sb-ext:compiler-note))
    (labels ((match-bonus (i)
               (incf score (/ 1.0 (+ inverse-start-bonus i))))
             (word-body (c)
               (let ((next (position c suggestion-string :start i)))
                 (setf i next)
                 (when (not next) (return-from word-body))
                 ;; bonus for continuous match
                 (when (and (> next 0)
                            (eq (aref (the (simple-array character) suggestion-string) (1- next))
                                lastchar))
                   (incf score continuous-bonus))
                 (match-bonus next)
                 (setf lastchar c)))
             (word-end ()
               ;; bonus for word ending early (shorter words at the beginning wins)
               (match-bonus
                (or (position-if (lambda (c) (member c word-separator))
                                 (the (simple-array character) suggestion-string) :start i)
                    (length suggestion-string)))
               (setf i 0)))
      ;; flex match, with higher weight for the beginning
      (loop for c across (the simple-array input) do
        (if (member c word-separator)
            (progn
              (when i (word-end))
              (setf i 0))
            (when i (word-body c))))
      (when (and i (> i 0)) (word-end)))
    score))

(defvar score-threshold 0.0 ; TODO: Learn good value and enable low-score filtering.
  "The threshold under which suggestions are eliminated.")

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
