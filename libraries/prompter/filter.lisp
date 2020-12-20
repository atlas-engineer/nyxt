;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(defun substring-norm (substrings string &key (substring-length 2))
  "Return the norm of SUBSTRINGS with regard to STRING.
The norm is closer to 1 if
- substrings start near the beginning of STRING;
- substrings length are closer to the length of STRING.

Only substrings of SUBSTRING-LENGTH characters or more are considered."
  ;; TODO: Remove duplicates in SUBSTRINGS?  Repeats could mean we insist more on it.
  (let ((position-factor 1.0)
        (length-factor 1.0)
        (long-substrings (remove-if (lambda (s) (> substring-length (length s)))
                                    substrings)))
    (if long-substrings
        (/ (apply #'+
                  (mapcar (lambda (s)
                            (let ((position (search s string)))
                              (if (not position)
                                  0
                                  (/ (+
                                      (* position-factor
                                         (/ 1
                                            ;; We use the sqrt to slow down the
                                            ;; decrease rate, we want the a
                                            ;; position of 10-15 still be >0.1.
                                            (sqrt (1+ position))))
                                      (* length-factor
                                         (/ (min (length s) (length string))
                                            (length string))))
                                     (+ position-factor length-factor)))))
                          long-substrings))
           (length long-substrings))
        0)))

(defun to-unicode (input)
  "Convert INPUT to (simple-array character) type."
  (if (typep input 'base-string)
      (coerce input `(simple-array character (,(length input))))
      input))

(defun score-suggestion-string (input suggestion-string)
  "Return a SUGGESTION's score for INPUT.
A higher score means the suggestion-string comes first."
  ;; The Jaccard metric seems to provide much better results than, say,
  ;; Damerau-Levensthein but it's much slower.
  ;; TODO: Check out fzf for a possibly good scoring algorithm.
  (+ (* 1.0 (mk-string-metrics:norm-damerau-levenshtein suggestion-string input))
     (* 1.0 (substring-norm (str:split " " input) suggestion-string))))

(defvar score-threshold 0.0             ; TODO: Learn good value and enable low-score filtering.
  "The threshold under which suggestions are eleminated.")

(export-always 'score>)
(defun score> (suggestion1 suggestion2)
  "Suitable as a `prompter-source' `sort-predicate'."
  (> (score suggestion1)
     (score suggestion2)))

(export-always 'fuzzy-match)
(defun fuzzy-match (input suggestion)
  "Score the SUGGESTION according to a fuzzy string distance to the INPUT."
  (setf input (to-unicode input))
  (setf (match-data suggestion) (to-unicode (match-data suggestion)))
  (setf (score suggestion)
        (score-suggestion-string input (match-data suggestion)))
  suggestion)
