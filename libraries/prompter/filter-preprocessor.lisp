;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(defun find-exactly-matching-substrings (input suggestion-match-data
                                         &key (substring-length 2))
  "Return the list of input substrings that match at least one suggestion.
The substrings must be SUBSTRING-LENGTH characters long or more."
  (let ((input-strings (delete-if (lambda (s) (< (length s) substring-length))
                                  (str:split " " input :omit-nulls t))))
    (when input-strings
      (delete-duplicates
       (loop for match-datum in suggestion-match-data
             append (remove-if
                     (lambda (i)
                       (not (search i match-datum)))
                     input-strings))
       :test #'string=))))

;; TODO: Disambiguate naming of `delete-inexact-matches' and `filter-exact-matches'?

(export-always 'delete-inexact-matches)
(defun delete-inexact-matches (suggestions source input)
  "Destructively filter out non-exact matches from SUGGESTIONS.
Return the resulting list.
If any input substring matches exactly (but not necessarily a whole word),
then all suggestions that are not exactly matched by at least one substring are removed.

Suitable as a `source' `filter-preprocessor'."
  (unless (str:empty? input)
    (let ((exactly-matching-substrings (find-exactly-matching-substrings
                                        input
                                        (mapcar (lambda (suggestion)
                                                  (ensure-match-data-string suggestion source))
                                                suggestions))))
      (when exactly-matching-substrings
        (setf suggestions
              (delete-if (lambda (suggestion)
                           (not (loop for i in exactly-matching-substrings
                                      always (search i (match-data suggestion)))))
                         suggestions)))))
  suggestions)

(export-always 'filter-exact-matches)
(defun filter-exact-matches (suggestions source input)
  "Return only SUGGESTIONS that match all the words in INPUT."
  (if (str:empty? input)
      suggestions
      (let ((words (sera:words input)))
        (delete-if (lambda (suggestion)
                     (notevery (lambda (sub) (search sub (ensure-match-data-string suggestion source)))
                               words))
                   suggestions))))

(export-always 'filter-exact-match)
(defun filter-exact-match (suggestions source input)
  "Return only SUGGESTIONS that are identical to INPUT"
  (declare (ignore source))
  (if (str:empty? input)
      suggestions
      (delete-if (lambda (suggestion)
                   (not (string-equal input (attributes-default suggestion))))
                 suggestions)))
