;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :minibuffer)

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

(defun format-properties (properties &optional downcasedp)
  (let ((result
          (str:join " "
                    (loop for i on properties by #'cddr
                          collect (second i)))))
    (if downcasedp
        (string-downcase result)
        result)))

(export-always 'delete-inexact-matches)
(defun delete-inexact-matches (suggestions source input)
  "Destructively filter out non-exact matches from SUGGESTIONS.
Return the resulting list.
If any input substring matches exactly (but not necessarily a whole word),
then all suggestions that are not exactly matched by at least one substring are removed.

Suitable as a `minibuffer-source' `filter-preprocessor'."
  ;; TODO: Compute match-data separately.  Make it customizable.
  (dolist (suggestion suggestions)
    (setf
     (match-data suggestion)
     (format-properties
      (filtered-properties-suggestion suggestion (active-properties source))
      (str:downcasep input))))
  (when input
    (let ((exactly-matching-substrings (find-exactly-matching-substrings
                                        input
                                        (mapcar #'match-data suggestions))))
      (when exactly-matching-substrings
        (setf suggestions
              (delete-if (lambda (suggestion)
                           (not (loop for i in exactly-matching-substrings
                                      always (search i (match-data suggestion)))))
                         suggestions)))))
  suggestions)
