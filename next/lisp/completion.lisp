;;;; completion.lisp --- completion matchers

;;;; completion matchers are functions that accept an input string and
;;;; a list of strings, and given some logic, return an ordered list
;;;; of strings matched based on probability
;;;;
;;;; as an example, consider the possible implementation
;;;; "fuzzy_matcher" with input string "so str" and a list of strings:
;;;; "some string", "strike" we would expect the return to be "some
;;;; string", "strike", because it is more probable that the user is
;;;; looking for "some string"

(in-package :next)


(defun fuzzy-match (string string-list)
  (let ((regex
	 (with-output-to-string (stream)
	   (loop for char across string do
		(princ #\. stream)
		(princ #\* stream)
		(princ char stream))
	   ;; match any set of chars after final char
	   (princ #\. stream)
	   (princ #\* stream)))
	(results nil))
    (loop for element in string-list do
	 (when (match-re regex element)
	   (push element results)))
    results))
