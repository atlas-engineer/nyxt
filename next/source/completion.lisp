;;; completion.lisp --- completion matchers

;;; completion matchers are functions that accept an input string and
;;; a list of strings, and given some logic, return an ordered list
;;; of strings matched based on probability

(in-package :next)


(defun fuzzy-match (input candidates &optional accessor-function)
  ;; fuzzy-match works by taking a string input from the user. the
  ;; string is then populated with ".*" between each character to
  ;; create a regex. As an example, "nt" will become "n.*t.*". This
  ;; will enable matching of "next" or "note" etc.
  (let ((regex
	 (with-output-to-string (stream)
	   (loop for char across input do
		(princ #\. stream)
		(princ #\* stream)
		(princ char stream))
	   ;; match any chars after final char in input
	   (princ #\. stream)
	   (princ #\* stream)))
	(completions nil))
    ;; use constructed regex to see which options match
    (loop for candidate in candidates do
	 (when (cl-string-match:match-re
		regex
		(if accessor-function
		    (funcall accessor-function candidate)
		    candidate))
	   (push candidate completions)))
    completions))
