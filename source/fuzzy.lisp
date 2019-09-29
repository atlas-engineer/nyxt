;;; utility.lisp --- fuzzy matching utilities.

(in-package :next)
(annot:enable-annot-syntax)


(defun interleave-chars-with-regexp (input)
  "Insert '.*' between all characters of `input'."
  (check-type input string)
  (with-output-to-string (stream)
    (loop for char across input do
         (princ #\. stream)
         (princ #\* stream)
         (princ (ppcre:quote-meta-chars (string char)) stream))
    ;; match any chars after final char in cleaned-input
    (princ #\. stream)
    (princ #\* stream)))

(defun all-permutations (list)
  "From a list of words, return a list of lists containing all the permutations.
(foo bar) => ((foo bar) (bar foo))"
  ;; thanks https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
              append (mapcar (lambda (l) (cons element l))
                             (all-permutations (remove element list)))))))

(defun join-permutations-to-regexp (permutations)
  "Join the `permutations' (list of lists of strings) into a 'OR' regexp.
  For example: ((foo bar) (bar foo)) => (foo.*bar)|(bar.*foo)"
  (check-type permutations list)
  (str:join "|"
            (loop for innerlist in permutations
               for innerlist-with-regexp = (mapcar (lambda (word)
                                                    (interleave-chars-with-regexp word))
                                                  innerlist)
               collect
                 (str:join "" innerlist-with-regexp))))

(defun build-input-regexp-permutations (input)
  "Build a regexp of all words permutations from `input'.
'foo bar' => '(foo.*bar)|(bar.*foo)'"
  (let* ((input-permutations (all-permutations (str:words input)))
         (input-re (join-permutations-to-regexp input-permutations)))
    input-re))

(defun candidate-match-p (input-re candidate)
  (ppcre:scan input-re candidate))

(defun match-permutation-candidates (input candidate-pairs)
  "Return the list of candidates that contain all the words of the input.
CANDIDATE-PAIRS is a list of (display-value real-value).  See `fuzzy-match' for
more details."
  (when (and input candidate-pairs)
    (loop for (candidate real-value) in candidate-pairs
       when (candidate-match-p (build-input-regexp-permutations input) candidate)
         collect (list candidate real-value))))

(defun search-or-lose (substring string)
  "Search for `substring' in `string' but always return a number.
If there is no match, return a \"big\"number instead of nil (necessary to
use this as a `sort' function, here in the context of sorting
autocompletion candidates)."
  (let ((index (search substring string)))
    (if index index 1000)))

(defun sort-beginning-with (word candidate-pairs)
  "Return (a new sequence) with candidates that start with `word' first.
CANDIDATE-PAIRS is a list of (display-value real-value).  The display-value is
used for comparison.  See `fuzzy-match' for more details."
  (sort (copy-seq candidate-pairs) (lambda (x y)
                                     (< (search-or-lose word (first x))
                                        (search-or-lose word (first y))))))

(defun sort-levenshtein (input candidate-pairs)
  "Sort CANDIDATE-PAIRS, the pair closest to INPUT in the levenshtein distance comes first.
CANDIDATE-PAIRS is a list of (display-value real-value).  See `fuzzy-match' for
more details."
  (sort (copy-seq candidate-pairs) (lambda (x y)
                                     (< (mk-string-metrics:levenshtein input (first x))
                                        (mk-string-metrics:levenshtein input (first y))))))

@export
(defun fuzzy-match (input candidates)
  "From the user input and a list of candidates, return a filtered list of
candidates that have all the input words in them, and sort this list to have the
'most relevant' first.
The match is case-sensitive if INPUT contains at least one uppercase character."
  (if (not (str:empty? input))
      (let* ((input (str:replace-all " " " " input))
             ;; To sort by the display value, we store all the candidates in a
             ;; (display-value real-value) list or pairs.
             (pairs (mapcar (lambda (c) (list (object-string c) c)) candidates))
             (pairs (if (str:downcasep input)
                        (mapcar (lambda (p) (list (string-downcase (first p)) (second p))) pairs)
                        pairs))
             (pairs (match-permutation-candidates input pairs))
             (pairs (sort-levenshtein input pairs))
             (pairs (sort-beginning-with (first (str:words input)) pairs)))
        (mapcar #'second pairs))
      candidates))
