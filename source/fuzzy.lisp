;;; utility.lisp --- fuzzy matching utilities.

(in-package :next)
(annot:enable-annot-syntax)


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

(defun exact-match-norm (input candidate)
  "Average of the normalized position of all the exact matches in input substrings.
If 0 exact matches, return 0."
  (let* ((exactly-matching-substrings (find-exactly-matching-substrings
                                       input
                                       (list candidate))))
    (if exactly-matching-substrings
        (let ((positions (mapcar (lambda (p) (- (length candidate) p))
                                 (delete-if #'null
                                            (mapcar (lambda (substring)
                                                      (search substring candidate))
                                                    exactly-matching-substrings)))))
          (/ (apply #'+ positions)
             (* (length positions) (length candidate))))
        0)))

(defun to-unicode (input)
  "Convert INPUT to (simple-array character) type."
  (if (typep input 'base-string)
      (coerce input `(simple-array character (,(length input))))
      input))

(defun score-candidate (input candidate)
  "Return a CANDIDATE's score for INPUT.
A higher score means the candidate comes first."
  ;; The Jaccard metric seems to provide much better results than, say,
  ;; Damerau-Levensthein but it's much slower.
  ;; TODO: Check out fzf for a possibly good scoring algorithm.
  (+ (* 1.0 (mk-string-metrics:norm-damerau-levenshtein candidate input))
     (* 1.0 (substring-norm (str:split " " input) candidate))
     (* 1.0 (exact-match-norm input candidate))))

(defun sort-candidates (input candidate-pairs)
  "Sort CANDIDATE-PAIRS, the pair closest to INPUT in the levenshtein distance comes first.
CANDIDATE-PAIRS is a list of (display-value real-value).  See `fuzzy-match' for
more details."
    ;; WARNING: mk-string-metrics works on low-level arrays and might not get
    ;; the text encoding right.  We need to make sure the candidates and the
    ;; input are of the same encoding.
  (setf input (to-unicode input))
  (dolist (candidate-pair candidate-pairs)
    (setf (first candidate-pair) (to-unicode (first candidate-pair))))
  (sort candidate-pairs
        (lambda (x y)
          (> (score-candidate input (first x))
             (score-candidate input (first y))))))

(defun find-exactly-matching-substrings (input candidates &key (substring-length 2))
  "Return the list of input substrings that match at least one candidate.
The substrings must be SUBSTRING-LENGTH characters long or more."
  (let ((input-strings (delete-if (lambda (s) (< (length s) substring-length))
                                  (str:split " " input :omit-nulls t))))
    (when input-strings
      (delete-duplicates
       (loop for candidate in candidates
             append (remove-if
                     (lambda (i)
                       (not (search i candidate)))
                     input-strings))
       :test #'string=))))

(defun keep-exact-matches-in-candidates (input candidate-pairs)
  "Destructively filter out non-exact matches from candidates.
If any input substring matches exactly (but not necessarily a whole word),
then all candidates that are not exactly matched by at least one substring are removed."
  (let* ((exactly-matching-substrings (find-exactly-matching-substrings
                                       input
                                       (mapcar #'first candidate-pairs))))
    (if exactly-matching-substrings
        (setf candidate-pairs
              (delete-if (lambda (candidate-pair)
                           (not (loop for i in exactly-matching-substrings
                                      always (search i (first candidate-pair)))))
                         candidate-pairs))
        candidate-pairs)))

@export
(defun fuzzy-match (input candidates)   ; TODO: Make score functions customizable, e.g. for global history.
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
             (pairs (keep-exact-matches-in-candidates input pairs))
             (pairs (sort-candidates input pairs)))
        (log:debug "~a"
                   (mapcar (lambda (c)
                             (list (first c)
                                   (score-candidate (to-unicode input) (first c))))
                                pairs))
        (mapcar #'second pairs))
      candidates))

@export
(defun file-completion-function (input files)
  "Fuzzy-match this list of files."
  (fuzzy-match input files))
