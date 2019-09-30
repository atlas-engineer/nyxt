;;; utility.lisp --- fuzzy matching utilities.

(in-package :next)
(annot:enable-annot-syntax)


(defun substring-norm (string substrings)
  "Return the norm of SUBSTRINGS with regard to STRING.
The norm is closer to 1 if
- substrings start near the beginning of STRING;
- substrings length are closer to the length of STRING.

Only substrings of 3 characters or more are considered."
  ;; TODO: Remove duplicates in SUBSTRINGS?  Repeats could mean we insist more on it.
  (let ((long-substrings (remove-if (lambda (s) (> 3 (length s)))
                                    substrings)))
    (if long-substrings
        (/ (apply #'+
                  (mapcar (lambda (s)
                            (let ((position (search s string)))
                              (if (not position)
                                  0
                                  (/ (+
                                      ;; Position factor.
                                      (/ (- (length string) position)
                                         (length string))
                                      ;; Length factor.
                                      (/ (min (length s) (length string))
                                         (length string)))
                                     2))))
                          long-substrings))
           (length long-substrings))
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
  ;; Damerau-Levensthein.
  ;; TODO: Check out fzf for a possibly good scoring algorithm.
  (+ (* 1.0 (mk-string-metrics:jaccard input candidate))
     (* 1.0 (substring-norm candidate (str:split " " input)))))

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
             (pairs (sort-candidates input pairs)))
        (log:debug "~a" (mapcar (lambda (c) (list (first c)
                                                  (score-candidate (to-unicode input) (first c))))
                                pairs))
        (mapcar #'second pairs))
      candidates))
