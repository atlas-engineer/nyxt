(in-package :nyxt)

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

(defun score-suggestion (input suggestion)
  "Return a SUGGESTION's score for INPUT.
A higher score means the suggestion comes first."
  ;; The Jaccard metric seems to provide much better results than, say,
  ;; Damerau-Levensthein but it's much slower.
  ;; TODO: Check out fzf for a possibly good scoring algorithm.
  (+ (* 1.0 (mk-string-metrics:norm-damerau-levenshtein suggestion input))
     (* 1.0 (substring-norm (str:split " " input) suggestion))))

(defvar score-threshold 0.0             ; TODO: Learn good value and enable low-score filtering below.
  "The threshold under which suggestions are eleminated.")

(defun sort-suggestions (input suggestion-pairs)
  "Sort SUGGESTION-PAIRS, the pair closest to INPUT in the levenshtein distance comes first.
SUGGESTION-PAIRS is a list of (display-value real-value).  See `fuzzy-match' for
more details."
  ;; WARNING: mk-string-metrics works on low-level arrays and might not get
  ;; the text encoding right.  We need to make sure the suggestions and the
  ;; input are of the same encoding.
  (setf input (to-unicode input))
  (dolist (suggestion-pair suggestion-pairs)
    (setf (first suggestion-pair) (to-unicode (first suggestion-pair))))
  (flet ((score-suggestion (pair)
           (cons (score-suggestion input (first pair)) pair))
         ;; (low-score (triplet)
         ;;   (< (first triplet) score-threshold))
         (sort-suggestion (triplet1 triplet2)
           (> (first triplet1)
              (first triplet2)))
         (triplet-to-pair (triplet)
           (rest triplet)))
    (mapcar #'triplet-to-pair
            (sort ;; (remove-if #'low-score)
                  (mapcar #'score-suggestion suggestion-pairs)
                  #'sort-suggestion))))

(defun find-exactly-matching-substrings (input suggestions &key (substring-length 2))
  "Return the list of input substrings that match at least one suggestion.
The substrings must be SUBSTRING-LENGTH characters long or more."
  (let ((input-strings (delete-if (lambda (s) (< (length s) substring-length))
                                  (str:split " " input :omit-nulls t))))
    (when input-strings
      (delete-duplicates
       (loop for suggestion in suggestions
             append (remove-if
                     (lambda (i)
                       (not (search i suggestion)))
                     input-strings))
       :test #'string=))))

(defun keep-exact-matches-in-suggestions (input suggestion-pairs)
  "Destructively filter out non-exact matches from suggestions.
If any input substring matches exactly (but not necessarily a whole word),
then all suggestions that are not exactly matched by at least one substring are removed."
  (let* ((exactly-matching-substrings (find-exactly-matching-substrings
                                       input
                                       (mapcar #'first suggestion-pairs))))
    (if exactly-matching-substrings
        (setf suggestion-pairs
              (delete-if (lambda (suggestion-pair)
                           (not (loop for i in exactly-matching-substrings
                                      always (search i (first suggestion-pair)))))
                         suggestion-pairs))
        suggestion-pairs)))

(export-always 'fuzzy-match)
(defun fuzzy-match (input suggestions &key suggestions-display) ; TODO: Make score functions customizable, e.g. for global history.
  "From the user input and a list of suggestions, return a filtered list of
suggestions that have all the input words in them, and sort this list to have the
'most relevant' first.
The match is case-sensitive if INPUT contains at least one uppercase character.
SUGGESTIONS-DISPLAY can be used to pass the pre-computed display strings of the
suggestions; otherwise `object-display' is used."
  ;; To sort by the display value, we store all the suggestions in a
  ;; (display-value real-value) list or pairs.
  (let ((pairs (if suggestions-display
                   (mapcar #'list suggestions-display suggestions)
                   (mapcar (lambda (c) (list (object-display c) c)) suggestions))))
    (if (not (str:empty? input))
        (let* ((input (str:replace-all " " " " input))
               (pairs (if (str:downcasep input)
                          (mapcar (lambda (p) (list (string-downcase (first p)) (second p))) pairs)
                          pairs))
               (pairs (keep-exact-matches-in-suggestions input pairs))
               (pairs (sort-suggestions input pairs)))
          (log:debug "~a"
                     (let ((limit 100)
                           (pairs (mapcar (lambda (c)
                                            (list (first c)
                                                  (score-suggestion (to-unicode input) (first c))))
                                          pairs)))
                       ;; Don't display more than 100 elements to avoid flooding stdout.
                       (if (< (length pairs) limit)
                           pairs
                           (nconc (subseq pairs 0 limit) (list "...")))))
          (mapcar #'second pairs))
        suggestions)))

(export-always 'file-suggestion-function)
(defun file-suggestion-function (input files)
  "Fuzzy-match this list of files."
  (fuzzy-match input files))
