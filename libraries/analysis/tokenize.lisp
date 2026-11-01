;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(-> stop-word-p (string) boolean)
(defun stop-word-p (word)
  (gethash word (stop-words-lookup *language-data*)))

(-> fold-word (string) string)
(defun fold-word (word)
  (substitute #\' #\’ (string-downcase word)))

(-> strip-possessive (string) string)
(defun strip-possessive (word)
  "Drop a trailing `'s' so possessives share a term with the base word."
  (let ((length (length word)))
    (if (and (> length 2)
             (char-equal (char word (1- length)) #\s)
             (char= (char word (- length 2)) #\'))
        (subseq word 0 (- length 2))
        word)))

(-> stem-word (string) string)
(defun stem-word (word)
  "Return WORD stemmed when every character is a lowercase letter.
Other tokens are returned unchanged."
  (if (and (plusp (length word))
           (every (lambda (char) (char<= #\a char #\z)) word))
      (copy-seq (stem word))
      word))

(-> next-word (string integer) (values (or null string) integer))
(defun next-word (string start)
  "Return the next word in STRING at or after START, and the index after it.
A word is letters and digits. An apostrophe or hyphen is kept only when it
sits between those characters."
  (let ((length (length string))
        (index start))
    (loop while (and (< index length)
                     (not (alphanumericp (char string index))))
          do (incf index))
    (when (>= index length)
      (return-from next-word (values nil index)))
    (let ((word-start index))
      (loop while (< index length)
            for char = (char string index)
            do (cond
                 ((alphanumericp char)
                  (incf index))
                 ((and (or (char= char #\')
                           (char= char #\-)
                           (char= char #\’))
                       (< (1+ index) length)
                       (alphanumericp (char string (1+ index))))
                  (incf index))
                 (t
                  (return))))
      (values (subseq string word-start index) index))))

(-> word-tokenize ((or null string) &key (:remove-stop-words boolean)
                    (:stem boolean)
                    (:down-case boolean)
                    (:alphabetic boolean)
                    (:min-length integer))
    list)
(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t)
                              (alphabetic t) (min-length 1))
  "Split STRING into words.
Punctuation around a word is ignored. Apostrophes and hyphens stay when they
sit inside a word. Stop words, tokens shorter than MIN-LENGTH, and, when
ALPHABETIC is true, tokens with no letter are dropped."
  (unless (and (stringp string) (plusp (length string)))
    (return-from word-tokenize '()))
  (let ((words '())
        (index 0))
    (loop
      (multiple-value-bind (raw next) (next-word string index)
        (unless raw
          (return))
        (setf index next)
        (let* ((folded (fold-word raw))
               (token (if down-case folded raw)))
          (unless (and remove-stop-words (stop-word-p folded))
            (let* ((stripped (strip-possessive token))
                   (folded-stripped (fold-word stripped)))
              (when (and (not (and remove-stop-words (stop-word-p folded-stripped)))
                         (>= (length stripped) min-length)
                         (or (not alphabetic)
                             (find-if #'alpha-char-p stripped)))
                (push (if stem
                          (stem-word folded-stripped)
                          stripped)
                      words)))))))
    (nreverse words)))

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  ;; TODO: Use "\\p{Terminal_Punctuation}" regexp instead to catch all terminal
  ;; punctuation marks, including "," and ";"?
  (remove "" (mapcar #'str:trim (cl-ppcre:split "[.!?]" string)) :test #'equal))
