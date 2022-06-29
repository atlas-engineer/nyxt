# SPDX-FileCopyrightText: 2017 John Mercouris <john@atlas.engineer>
# SPDX-FileCopyrightText: 2017 Atlas Engineer LLC
#
# SPDX-License-Identifier: BSD-3-Clause

# Atlas Engineer LLC


(in-package :analysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The software is completely free for any purpose, unless notes at
;; the head of the program text indicates otherwise (which is
;; rare). In any case, the notes about licensing are never more
;; restrictive than the BSD License.
;
;; In every case where the software is not written by me (Martin
;; Porter), this licensing arrangement has been endorsed by the
;; contributor, and it is therefore unnecessary to ask the contributor
;; again to confirm it.
;
;; The Porter Stemming Algorithm, somewhat mechanically hand translated to Common Lisp by
;; Steven M. Haflich smh@franz.com Feb 2002.  Most of the inline comments refer to the
;; original C code.  At the time of this translation the code passes the associated Porter
;; test files.  See the function test at the end of this file.

;; This port is intended to be portable ANSI Common Lisp.  However, it has only been
;; compiled and tested with Allegro Common Lisp.  This code is offered in the hope it will
;; be useful, but with no warranty of correctness, suitability, usability, or anything
;; else.  The C implementation from which this code was derived was not reentrant, relying
;; on global variables.  This implementation corrects that.  It is intended that a word to
;; be stemmed will be in a string with fill-pointer, as this is a natural result when
;; parsing user input, web scraping, whatever.  If not, a string with fill-pointer is
;; created, but this is an efficiency hit and is here intended only for lightweight use or
;; testing.  Using some resource mechanism on these strings would be a useful improvement,
;; whether here or in the calling code.

;; Postscript: When I contacted Martin Porter about this anachronism, he decided to fix
;; the C version to implement proper reentrancy.  The CL version is now also served from
;; his central site.  It should be functionally identical to this one, modulo the current
;; comment and a couple harmless formatting and comment changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the Porter stemming algorithm, coded up in ANSI C by the
;; author. It may be be regarded as cononical, in that it follows the
;; algorithm presented in

;; Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
;; no. 3, pp 130-137,

;; only differing from it at the points maked --DEPARTURE-- below.

;; See also http://www.tartarus.org/~martin/PorterStemmer

;; The algorithm as described in the paper could be exactly replicated
;; by adjusting the points of DEPARTURE, but this is barely necessary,
;; because (a) the points of DEPARTURE are definitely improvements, and
;; (b) no encoding of the Porter stemmer I have seen is anything like
;; as exact as this version, even with the points of DEPARTURE!

;; You can compile it on Unix with 'gcc -O3 -o stem stem.c' after which
;; 'stem' takes a list of inputs and sends the stemmed equivalent to
;; stdout.

;; The algorithm as encoded here is particularly fast.

;; Release 1

;; The main part of the stemming algorithm starts here. b is a buffer
;; holding a word to be stemmed. The letters are in b[k0], b[k0+1] ...
;; ending at b[k]. In fact k0 = 0 in this demo program. k is readjusted
;; downwards as the stemming progresses. Zero termination is not in fact
;; used in the algorithm.

;; Note that only lower case sequences are stemmed. Forcing to lower case
;; should be done before stem(...) is called.

;; cons(i) is TRUE <=> b[i] is a consonant.

;;; Common Lisp port Version 1.01

;;;
;;; Common Lisp port Version history
;;;
;;; 1.0  -- smh@franz.com Feb 2002
;;;         initial release
;;;
;;; 1.01 -- smh@franz.com 25 Apr 2004
;;;         step4 signalled error for "ion" "ions".  Thanks to Jeff Heard
;;;         for detecting this and suggesting the fix.

(defun consonantp (str i)
  (let ((char (char str i)))
    (cond ((member char '(#\a #\e #\i #\o #\u)) nil)
	  ((eql char #\y)
	   (if (= i 0) t (not (consonantp str (1- i)))))
	  (t t))))

;; m() measures the number of consonant sequences between k0 and j. if c is
;; a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
;; presence,

;;    <c><v>       gives 0
;;    <c>vc<v>     gives 1
;;    <c>vcvc<v>   gives 2
;;    <c>vcvcvc<v> gives 3
;;    ....

(defun m (str lim)
  (let ((n 0)
	(i 0))
    (loop
      (when (>= i lim) (return-from m n))
      (if (not (consonantp str i)) (return nil))
      (incf i))
    (incf i)
    (loop
      (loop
	(if (>= i lim) (return-from m n))
	(if (consonantp str i) (return nil))
	(incf i))
      (incf i)
      (incf n)
      (loop
	(if (>= i lim) (return-from m n))
	(if (not (consonantp str i)) (return nil))
	(incf i))
      (incf i))))

;; vowelinstem() is TRUE <=> k0,...j contains a vowel

(defun vowelinstem (str)
  (loop for i from 0 below (fill-pointer str)
      unless (consonantp str i) return t))

;; doublec(j) is TRUE <=> j,(j-1) contain a double consonant.

(defun doublec (str i)
  (cond ((< i 1) nil)
	((not (eql (char str i) (char str (1- i)))) nil)
	(t (consonantp str i))))

;; cvc(i) is TRUE <=> i-2,i-1,i has the form consonant - vowel - consonant
;; and also if the second c is not w,x or y. this is used when trying to
;; restore an e at the end of a short word. e.g.

;;    cav(e), lov(e), hop(e), crim(e), but
;;    snow, box, tray.

(defun cvc (str lim)
  (decf lim)
  (if (or (< lim 2)
	  (not (consonantp str lim))
	  (consonantp str (1- lim))
	  (not (consonantp str (- lim 2))))
      (return-from cvc nil))
  (if (member (char str lim) '(#\w #\x #\y)) (return-from cvc nil))
  t)

;; ends(s) is TRUE <=> k0,...k ends with the string s.

(defun ends (str ending)
  (declare (string str) (simple-string ending))
  (let ((len1 (length str)) (len2 (length ending)))
    (loop
	for pa downfrom (1- len1) to 0
	and pb downfrom (1- len2) to 0
	unless (eql (char str pa) (char ending pb))
	return nil
	finally (return (when (< pb 0)
			  (decf (fill-pointer str) len2)
			  t)))))

;; setto(s) sets (j+1),...k to the characters in the string s, readjusting k.

(defun setto (str suffix)
  (declare (string str) (simple-string suffix))
  (loop for char across suffix
      do (vector-push-extend char str)))

;; r(s) is used further down.

(defun r (str s sfp)
  (if (> (m str (fill-pointer str)) 0)
      (setto str s)
    (setf (fill-pointer str) sfp)))

;; step1ab() gets rid of plurals and -ed or -ing. e.g.

;;     caresses  ->  caress
;;     ponies    ->  poni
;;     ties      ->  ti
;;     caress    ->  caress
;;     cats      ->  cat

;;     feed      ->  feed
;;     agreed    ->  agree
;;     disabled  ->  disable

;;     matting   ->  mat
;;     mating    ->  mate
;;     meeting   ->  meet
;;     milling   ->  mill
;;     messing   ->  mess

;;     meetings  ->  meet

(defun step1ab (str)
  (when (eql (char str (1- (fill-pointer str))) #\s)
    (cond ((ends str "sses") (incf (fill-pointer str) 2))
	  ((ends str "ies")  (setto str "i"))
	  ((not (eql (char str (- (fill-pointer str) 2)) #\s)) (decf (fill-pointer str)))))
  (cond ((ends str "eed") (if (> (m str (fill-pointer str)) 0)
			      (incf (fill-pointer str) 2)
			    (incf (fill-pointer str) 3)))
	((let ((sfp (fill-pointer str)))
	   (if (or (ends str "ed")
		   (ends str "ing"))
	       (if (vowelinstem str)
		   t
		 (progn (setf (fill-pointer str) sfp)
			nil))))
	 (cond ((ends str "at") (setto str "ate"))
	       ((ends str "bl") (setto str "ble"))
	       ((ends str "iz") (setto str "ize"))
	       ((doublec str (1- (fill-pointer str)))
		(unless (member (char str (1- (fill-pointer str))) '(#\l #\s #\z))
		  (decf (fill-pointer str))))
	       (t (if (and (= (m str (fill-pointer str)) 1)
			   (cvc str (fill-pointer str)))
		      (setto str "e"))))))
  str)

;; step1c() turns terminal y to i when there is another vowel in the stem.

(defun step1c (str)
  (let ((saved-fill-pointer (fill-pointer str)))
    (when (and (ends str "y")
	       (vowelinstem str))
	(setf (char str (fill-pointer str)) #\i))
    (setf (fill-pointer str) saved-fill-pointer))
  str)

;; step2() maps double suffices to single ones. so -ization ( = -ize plus
;; -ation) maps to -ize etc. note that the string before the suffix must give
;; m() > 0.

(defun step2 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)
      (block nil
	(case (char str (- (length str) 2))
	  (#\a (when (ends str "ational") (r str "ate"  sfp)  (return))
	       (when (ends str "tional")  (r str "tion" sfp) (return)))
	  (#\c (when (ends str "enci")    (r str "ence" sfp) (return))
	       (when (ends str "anci")    (r str "ance" sfp) (return)))
	  (#\e (when (ends str "izer")    (r str "ize"  sfp)  (return)))
	  (#\l (when (ends str "bli")     (r str "ble"  sfp)  (return))
	       ;; -DEPARTURE-
	       ;; To match the published algorithm, replace prev line with
	       ;; ((when (ends str "abli")    (r str "able" sfp) (return))
	       (when (ends str "alli")    (r str "al"  sfp)   (return))
	       (when (ends str "entli")   (r str "ent" sfp)  (return))
	       (when (ends str "eli")     (r str "e"   sfp)    (return))
	       (when (ends str "ousli")   (r str "ous" sfp)  (return)))
	  (#\o (when (ends str "ization") (r str "ize" sfp)  (return))
	       (when (ends str "ation")   (r str "ate" sfp)  (return))
	       (when (ends str "ator")    (r str "ate" sfp)  (return)))
	  (#\s (when (ends str "alism")   (r str "al"  sfp)   (return))
	       (when (ends str "iveness") (r str "ive" sfp)  (return))
	       (when (ends str "fulness") (r str "ful" sfp)  (return))
	       (when (ends str "ousness") (r str "ous" sfp)  (return)))
	  (#\t (when (ends str "aliti")   (r str "al"  sfp)   (return))
	       (when (ends str "iviti")   (r str "ive" sfp)  (return))
	       (when (ends str "biliti")  (r str "ble" sfp)  (return)))
	  ;; -DEPARTURE-
	  ;; To match the published algorithm, delete next line.
	  (#\g (when (ends str "logi")    (r str "log" sfp)  (return)))))))
  str)

;; step3() deals with -ic-, -full, -ness etc. similar strategy to step2.

(defun step3 (str)
  (let ((sfp (fill-pointer str)))
    (block nil
      (case (char str (1- (length str)))
	(#\e (when (ends str "icate") (r str "ic" sfp) (return))
	     (when (ends str "ative") (r str "" sfp)   (return)) ; huh?
	     (when (ends str "alize") (r str "al" sfp) (return)))
	(#\i (when (ends str "iciti") (r str "ic" sfp) (return)))
	(#\l (when (ends str "ical")  (r str "ic" sfp) (return))
	     (when (ends str "ful")   (r str "" sfp)   (return))) ; huh?
	(#\s (when (ends str "ness")  (r str "" sfp)   (return))) ; huh?
	)))
  str)

;; step4() takes off -ant, -ence etc., in context <c>vcvc<v>.

(defun step4 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)			; Unnecessary?
      (block nil
	(case (char str (- sfp 2))
	  (#\a (if (ends str "al")    (return)))
	  (#\c (if (ends str "ance")  (return))
	       (if (ends str "ence")  (return)))
	  (#\e (if (ends str "er")    (return)))
	  (#\i (if (ends str "ic")    (return)))
	  (#\l (if (ends str "able")  (return))
	       (if (ends str "ible")  (return)))
	  (#\n (if (ends str "ant")   (return))
	       (if (ends str "ement") (return))
	       (if (ends str "ment")  (return))
	       (if (ends str "ent")   (return)))
	  (#\o (if (ends str "ion")
		   (let ((len (length str)))
		     (if (and (> len 0)
			      (let ((c (char str (1- len))))
				(or (eql c #\s) (eql c #\t))))
			 (return)
		       (setf (fill-pointer str) sfp))))
	       (if (ends str "ou")    (return))) ; takes care of -ous
	  (#\s (if (ends str "ism")   (return)))
	  (#\t (if (ends str "ate")   (return))
	       (if (ends str "iti")   (return)))
	  (#\u (if (ends str "ous")   (return)))
	  (#\v (if (ends str "ive")   (return)))
	  (#\z (if (ends str "ize")   (return))))
	(return-from step4 str))
      (unless (> (m str (fill-pointer str)) 1)
	(setf (fill-pointer str) sfp)))
    str))

;; step5() removes a final -e if m() > 1, and changes -ll to -l if m() > 1.

(defun step5 (str)
  (let ((len (fill-pointer str)))
    (if (eql (char str (1- len)) #\e)
	(let ((a (m str len)))
	  (if (or (> a 1)
		  (and (= a 1)
		       (not (cvc str (1- len)))))
	      (decf (fill-pointer str))))))
  (let ((len (fill-pointer str)))
    (if (and (eql (char str (1- len)) #\l)
	     (doublec str (1- len))
	     (> (m str len) 1))
	(decf (fill-pointer str))))
  str)

;; In stem(p,i,j), p is a char pointer, and the string to be stemmed is from p[i] to p[j]
;; inclusive. Typically i is zero and j is the offset to the last character of a string,
;; (p[j+1] == '\0'). The stemmer adjusts the characters p[i] ... p[j] and returns the new
;; end-point of the string, k.  Stemming never increases word length, so i <= k <= j. To
;; turn the stemmer into a module, declare 'stem' as extern, and delete the remainder of
;; this file.

(defun stem (str)
  (let ((len (length str)))
    ;; With this line, strings of length 1 or 2 don't go through the
    ;; stemming process, although no mention is made of this in the
    ;; published algorithm. Remove the line to match the published
    ;; algorithm.
    (if (<= len 2) (return-from stem str)) ; /*-DEPARTURE-*/
    (if (typep str 'simple-string)	; Primarily for testing.
	(setf str
	  (make-array len :element-type 'character
		      :fill-pointer len :initial-contents str)))
    (step1ab str) (step1c str) (step2 str) (step3 str) (step4 str) (step5 str)
    str))

#+never
(trace step1ab step1c step2 step3 step4 step5)

#+never
(defun test ()				; Run against the distributed test files.
  (with-open-file (f1 "voc.txt")
    (with-open-file (f2 "output.txt")
      (loop as w1 = (read-line f1 nil nil)
	  while w1
	  as w2 = (read-line f2 nil nil)
	  as w3 = (stem w1)
	  if (equal w2 w3)
	  count t into successes
	  else count t into failures
	  and do (format t "(stem ~s) => ~s wanted ~s~%" w1 w3 w2)
	  finally (progn (format t "sucesses ~d failures ~d~%" successes failures)
			 (return failures))))))
