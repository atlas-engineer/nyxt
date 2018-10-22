;;; link-hint.lisp --- functions to enable link hinting and navigation
;;; This file's parenscript is licensed under license documents/external/LICENSE1

(in-package :next)

(defparenstatic add-link-hints
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun remove-if-not (predicate sequence)
    "Small reimplementation of remove-if"
    (loop for el in sequence
       when (predicate el)
       collect el))
  (defun code-char (n)
    "Alias of String.fromCharCode"
    (ps:chain -string (from-char-code n)))
  (defun is-in-viewport (el)
    "Finds out if an element is in the viewport"
    (let ((rect (ps:chain el (get-bounding-client-rect))))
      (and
       (>= (ps:@ rect top) 0)
       (>= (ps:@ rect left) 0)
       (<= (ps:@ rect bottom) (or (ps:@ window inner-height)
				  (ps:@ document document-element client-height)))
       (<= (ps:@ rect right) (or (ps:@ window inner-width)
				 (ps:@ document document-element client-width))))))
  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the link"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
	       :left (+ (ps:@ window page-x-offset) (- (ps:@ rect left) 20))))
  (defun hint-create-element (link hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain link (get-bounding-client-rect)))
	      (position (hint-determine-position rect))
	      (el (ps:chain document (create-element "span"))))
      (when (< (ps:@ position left) 0)
	(setf (ps:@ position left) (+ (ps:@ position left) 20)))
      (when (< (ps:@ position top) 0)
	(setf (ps:@ position top) (+ (ps:@ position top) 20)))
      (setf (ps:@ el class-name) "next-link-hint")
      (setf (ps:@ el style background) "rgba(255, 255, 255, 0.75)")
      (setf (ps:@ el style border) "1px solid red")
      (setf (ps:@ el style font-weight) "bold")
      (setf (ps:@ el style position) "absolute")
      (setf (ps:@ el style text-align) "center")
      (setf (ps:@ el style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ el style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ el text-content) hint)
      el))
  (defun hint-add (link hint)
    "Adds a hint on a single link"
    (ps:let ((hint-element (hint-create-element link hint)))
      (ps:chain document body (append-child hint-element))
      hint-element))
  (defun hints-add (links)
    "Adds hints on links"
    (ps:let* ((links-length (length links))
	   (hints (hints-generate links-length)))
      (ps:chain -j-s-o-n
		(stringify
		 (loop for i from 0 to (- links-length 1)
		    collect (list
			     (ps:@ (hint-add (elt links i) (elt hints i)) inner-text)
			     (ps:@ (elt links i) href)))))))
  (defun hints-determine-chars-length (length)
    "Finds out how many chars long the hints must be"
    (ps:let ((i 1))
      ;; 26 chars in alphabet
      (loop while (> length (expt 26 i))
	 do (incf i))
      i))
  (defun hints-generate (length)
    "Generates hints that will appear on the links"
    (strings-generate length (hints-determine-chars-length length)))
  (defun links-find (window document)
    "Finds all the links within the viewport"
    (remove-if-not #'is-in-viewport (qsa document "a")))
  (defun strings-generate (length chars-length)
    "Generates strings of specified length"
    (ps:let ((minimum (1+ (ps:chain -math (pow 26 (- chars-length 1))))))
      (loop for i from minimum to (+ minimum length)
	 collect (string-generate i))))
  (defun string-generate (n)
    "Generates a string from a number"
    (if (>= n 0)
	(+ (string-generate (floor (- (/ n 26) 1)))
	   (code-char (+ 65
			 (rem n 26))))
	""))
  (hints-add (links-find window document)))

(defparenstatic remove-link-hints
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun hints-remove-all ()
    "Removes all the links"
    (ps:dolist (el (qsa document ".next-link-hint"))
      (ps:chain el (remove))))
  (hints-remove-all))
