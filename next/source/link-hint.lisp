;;;; link-hint.lisp --- functions to enable link hinting and navigation
;;;; This file's parenscript is licensed under license documents/external/LICENSE1

(in-package :next)

(defparen add-link-hints
  (ps:defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (ps:defun remove-if-not (predicate sequence)
    "Small reimplementation of remove-if"
    (ps:loop for el in sequence
	     when (predicate el)
	     collect el))
  (ps:defun code-char (n)
    "Alias of String.fromCharCode"
    (ps:chain -string (from-char-code n)))
  (ps:defun is-in-viewport (el)
    "Finds out if an element is in the viewport"
    (ps:let ((rect (ps:chain el (get-bounding-client-rect))))
      (and
       (>= (ps:@ rect top) 0)
       (>= (ps:@ rect left) 0)
       (<= (ps:@ rect bottom) (or (ps:@ window inner-height)
				  (ps:@ document document-element client-height)))
       (<= (ps:@ rect right) (or (ps:@ window inner-width)
				 (ps:@ document document-element client-width))))))
  (ps:defun hint-determine-position (rect)
    "Determines the position of a hint according to the link"
    (ps:create :top  (ps:@ rect top)
	       :left (- (ps:@ rect left) 20)))
  (ps:defun hint-create-element (link hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain link (get-bounding-client-rect)))
	      (position (hint-determine-position rect))
	      (el (ps:chain document (create-element "span"))))
      (when (< (ps:@ position left) 0)
	(setf (ps:@ position left) (+ (ps:@ position left) 20)))
      (when (< (ps:@ position top) 0)
	(setf (ps:@ position top) (+ (ps:@ position top) 20)))
      (setf (ps:@ el class-name) "lispkit-link-hints")
      (setf (ps:@ el style background) "yellow")
      (setf (ps:@ el style border) "1px solid #ccc")
      (setf (ps:@ el style position) "absolute")
      (setf (ps:@ el style min-width) "10px")
      (setf (ps:@ el style text-align) "center")
      (setf (ps:@ el style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ el style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ el text-content) hint)
      el))
  (ps:defun hint-add (link hint)
    "Adds a hint on a single link"
    (ps:let ((hint-element (hint-create-element link hint)))
      (ps:chain document body (append-child hint-element))
      hint-element))
  (ps:defun hints-add (links)
    "Adds hints on links"
    (ps:let* ((links-length (length links))
	   (hints (hints-generate links-length)))
      (ps:loop for i from 0 to (- links-length 1)
	 collect (ps:create :link (elt links i) :hint (hint-add (elt links i) (elt hints i))))))
  (ps:defun hints-determine-chars-length (length)
    "Finds out how many chars long the hints must be"
    (ps:let ((i 1))
      ;; 26 chars in alphabet
      (ps:loop while (> length (expt 26 i))
	 do (incf i))
      i))
  (ps:defun hints-generate (length)
    "Generates hints that will appear on the links"
    (strings-generate length (hints-determine-chars-length length)))
  (ps:defun links-find (window document)
    "Finds all the links within the viewport"
    (remove-if-not #'is-in-viewport (qsa document "a")))
  (ps:defun strings-generate (length chars-length)
    "Generates strings of specified length"
    (ps:let ((minimum (1+ (ps:chain -math (pow 26 (- chars-length 1))))))
      (ps:loop for i from minimum to (+ minimum length)
	 collect (string-generate i))))
  (ps:defun string-generate (n)
    "Generates a string from a number"
    (if (>= n 0)
	(+ (string-generate (floor (- (/ n 26) 1)))
	   (code-char (+ 65
			 (rem n 26))))
	""))
  (hints-add (links-find window document)))

(defun add-link-hints ()
  (interface:web-view-execute (view *active-buffer*) add-link-hints))

(defparen remove-link-hints
  (ps:defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (ps:defun hints-remove-all ()
    "Removes all the links"
    (ps:dolist (el (qsa document ".lispkit-link-hints"))
      (ps:chain el (remove))))
  (hints-remove-all))

(defun remove-link-hints ()
  (interface:web-view-execute (view *active-buffer*) remove-link-hints))
