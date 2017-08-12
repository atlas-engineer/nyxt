;;;; keymap.lisp --- lisp subroutines for key binding detection

;;;; keymaps are executed in priority from most specific to least
;;;; that is, the order of execution for keymaps is:
;;;; global-map --> major-mode-map --> minor-mode-maps
;;;; 
;;;; keys are defined with the following syntax:
;;;; (define-key global-map (kbd "C-x o") #'function-reference)
;;;; in the previous example, the sequence of keys:
;;;; "control+x", "o" would invoke the "function-reference"

(in-package :next)


(defvar *control-modifier* nil
  "A variable to store the status of the control key")
(defvar *meta-modifier* nil
  "A variable to store the status of the alt/meta key")
(defvar *super-modifier* nil
  "A variable to store the status of the super/cmd key")

(defparameter global-map (make-hash-table :test 'equalp)
  "A global key map, available in every mode/buffer")
(defparameter *key-sequence-stack* ()
  "A stack that keeps track of the key chords a user has inputted")

;; A struct used to describe a key-chord
(defstruct key
  character
  control-modifier
  meta-modifier
  super-modifier)

(defun key-press (obj event)
  ;; Invoked upon key-press
  (declare (ignore obj)) ; supress unused warnings
  (let ((key (|key| event)))
    (cond
      ((equalp key *control-key*)
       (setf *control-modifier* t))
      ((equalp key *meta-key*)
       (setf *meta-modifier* t))
      (t (progn
	   (push-key-chord key)
	   (consume-key-sequence))))))

(defun key-release (obj event)
  ;; Invoked upon key-release
  (declare (ignore obj)) ; supress unused warnings
  (let ((key (|key| event)))
    (cond
      ((equalp key *control-key*)
       (setf *control-modifier* nil))
      ((equalp key *meta-key*)
       (setf *meta-modifier* nil))
      (t (return-from key-release)))))

(defun push-key-chord (key-character-string)
  ;; Adds a new chord to key-sequence
  ;; For example, it may add C-M-s or C-x
  ;; to a stack which will be consumed by
  ;; consume-key-sequence
  (let ((key-chord (make-key)))
    (if *control-modifier*
	(setf (key-control-modifier key-chord) t))
    
    (setf (key-character key-chord) key-character-string)
    (push key-chord *key-sequence-stack*)))

(defun consume-key-sequence ()
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let ((key-maps (list
		   global-map
		   (mode-keymap (buffer-mode *active-buffer*)))))
    (dolist (map key-maps)
      (if (gethash *key-sequence-stack* map)
	  (progn 
	    ;; If not prefix key, consume
	    (if (not (equalp (gethash *key-sequence-stack* map) "prefix"))
		(progn
		  (funcall (gethash *key-sequence-stack* map))
		  (setf *key-sequence-stack* ())))
	    (return-from consume-key-sequence t))))
    ;; If we make it to this point, key did not exist
    ;; in any of the keymaps, print a message and clear stack
    (print "Key Undefined")
    (setf *key-sequence-stack* ())
    (return-from consume-key-sequence nil)))

(defun define-key (mode-map key-sequence function)
  ;; A sequence of "C-x" "C-s" "C-a" will be broken
  ;; up into three keys for the mode map, these are
  ;; "C-x" "C-s" "C-a" - points to function
  ;; "C-x" "C-s"       - set to "prefix"
  ;; "C-x"             - set to "prefix"
  ;;
  ;; When a key is set to "prefix" it will not
  ;; consume the stack, so that a sequence of keys
  ;; longer than one key-chord can be recorded
  (setf (gethash key-sequence mode-map) function)
  ;; generate prefix representations
  (loop while key-sequence
     do
       (pop key-sequence)
       (setf (gethash key-sequence mode-map) "prefix")))

(defun kbd (key-sequence-string)
  ;; Take a key-sequence-string in the form of "C-x C-s"
  ;; Firstly, break it apart into chords: "C-x" and "C-s"
  ;; Then, break apart the chords into individual keys
  ;; Use those individual keys to create a "key" struct
  ;; that describes the chord. We now have two "keys"
  ;; connect these two keys in a list <key> C-x, <key> C-s
  ;; this is will serve as the key to our key->function map
  (let ((key-sequence ()))
    ;; Iterate through all key chords (space delimited)
    (loop for key-chord-string in (split key-sequence-string " ")
       ;; Iterate through all keys in chord (hyphen delimited)
       do (let ((key-chord (make-key)))
  	    (loop for key-character-string in (split key-chord-string "-")
  	       do (cond
  		    ((equal "C" key-character-string) (setf (key-control-modifier key-chord) t))
  		    (t (setf (key-character key-chord)
  			     ;; Convert from the actual key to the QT code representation
  			     (gethash key-character-string *character->keycode*)))))
  	    (push key-chord key-sequence)))
    key-sequence))

;;; set standard keybindings
(define-key global-map (kbd "C-x C-c") #'qquit)
