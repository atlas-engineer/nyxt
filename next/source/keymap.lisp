;;; keymap.lisp --- lisp subroutines for key binding detection

;;; keymaps are executed in priority from most specific to least
;;; that is, the order of execution for keymaps is:
;;; *global-map* --> major-mode-map --> minor-mode-maps
;;;
;;; keys are defined with the following syntax:
;;; (define-key *global-map* (kbd "C-x o") #'function-reference)
;;; in the previous example, the sequence of keys:
;;; "control+x", "o" would invoke the "function-reference"

(in-package :next)

(defvar *key-sequence-stack* ()
  "A stack that keeps track of the key chords a user has inputted")

;; A struct used to describe a key-chord
(defstruct key
  character-code
  control-modifier
  meta-modifier
  super-modifier)

(defvar *character-conversion-table* (make-hash-table :test 'equalp))
(setf (gethash "RETURN" *character-conversion-table*) (char-code #\Return))
(setf (gethash "HYPHEN" *character-conversion-table*) (char-code #\-))
(setf (gethash "ESCAPE" *character-conversion-table*) 27) ;; 27 is ascii for esc

(defun get-char-code (char-string)
  ;; Take a string that represents a character and convert it into
  ;; key code representing it.
  ;; If the char-string does not represent a single character; returns nil
  (let ((character-code (gethash char-string *character-conversion-table* nil))
	(single-char? (= (length char-string) 1)))
    (cond
      (character-code character-code)
      (single-char? (char-code (char char-string 0)))
      (t ()))))

(defun push-key-chord (control-modifier meta-modifier super-modifier key-code)
  ;; Adds a new chord to key-sequence
  ;; For example, it may add C-M-s or C-x
  ;; to a stack which will be consumed by
  ;; consume-key-sequence
  (let ((key-chord (make-key)))
    (when control-modifier
      (setf (key-control-modifier key-chord) t))
    (when meta-modifier
      (setf (key-meta-modifier key-chord) t))
    (when super-modifier
      (setf (key-super-modifier key-chord) t))
    (setf (key-character-code key-chord) key-code)
    (push key-chord *key-sequence-stack*))
  (consume-key-sequence))

(defun consume-key-sequence ()
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let ((key-maps (list
		   *global-map*
		   (keymap (mode *active-buffer*)))))
    (dolist (map key-maps)
      (when (gethash *key-sequence-stack* map)
	;; If not prefix key, consume
	(when (not (equalp (gethash *key-sequence-stack* map) "prefix"))
	  (funcall (gethash *key-sequence-stack* map))
	  (setf *key-sequence-stack* ()))
	(return-from consume-key-sequence t)))
    ;; If we made it to this point, key did not exist, return false,
    ;; allowing the key to be consumed by other widgets
    (setf *key-sequence-stack* ())))

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
  ;; set prefixes (suffixes of reversed list) to "prefix"
  (maplist #'(lambda (key-seq) (setf (gethash (cdr key-seq) mode-map) "prefix"))
	   (reverse key-sequence)))

(defun split-chord (chord-string)
  ;; Take a sequnce like "C-x" or "C-HYPHEN" and convert it into a
  ;; key struct. The symbol after the last - should be either a literal
  ;; character or a sequence characters describing a key code.
  ;; We need to treat the last element of the sequence specially in
  ;; order to have "C-C" be control C.
  (let ((chord-seq (reverse (cl-strings:split chord-string "-")))
	(key (make-key)))
    ;; first get the character code for the last element of the chord string
    (setf (key-character-code key) (get-char-code (car chord-seq)))
    ;; then set the modifiers from the prefix of the chord string
    (loop for modifier-string in (cdr chord-seq) do
      (cond
	((equal "C" modifier-string) (setf (key-control-modifier key) t))
	((equal "M" modifier-string) (setf (key-meta-modifier key) t))
	((equal "S" modifier-string) (setf (key-super-modifier key) t))))
    key))

(defun kbd (key-sequence-string)
  ;; Take a key-sequence-string in the form of "C-x C-s"
  ;; Firstly, break it apart into chords: "C-x" and "C-s"
  ;; Then, break apart the chords into individual keys
  ;; Use those individual keys to create a "key" struct
  ;; that describes the chord. We now have two "keys"
  ;; connect these two keys in a list <key> C-x, <key> C-s
  ;; this is will serve as the key to our key->function map
  (mapcar #'split-chord (cl-strings:split key-sequence-string " ")))
