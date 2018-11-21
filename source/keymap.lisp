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

(defvar *key-chord-stack* ()
  "A stack that keeps track of the key chords a user has inputted")

;; A struct used to describe a key-chord
(defstruct key-chord
  key-code
  key-string
  modifiers)

(defvar *character-conversion-table* (make-hash-table :test 'equalp))
(setf (gethash "SPACE" *character-conversion-table*) (char-code #\Space))
(setf (gethash "BACKSPACE" *character-conversion-table*) 127)
(setf (gethash "RETURN" *character-conversion-table*) (char-code #\Return))
(setf (gethash "HYPHEN" *character-conversion-table*) (char-code #\-))
(setf (gethash "ESCAPE" *character-conversion-table*) (char-code #\Esc))

(defun push-key-chord (key-code key-string modifiers)
  ;; Adds a new chord to key-sequence
  ;; For example, it may add C-M-s or C-x
  ;; to a stack which will be consumed by
  ;; consume-key-sequence
  (let ((key-chord (make-key-chord
                    :key-code key-code
                    :key-string key-string
                    :modifiers (when (listp modifiers)
                                 (sort modifiers #'string-lessp)))))
    (print key-chord)
    (push key-chord *key-chord-stack*))
  (if (consume-key-sequence) 1 0))

(defun consume-key-sequence ()
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let ((key-maps (list
		   *global-map*
		   (keymap (mode (active-buffer *interface*))))))
    (dolist (map key-maps)
      (when (gethash *key-chord-stack* map)
	;; If not prefix key, consume
	(when (not (equalp (gethash *key-chord-stack* map) "prefix"))
	  (funcall (gethash *key-chord-stack* map))
	  (setf *key-chord-stack* ()))
	(return-from consume-key-sequence t)))
    ;; If we made it to this point, key did not exist, return false,
    ;; allowing the key to be consumed by other widgets
    (setf *key-chord-stack* ())))

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
    (loop for key-chord-string in (cl-strings:split key-sequence-string " ")
	  ;; Iterate through all keys in chord (hyphen delimited)
	  do (let* ((keys (cl-strings:split key-chord-string "-"))
                    (key-chord (make-key-chord
                                :key-code nil
                                :key-string (last keys) ;; key string map
                                :modifiers (sort (butlast keys) #'string-lessp))))
	       (push key-chord key-sequence)))
    key-sequence))
