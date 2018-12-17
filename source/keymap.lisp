;;; keymap.lisp --- lisp subroutines for key binding detection

;;; keymaps are executed in priority from most specific to least
;;; that is, the order of execution for keymaps is:
;;; *global-map* --> major-mode-map --> minor-mode-maps
;;;
;;; keys are defined with the following syntax:
;;; (define-key *global-map* (key "C-x o") #'function-reference)
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

(defun serialize-key-chord (key-chord)
  (let ((*package* (find-package 'next)))
    (format nil "~a" key-chord)))

(defun look-up-key-chord-stack (key-chords map)
  (let ((key (mapcar #'serialize-key-chord key-chords)))
    (gethash key map)))

(defun push-key-event (key-code key-string modifiers sender)
  ;; Adds a new chord to key-sequence
  ;; For example, it may add C-M-s or C-x
  ;; to a stack which will be consumed by
  ;; consume-key-sequence
  (declare (ignore key-code)) ;; current implementation ignores keycode
  (let ((key-chord (make-key-chord
                    :key-code nil
                    :key-string key-string
                    :modifiers (when (listp modifiers)
                                 (sort modifiers #'string-lessp)))))
    (push key-chord *key-chord-stack*))
  (if (consume-key-sequence-p sender) 1 0))

(defun consume-key-sequence-p (sender)
  (let* ((active-buffer (active-buffer (gethash sender (windows *interface*))))
         (key-maps (list *global-map* (keymap (mode active-buffer)))))
    (flet ((is-in-maps? (key-maps)
             (dolist (map key-maps)
               (when (look-up-key-chord-stack *key-chord-stack* map)
                 (return-from is-in-maps? t)))))
      (cond ((eql active-buffer *minibuffer*) t)
            ((is-in-maps? key-maps) t)
            (t (setf *key-chord-stack* ()))))))

(defun consume-key-sequence (sender)
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let* ((active-buffer (active-buffer (gethash sender (windows *interface*))))
         (key-maps (list *global-map* (keymap (mode active-buffer))))
         (serialized-key-stack (mapcar #'serialize-key-chord *key-chord-stack*)))
    (dolist (map key-maps)
      (let ((bound (gethash serialized-key-stack map)))
        (cond ((equalp "prefix" bound)
               (return-from consume-key-sequence t))
              (bound
               (progn
                 (funcall bound)
                 (setf *key-chord-stack* ())
                 (return-from consume-key-sequence t)))
              ((equalp map *minibuffer-mode-map*)
               (progn
                 (self-insert *minibuffer* (key-chord-key-string
                                            (first *key-chord-stack*)))
                 (setf *key-chord-stack* ())
                 (return-from consume-key-sequence t))))))
    ;; not found in any key-maps
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

(defun key (key-sequence-string)
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
                                :key-string (gethash (car (last keys))
                                                     *character-conversion-table*
                                                     (car (last keys)))
                                :modifiers (sort (butlast keys) #'string-lessp))))
	       (push (serialize-key-chord key-chord) key-sequence)))
    key-sequence))
