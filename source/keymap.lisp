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

(defun serialize-key-chord (key-chord)
  ;; current implementation ignores keycode
  (append (list nil
                (key-chord-key-string key-chord))
          (key-chord-modifiers key-chord)))

(defun look-up-key-chord-stack (key-chords map)
  (let ((key (mapcar #'serialize-key-chord key-chords)))
    (gethash key map)))

(defun |push.input.event| (key-code key-string modifiers x y low-level-data sender)
  ;; Adds a new chord to key-sequence
  ;; For example, it may add C-M-s or C-x
  ;; to a stack which will be consumed by
  ;; |consume.key.sequence|.
  (let ((key-chord (make-key-chord
                    :key-code key-code
                    :key-string key-string
                    :position (list x y)
                    :modifiers (when (listp modifiers)
                                 (sort modifiers #'string-lessp))
                    :low-level-data low-level-data)))
    (push key-chord *key-chord-stack*)
    (if (consume-key-sequence-p sender)
        (|consume.key.sequence| sender)
        (generate-input-event *interface*
                              (gethash sender (windows *interface*))
                              key-chord)))
  t)

(defun consume-key-sequence-p (sender)
  (let* ((active-window (gethash sender (windows *interface*)))
         (active-buffer (active-buffer active-window))
         (local-map (if (minibuffer-active active-window)
                        *minibuffer-mode-map*
                        (keymap (mode active-buffer))))
         ;; TODO: Shouldn't we give higher priority to the buffer keymap?
         (key-maps (list *global-map* local-map)))
    (flet ((is-in-maps? (key-maps)
             (dolist (map key-maps)
               (when (look-up-key-chord-stack *key-chord-stack* map)
                 (return-from is-in-maps? t)))))
      (cond ((minibuffer-active active-window)
             (log:debug "Minibuffer active")
             t)
            ((is-in-maps? key-maps)
             (log:debug "Found in maps")
             t)
            (t (setf *key-chord-stack* ()))))))

(defun |consume.key.sequence| (sender)
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let* ((active-window (gethash sender (windows *interface*)))
         (active-buffer (active-buffer active-window))
         (local-map (if (minibuffer-active active-window)
                        *minibuffer-mode-map*
                        (keymap (mode active-buffer))))
         (key-maps (list *global-map* local-map))
         (serialized-key-stack (mapcar #'serialize-key-chord *key-chord-stack*)))
    (dolist (map key-maps)
      (let ((bound (gethash serialized-key-stack map)))
        (cond ((equalp "prefix" bound)
               (return-from |consume.key.sequence| t))
              (bound
               (progn
                 (log:debug "Key sequence bound")
                 (funcall bound)
                 (setf *key-chord-stack* ())
                 (return-from |consume.key.sequence| t)))
              ((equalp map *minibuffer-mode-map*)
               (progn
                 (log:debug "Insert ~s in minibuffer" (key-chord-key-string
                                                       (first *key-chord-stack*)))
                 (self-insert *minibuffer* (key-chord-key-string
                                            (first *key-chord-stack*)))
                 (setf *key-chord-stack* ())
                 (return-from |consume.key.sequence| t))))))
    (log:debug "Not found in any keymaps")
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
                                :key-string (car (last keys))
                                :modifiers (sort (butlast keys) #'string-lessp))))
               (push (serialize-key-chord key-chord) key-sequence)))
    key-sequence))
