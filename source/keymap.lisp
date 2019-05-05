;;; keymap.lisp --- lisp subroutines for key binding detection

;;; keys are defined with the following syntax:
;;;
;;; (define-key (key "C-x o") #'function-reference)
;;;
;;; in the previous example, the sequence of keys:
;;; "control+x", "o" would invoke the "function-reference"

(in-package :next)

(defun serialize-key-chord (key-chord)
  ;; current implementation ignores keycode
  (append (list nil
                (key-chord-key-string key-chord))
          (key-chord-modifiers key-chord)))

(defun look-up-key-chord-stack (key-chords map)
  (let ((key (mapcar #'serialize-key-chord key-chords)))
    (gethash key map)))

(defun |push.input.event| (key-code key-string modifiers x y low-level-data sender)
  "Add a new key chord to the interface key-chord-stack.
For example, it may add C-M-s or C-x to a stack which will be consumed by
`|consume.key.sequence|'."
  (let ((key-chord (make-key-chord
                    :key-code key-code
                    :key-string key-string
                    :position (list x y)
                    :modifiers (when (listp modifiers)
                                 (sort modifiers #'string-lessp))
                    :low-level-data low-level-data)))
    ;; Don't stack the release key-chords or else pressing "C-x" then "C-+""
    ;; will be understood as "C-x C-R-x C-+ C-R-+".
    (when (or (null (key-chord-stack *interface*))
              (not (member "R" (key-chord-modifiers key-chord)
                           :test #'string=)))
      (push key-chord (key-chord-stack *interface*))
      (if (consume-key-sequence-p sender)
          (|consume.key.sequence| sender)
          (%%generate-input-event *interface*
                                  (gethash sender (windows *interface*))
                                  key-chord))))
  t)

;; TODO: If we move the global mode to root-mode, how do we make sure which one has priority?
;; What about reserving a prefix key for application mode and root mode?
(defun consume-key-sequence-p (sender)
  (let* ((active-window (gethash sender (windows *interface*)))
         (active-buffer (active-buffer active-window))
         (local-map (if (minibuffer-active active-window)
                        (keymap (mode (minibuffer *interface*)))
                        (keymap (mode active-buffer))))
         ;; TODO: Shouldn't we give higher priority to the buffer keymap?
         (key-maps (list (root-mode-default-keymap) local-map)))
    (flet ((is-in-maps? (key-maps)
             (dolist (map key-maps)
               (when (look-up-key-chord-stack (key-chord-stack *interface*) map)
                 (return-from is-in-maps? map)))))
      (cond ((minibuffer-active active-window)
             (log:debug "Minibuffer active")
             t)
            ((is-in-maps? key-maps)
             (log:debug "~a found in map ~a" (mapcar #'serialize-key-chord (key-chord-stack *interface*))
                        (is-in-maps? key-maps))
             t)
            (t (setf (key-chord-stack *interface*) ()))))))

(defun |consume.key.sequence| (sender)
  ;; Iterate through all keymaps
  ;; If key recognized, execute function
  (let* ((active-window (gethash sender (windows *interface*)))
         (active-buffer (active-buffer active-window))
         (local-map (if (minibuffer-active active-window)
                        (keymap (mode (minibuffer *interface*)))
                        (keymap (mode active-buffer))))
         (key-maps (list (root-mode-default-keymap) local-map))
         (serialized-key-stack (mapcar #'serialize-key-chord (key-chord-stack *interface*))))
    (dolist (map key-maps)
      (let ((bound-function (gethash serialized-key-stack map)))
        (cond ((equalp "prefix" bound-function)
               (return-from |consume.key.sequence| t))
              (bound-function
               (progn
                 (log:debug "Key sequence ~a bound to ~a" serialized-key-stack bound-function)
                 (funcall bound-function (mode (if (minibuffer-active active-window)
                                                   (minibuffer *interface*)
                                                   active-buffer)))
                 (setf (key-chord-stack *interface*) ())
                 (return-from |consume.key.sequence| t)))
              ((equalp map (keymap (mode (minibuffer *interface*))))
               (if (member "R" (key-chord-modifiers (first (key-chord-stack *interface*)))
                           :test #'string=)
                   (log:debug "Key released")
                   (progn
                     (log:debug "Insert ~s in minibuffer" (key-chord-key-string
                                                           (first (key-chord-stack *interface*))))
                     (insert (key-chord-key-string (first (key-chord-stack *interface*))))))
               (setf (key-chord-stack *interface*) ())
               (return-from |consume.key.sequence| t)))))
    (log:debug "Not found in any keymaps")
    (setf (key-chord-stack *interface*) ())))

(defun define-key (&rest key-command-pairs
                         &key mode keymap
                         &allow-other-keys)
  "Bind KEY to COMMAND.
The KEY command transforms key chord strings to valid key sequences.
When MODE is provided (as a symbol referring to a class name), the binding is
registered into the mode class and all future mode instance will use the
binding.
If MODE and KEYMAP are nil, the binding is registered into root-mode.

Examples:

  (define-key (\"C-x C-c\") 'kill)
  (define-key (\"C-n\") 'scroll-down
              :mode 'document-mode)
  ;; Only affect the first mode of the current buffer:
  (define-key (\"C-c C-c\") '
              :keymap (keymap (mode (active-buffer *interface*))))"
  (remf key-command-pairs :mode)
  (remf key-command-pairs :keymap)
  (flet ((set-key (mode-map key-sequence command)
           ;; A sequence of "C-x" "C-s" "C-a" will be broken
           ;; up into three keys for the mode map, these are
           ;; "C-x" "C-s" "C-a" - points to command
           ;; "C-x" "C-s"       - set to "prefix"
           ;; "C-x"             - set to "prefix"
           ;;
           ;; When a key is set to "prefix" it will not
           ;; consume the stack, so that a sequence of keys
           ;; longer than one key-chord can be recorded
           (setf (gethash key-sequence mode-map) command)
           ;; generate prefix representations
           (loop while key-sequence
                 do (pop key-sequence)
                    (setf (gethash key-sequence mode-map) "prefix"))))
    (when (and (null mode) (null keymap))
      (setf mode 'root-mode))
    (loop for (key-sequence command . rest) on key-command-pairs by #'cddr
          do (when mode
               (set-default
                mode 'keymap
                (let ((map (eval (closer-mop:slot-definition-initform
                                  (find-slot mode 'keymap)))))
                  (set-key map key-sequence command)
                  map)))
             (when keymap
               (set-key keymap key-sequence command)))))

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
