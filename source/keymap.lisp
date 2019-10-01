;;; keymap.lisp --- lisp subroutines for key binding detection

(in-package :next)
(annot:enable-annot-syntax)

(defclass keymap ()
  ((table :initarg :table :accessor table)))

@export
(defun make-keymap ()
  "Return an empty keymap."
  (make-instance 'keymap :table (make-hash-table :test 'equal)))

(defmethod set-key ((map keymap) key-sequence-string command)
  "Bind KEY-SEQUENCE-STRING to COMMAND in MAP.

A sequence of \"C-x\" \"C-s\" \"C-a\" will be broken up into three keys for the
mode map, which are

  \"C-x\" \"C-s\" \"C-a\" - points to COMMAND
  \"C-x\" \"C-s\"         - set to \"prefix\"
  \"C-x\"                 - set to \"prefix\"

When a key is set to #'prefix it will not consume the stack, so that a
sequence of keys longer than one key-chord can be recorded."
  (let ((key-sequence (key key-sequence-string)))
    (setf (gethash key-sequence (table map)) command)
    ;; generate prefix representations
    (loop while key-sequence
          do (pop key-sequence)
             (setf (gethash key-sequence (table map)) #'prefix))))

(defmethod get-key ((map keymap) key-sequence)
  (gethash key-sequence (table map)))

(defun prefix ()
  "Dummy function used for prefix bindings."
  nil)

(defun serialize-key-chord (key-chord &key normalize)
  ;; TODO: Make use of keycode?
  ;; TODO: Case opposite to Caps-Lock status?
  "When NORMALIZE is non-nil, remove the shift modifier and upcase the keys."
  (let ((modifiers (copy-list (key-chord-modifiers key-chord))))
    (when (and normalize
               (member-string "s" modifiers))
      (setf modifiers (delete "s" modifiers :test #'string=))
      (string-upcase (key-chord-key-string key-chord)))
    (setf modifiers (delete-if #'str:emptyp modifiers))
    (append (list nil
                  (key-chord-key-string key-chord))
            modifiers)))

(defun serialize-key-chord-stack (key-chord-stack &key normalize)
  (mapcar (lambda (k) (serialize-key-chord k :normalize normalize))
          key-chord-stack))

;; TODO: Ideally, we wouldn't need the serialized intermediary representation
;; and we could remove this function altogether.
(defun stringify (serialized-key-stack)
  "Return string representation of a serialized key-chord stack.
E.g. print ((nil - C) (nil x C)) as \"C-x C--\".
This is effectively the inverse of `serialize-key-chord-stack'."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (serialized-key-chord)
                    (format nil "~{~a~^-~}"
                            (reverse
                             ;; We work over rest to ignore keycode, since it's always
                             ;; nil in this implementation.
                             (rest serialized-key-chord))))
                  (reverse serialized-key-stack))))

(defun current-keymaps (window)
  "Return the list of `keymap' for the current buffer, ordered by priority."
  (let ((buffer (active-buffer window)))
    (when buffer
      (cons (override-map buffer)
            (delete-if #'null (mapcar #'keymap (modes (if (active-minibuffers window)
                                                          (current-minibuffer)
                                                          (active-buffer window)))))))))

(defun look-up-key-chord-stack (window key-chord-stack)
  "Return the function bound to KEY-CHORD-STACK for WINDOW."
  (let* ((key-sequence (serialize-key-chord-stack key-chord-stack))
         (key-sequence-normal (serialize-key-chord-stack key-chord-stack :normalize t)))
    (loop for keymap in (current-keymaps window)
          for fun = (get-key keymap key-sequence)
          unless fun
            do (setf fun (get-key keymap key-sequence-normal))
          when fun
            return fun)))

(declaim (ftype (function (key-chord) boolean) pointer-event-p))
(defun pointer-event-p (key-chord)
  "Return non-nil if key-chord is a pointer event, e.g. a mouton button click."
  ;; See INPUT_IS_NOT_POINTER in platform port.
  (not (= -1 (first (key-chord-position key-chord)))))

(declaim (ftype (function (key-chord) boolean) printable-p))
(defun printable-p (key-chord)
  "Return non-nil if key-chord is printable.
Letters are printable, while function keys or backspace are not."
  ;; See INPUT_IS_PRINTABLE in platform port.
  (= -2 (second (key-chord-position key-chord))))

;; "Add a new key chord to the interface key-chord-stack.
;; For example, it may add C-M-s or C-x to a stack which will be consumed by
;; `consume-key-sequence'."
(dbus:define-dbus-method (core-object push-input-event)
    ((key-code :int32) (key-string :string) (modifiers (:array :string))
     (x :double) (y :double)
     (low-level-data :int32) (sender :string))
    ()
  (:interface +core-interface+)
  (:name "push_input_event")
  (%%push-input-event key-code key-string modifiers x y low-level-data sender)
  (values))

(defun %%push-input-event (key-code key-string modifiers x y low-level-data sender)
  (let ((key-chord (make-key-chord
                    :key-code key-code
                    :key-string key-string
                    :position (list x y)
                    :modifiers (when (listp modifiers)
                                 (sort modifiers #'string-lessp))
                    :low-level-data low-level-data)))
    ;; Don't stack the release key-chords or else pressing "C-x" then "C-+""
    ;; will be understood as "C-x C-R-x C-+ C-R-+".
    (log:debug key-chord)
    (when (or (null (key-chord-stack *interface*))
              (not (member-string "R" (key-chord-modifiers key-chord))))
      (push key-chord (key-chord-stack *interface*))
      (let* ((active-window (gethash sender (windows *interface*)))
             (active-buffer (active-buffer active-window))
             (bound-function (look-up-key-chord-stack active-window
                                                      (key-chord-stack *interface*))))
        (when active-buffer
          (setf (last-key-chords active-buffer) (list key-chord)))
        (cond
          ;; prefix binding
          ((eq bound-function #'prefix)
           (log:debug "Prefix binding"))
          ;; function bound
          ((functionp bound-function)
           (log:debug "Key sequence ~a bound to:"
                      (serialize-key-chord-stack (key-chord-stack *interface*)))
           (funcall bound-function)
           (setf (key-chord-stack *interface*) nil))
          ;; minibuffer is active
          ((active-minibuffers active-window)
           (if (member-string "R" (key-chord-modifiers (first (key-chord-stack *interface*))))
               (progn
                 ;; (log:debug "Key released") ; TODO: This makes the debug trace too verbose.  Middle ground?
                 nil)
               (when (printable-p (first (key-chord-stack *interface*)))
                 (log:debug "Insert ~s in minibuffer" (key-chord-key-string
                                                       (first (key-chord-stack *interface*))))
                 (insert (key-chord-key-string (first (key-chord-stack *interface*))))))
           (setf (key-chord-stack *interface*) nil))
          ;; forward back to the platform port
          ((or (and active-buffer (forward-input-events-p active-buffer))
               (pointer-event-p key-chord))
           ;; forward-input-events-p is NIL in VI normal mode so that we don't
           ;; forward unbound keys, unless it's a pointer (mouse) event.
           ;; TODO: Remove this special case and bind button1 to "self-insert" instead?
           (rpc-generate-input-event
                                     active-window
                                     key-chord)
           (setf (key-chord-stack *interface*) nil))
          (t (setf (key-chord-stack *interface*) nil)))))))

(declaim (ftype (function (&rest t &key (:scheme list) (:keymap keymap) &allow-other-keys)) define-key))
@export
(defun define-key (&rest key-command-pairs
                   &key keymap
                     (scheme :emacs) ; TODO: Deprecated, remove after some version.
                   &allow-other-keys)
  ;; TODO: Add option to define-key over the keymaps of all instantiated modes.
  "Bind KEY to COMMAND.
The KEY command transforms key chord strings to valid key sequences.

Examples:

  ;; Only affect the first mode of the current buffer:
  (define-key \"C-c C-c\" 'reload
              :keymap (getf (keymap-schemes (first (modes (current-buffer)))) :emacs))"
  ;; SBCL complains if we modify KEY-COMMAND-PAIRS directly, so we work on a copy.
  (let ((key-command-pairs-copy (copy-list key-command-pairs)))
    (dolist (key (remove-if (complement #'keywordp) key-command-pairs-copy))
      (remf key-command-pairs-copy key))
    (if keymap
        (loop for (key-sequence-string command . rest) on key-command-pairs-copy by #'cddr
              do (set-key keymap key-sequence-string command))
        (progn
          (log:warn "Calling define-key without specifying a keymap is deprecated.~&Consider moving the definition to the mode definition site or to a hook.")
          (loop for (key-sequence-string command . rest) on key-command-pairs-copy by #'cddr
                do (setf (get-default 'root-mode 'keymap-schemes)
                         (let* ((map-scheme (closer-mop:slot-definition-initform
                                             (find-slot 'root-mode 'keymap-schemes)))
                                ;; REVIEW: The return value of
                                ;; slot-definition-initform should be evaluated, but
                                ;; this only works if it is not a list.  Since we
                                ;; use a property list for the map-scheme, we need
                                ;; to check manually if it has been initialized.  We
                                ;; could make this cleaner by using a dedicated
                                ;; structure for map-scheme
                                (map-scheme (if (ignore-errors (getf map-scheme :emacs))
                                                map-scheme
                                                (eval map-scheme)))
                                (map (or (getf map-scheme scheme)
                                         (make-keymap))))
                           (set-key map key-sequence-string command)
                           (setf (getf map-scheme scheme) map)
                           map-scheme)))))))

(defun key (key-sequence-string)
  "Turn KEY-SEQUENCE-STRING into a sequence of serialized key-chords.
The return value is a list of strings.  The KEY-SEQUENCE-STRING is in the form
of \"C-x C-s\".

Firstly, we break it apart into chords: \"C-x\" and \"C-s\".  Then, we break
apart the chords into individual keys.  We use those individual keys to create a
`key' struct that describes the chord.  We now have two `key's.  We connect
these two keys in a list in reverse (<key C-s> <key C-x>) to
match (key-chord-stack *interface*).

This can serve as the key in the keymap."
  (serialize-key-chord-stack
   (nreverse
    ;; Iterate through all key chords (space delimited)
    (loop for key-chord-string in (str:split " " key-sequence-string)
          for keys = (str:split "-" key-chord-string)
          collect (make-key-chord
                   :key-code nil
                   :key-string (first (last keys))
                   :modifiers (sort (butlast keys) #'string-lessp))))))
