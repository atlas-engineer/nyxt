;;; keymap.lisp --- lisp subroutines for key binding detection

(in-package :next)

(defun prefix ()
  "Dummy function used for prefix bindings."
  nil)

(defun serialize-key-chord (key-chord &key normalize)
  ;; TODO: Make use of keycode?
  ;; TODO: Case opposite to Caps-Lock status?
  "When NORMALIZE is non-nil, remove the shift modifier and upcase the keys."
  (when (and normalize
             (member "s" (key-chord-modifiers key-chord) :test #'string=))
    (setf (key-chord-modifiers key-chord)
          (delete "s" (key-chord-modifiers key-chord) :test #'string=))
    (string-upcase (key-chord-key-string key-chord)))
  (append (list nil
                (key-chord-key-string key-chord))
          (key-chord-modifiers key-chord)))

(defun serialize-key-chord-stack (key-chord-stack &key normalize)
  (mapcar (lambda (k) (serialize-key-chord k :normalize normalize))
          key-chord-stack))

;; TODO: Add override map to the list.
(defun current-keymaps (window)
  "Return the list of (keymap . mode) for the current buffer, ordered by priority."
  (delete-if #'null (mapcar (lambda (mode) (when (keymap mode)
                                             (cons (keymap mode) mode)))
                            (modes (if (minibuffer-active window)
                                       (minibuffer *interface*)
                                       (active-buffer window))))))

(defun look-up-key-chord-stack (window key-chord-stack)
  "Return the function bound to key-chord-stack for current window.
The resulting function wraprs around the method and its associated mode so that
it can be called without argument."
  ;; TODO: Translate shifted keys.
  (let* ((key (serialize-key-chord-stack key-chord-stack))
         (key-normal (serialize-key-chord-stack key-chord-stack :normalize t))
         (fun+mode (loop for (keymap . mode) in (current-keymaps window)
                         for fun = (gethash key keymap)
                         unless fun
                           do (log:debug "Key-chord ~a normalized to ~a" key key-normal)
                              (setf fun (gethash key-normal keymap))
                         when fun
                           return (cons fun mode))))
    (when fun+mode
      (if (eq (first fun+mode) #'prefix)
          (first fun+mode)
          (lambda () (funcall (first fun+mode) (cdr fun+mode)))))))

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
      (let* ((active-window (gethash sender (windows *interface*)))
             (bound-function (look-up-key-chord-stack active-window
                                                      (key-chord-stack *interface*))))
        (cond
          ((eq bound-function #'prefix)
           (log:debug "Prefix binding"))

          ((functionp bound-function)
           (funcall bound-function)
           (log:debug "Key sequence ~a bound to ~a"
                      (serialize-key-chord-stack (key-chord-stack *interface*))
                      bound-function)
           (setf (key-chord-stack *interface*) nil))

          ((minibuffer-active active-window)
           (if (member "R" (key-chord-modifiers (first (key-chord-stack *interface*)))
                       :test #'string=)
               (log:debug "Key released")
               (progn
                 (log:debug "Insert ~s in minibuffer" (key-chord-key-string
                                                       (first (key-chord-stack *interface*))))
                 (insert (key-chord-key-string (first (key-chord-stack *interface*))))))
           (setf (key-chord-stack *interface*) nil))

          (t (%%generate-input-event *interface*
                                     active-window
                                     key-chord)
             (setf (key-chord-stack *interface*) nil))))))
  (values))

(defun define-key (&rest key-command-pairs
                   &key mode (scheme :emacs) keymap
                   &allow-other-keys)
  "Bind KEY to COMMAND.
The KEY command transforms key chord strings to valid key sequences.
When MODE is provided (as a symbol referring to a class name), the binding is
registered into the mode class and all future mode instances will use the
binding.
If MODE and KEYMAP are nil, the binding is registered into root-mode.

If SCHEME is unspecified, it defaults to :EMACS.  SCHEME is only useful together
with MODE, it does not have any effect on KEYMAP.

Examples:

  (define-key \"C-x C-c\" 'kill)
  (define-key \"C-n\" 'scroll-down
              :mode 'document-mode)
  ;; Only affect the first mode of the current buffer:
  (define-key \"C-c C-c\" 'reload
              :keymap (keymap (mode (active-buffer *interface*))))"
  (dolist (key (remove-if-not #'keywordp key-command-pairs))
    (remf key-command-pairs key))
  (flet ((set-key (mode-map key-sequence-string command)
           ;; A sequence of "C-x" "C-s" "C-a" will be broken
           ;; up into three keys for the mode map, these are
           ;; "C-x" "C-s" "C-a" - points to command
           ;; "C-x" "C-s"       - set to "prefix"
           ;; "C-x"             - set to "prefix"
           ;;
           ;; When a key is set to "prefix" it will not
           ;; consume the stack, so that a sequence of keys
           ;; longer than one key-chord can be recorded
           (let ((key-sequence (key key-sequence-string)))
             (setf (gethash key-sequence mode-map) command)
             ;; generate prefix representations
             (loop while key-sequence
                   do (pop key-sequence)
                      (setf (gethash key-sequence mode-map) #'prefix)))))
    (when (and (null mode) (null keymap))
      (setf mode 'root-mode))
    (loop for (key-sequence-string command . rest) on key-command-pairs by #'cddr
          do (when mode
               (setf (get-default mode 'keymap-schemes)
                     (let* ((map-scheme (closer-mop:slot-definition-initform
                                         (find-slot mode 'keymap-schemes)))
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
                       map-scheme)))
             (when keymap
               (set-key keymap key-sequence-string command)))))

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
