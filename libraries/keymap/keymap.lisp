(in-package :next-keymap)

(defstruct modifier
  (string "" :type string)
  (shortcut "" :type string))

(defun modifier= (string-or-modifier1 string-or-modifier2)
  (unless (or (modifier-p string-or-modifier1)
              (modifier-p string-or-modifier2))
    (error "At least one of the arguments must be a modifier."))
  (flet ((match-modifier (modifier string)
           (or (string= (modifier-string modifier) string)
               (string= (modifier-shortcut modifier) string))))
    (cond
      ((stringp string-or-modifier1)
       (match-modifier string-or-modifier2 string-or-modifier1))
      ((stringp string-or-modifier2)
       (match-modifier string-or-modifier1 string-or-modifier2))
      (t
       (or (string= (modifier-string string-or-modifier1)
                    (modifier-string string-or-modifier2))
           (string= (modifier-shortcut string-or-modifier1)
                    (modifier-shortcut string-or-modifier2)))))))

(defvar +control+ (make-modifier :string "control" :shortcut "C"))
(defvar +meta+ (make-modifier :string "meta" :shortcut "M"))
(defvar +shift+ (make-modifier :string "shift" :shortcut "s"))
(defvar +super+ (make-modifier :string "super" :shortcut "S"))
(defvar +hyper+ (make-modifier :string "hyper" :shortcut "H"))

(defparameter modifier-list
  (list +control+ +meta+ +shift+ +super+ +hyper+)
  "List of known modifiers.
`make-key-chord' and `define-key' will raise an error if you try setting a
modifier that is not in this list.")

(deftype key-status-type ()
  `(or (eql :pressed) (eql :released)))

;; Must be a structure so that it can served as a key in a hash-table with
;; #'equalp tests.
(defstruct (key (:constructor %make-key (code value modifiers status)))
  (code 0 :type integer)
  (value "" :type string)
  (modifiers (fset:set) :type fset:wb-set)
  (status :pressed :type key-status-type))

(defun key= (key1 key2)
  "Two keys are equal if the have the same modifiers, status and key code.
If codes don't match, the values are compared instead.  This way, code-matching
keys match before the value which is usually what the users want when they
specify a key-code binding."
  (and (or (= (key-code key1)
              (key-code key2))
           (string= (key-value key1)
                    (key-value key2)))
       (equalp (key-modifiers key1)
               (key-modifiers key2))
       (eq (key-status key1)
           (key-status key2))))

(defun make-key (&key code value modifiers status) ; TODO: make-key-chord from modifier structure?
  "Modifiers can be either a `modifier' type or a string that will be looked up in `modifier-list'."
  ;; TODO: Display warning on duplicate modifiers.
  ;; Better: Make set.
  (unless (or code value)
    (error "One of CODE or VALUE must be givven."))
  (let ((mods (when modifiers
                (delete-duplicates
                 (mapcar (lambda (mod)
                           (if (modifier-p mod)
                               mod
                               (let ((modifier (find-if (alex:curry #'modifier= mod) modifier-list)))
                                 (or modifier
                                     (error "Unknown modifier ~a" mod)))))
                         modifiers)
                 :test #'equalp))))
    (%make-key
     code
     value
     (fset:convert 'set mods)
     status)))

(defun keyspec->key (string)
  (let* ((tokens (str:split "-" string))
         (modifiers (butlast tokens))
         (value (first (last tokens))))
    (make-key :value value :modifiers modifiers)))

(declaim (ftype (function (string) list) keyspecs->keys))
(defun keyspecs->keys (spec)
  "Return list of keys.
Return nil if SPEC is invalid."
  ;; TODO: Return nil?
  (let* ((result (str:split " " spec :omit-nulls t)))
    (mapcar #'keyspec->key result)))

(defclass keymap ()
  ((entries :accessor entries
            :initarg :entries
            :initform nil
            :type hash-table
            :documentation
            "Hash table of which the keys are key-chords and the values are a
symbol or a keymap.")
   (parents :accessor parents
            :initarg :parents
            :initform nil
            :type list
            :documentation "List of parent keymaps.
The first parent has highest priority.")))


;; (declaim (ftype (function (&rest t) keymap) make-keymap)) ; TODO: Fix type.
(defun make-keymap (&rest parents)
  (make-instance 'keymap
                 :parents parents
                 ;; We cannot use the standard (make-hash-table :test #'equalp)
                 ;; because then it (set "a") and (set "A") would be the same thing.
                 :entries (fset:map)))

(defun keymap-p (object)
  (typep object 'keymap))

(deftype keyspecs-type ()               ; TODO: Rename to KEYDESC?
  `(satisfies keyspecs->keys))

;; TODO: Test if this does compile time type-checking when called inside lets.
;; If not, turn it to a macro.
;; TODO: Parse keycode with #.
(declaim (ftype (function (keymap (or keyspecs-type list) symbol)) define-key))
(defun define-key (keymap binding sym)
  "BINDING is either a `keyspecs-type' or a list of arguments passed to invocations of `make-key's.

Examples:

  (define-key foo-map \"C-x C-f\" 'find-file)

\"C-M-1 x\" on a QWERTY:

  (define-key foo-map '((:code 10 :modifiers (\"C\" \"M\") (:value \"x\")) 'find-file)"
  (let ((keys (if (listp binding)
                  (mapcar (alex:curry #'apply #'make-key) binding)
                  (keyspecs->keys binding))))
    (bind-key keymap keys sym)))

(defun bind-key (keymap keys sym)
  "Recursively bind the KEYS to keymaps starting from KEYMAP.
The last key is bound to SYM."
  (when (< (length keys) 1)
    (error "Empty key specifier."))
  (if (= (length keys) 1)
      (progn
        (when (fset:@ (entries keymap) (first keys))
          ;; TODO: Notify caller properly.
          (warn "Key was bound to ~a" (fset:@ (entries keymap) (first keys))))
        (setf (fset:@  (entries keymap) (first keys)) sym))
      (let ((submap (fset:@ keymap (first keys))))
        (unless (keymap-p submap)
          (setf submap (make-keymap))
          (setf (fset:@ keymap (first keys)) submap))
        (bind-key submap (rest keys) sym))))


(declaim (ftype (function (standard-char) standard-char) toggle-case))
;; TODO: Use toggle-case in translate-keys.
(defun toggle-case (char)               ; TODO: Should we deal with strings or chars?
  "Return the input with reversed case. "
  (let ((down (char-downcase char)))
    (if (char= down char)
        (char-upcase char)
        down)))

;; TODO: Make this customizable.  Inside keymap?  Default to global parameter.
;; Enable override in lookup?
(defun translate-keys (keys)
  "Return list of bindings that can be derived from KEYS.
The returned list contains KEYS as the first element."
  ;; Reverse case when shift is present.
  ;; Remove shift (without changing case) when other modifiers are there.  E.g. C-shift-a can be translated to C-A, C-a.
  ;; For last key-chord, toggle Control.
  ;; TODO: Finish translate-keys.
  (list keys))

(defun lookup-key-chord (keymap keys)
  "Return nil when no hit."
  (unless (null keys)
    (let ((hit (fset:@ keymap (first keys))))
      (when hit
        (if (and (keymap-p hit)
                 (rest keys))
            (lookup-key-chord hit (rest keys))
            ;; TODO: Return keymap or nil?
            hit)))))

(defun lookup-translated-keys (keymap keys)
  "Return the symbol associated to KEYS in KEYMAP.
Keymap parents are looked up one after the other."
  (let ((sym (lookup-key-chord keymap keys)))
    (or sym
        (loop for parent-keymap in (parents keymap)
              for sym = (lookup-translated-keys parent-keymap keys)
              when sym
              return sym))))

;; TODO: Catch cycles in lookups.
(defun lookup-key (keymap keys)   ; TODO: Rename to lookup-keys? lookup-binding?
  "Return the symbol associated to keymap.
Keymap parents are looked up one after the other."
  (let ((translations (translate-keys keys)))
    (loop for translated-key-chord in translations
          for sym = (lookup-translated-keys keymap translated-key-chord)
          when sym
          return sym)))

(defun key->keyspec (key)
  "Warning: Only KEY value is supported."
  (format nil "~a~a"
          (if (and (key-modifiers key) (not (fset:empty? (key-modifiers key))))
              (str:join (str:join "-" (fset:convert 'list (key-modifiers key)))
                        "-")
              "")
          (if (zerop (key-code key))    ; TODO: Can a keycode be 0?
              (key-value key)
              (format nil "#~a" (key-code key)))))

;; (declaim (ftype (function (list) string) keys->keyspecs)) ; TODO: Fix type
(defun keys->keyspecs (keys)
  "Return a keyspecs"
  (str:join " " (mapcar #'key->keyspec keys)))

;; (defun keys->string (keys)
;;   ;; TODO: Implement keys->string?  Do we have a 1-1 mapping between keyspecs and keys?
;;   )

;; TODO: Do we need a keymap->list function?
