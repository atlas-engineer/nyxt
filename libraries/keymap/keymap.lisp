(in-package :keymap)

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
`make-key' and `define-key' raise an error when setting a modifier that is not
in this list.")

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
  (and (or (and (not (zerop (key-code key1)))
                (= (key-code key1)
                   (key-code key2)))
           (string= (key-value key1)
                    (key-value key2)))
       (fset:equal? (key-modifiers key1)
                    (key-modifiers key2))
       (eq (key-status key1)
           (key-status key2))))

(declaim (ftype (function (&key (:code integer) (:value string)
                                (:modifiers list) (:status keyword))
                          key) make-key))
(defun make-key (&key (code 0 explicit-code) (value "" explicit-value)
                      modifiers
                      (status :pressed))
  "Modifiers can be either a `modifier' type or a string that will be looked up in `modifier-list'."
  ;; TODO: Display warning on duplicate modifiers?
  (unless (or explicit-code explicit-value)
    (error "One of CODE or VALUE must be given."))
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
     (fset:convert 'fset:set mods)
     status)))

(defmethod fset:compare ((x key) (y key))
  "Needed to user the KEY structure as keys in Fset maps."
  (if (key= x y)
      :equal
      :unequal))

;; TODO: Define conditions.
(declaim (ftype (function (string) key) keyspec->key))
(defun keyspec->key (string)
  "Parse STRING and return a new `key'.
The specifier is expected to be in the form

  MOFIFIERS-CODE/VALUE

MODIFIERS are hyphen-separated modifiers as per `modifier-list'.
CODE/VALUE is either a code that starts with '#' or a key symbol.

Note that '-' or '#' as a last character is supported, e.g. 'control--' and
'control-#' are valid."
  (when (string= string "")
    (error "Empty keyspec"))
  (let* ((last-nonval-hyphen (or (position #\- string :from-end t
                                                      :end (1- (length string)))
                                 -1))
         (code 0)
         (value "")
         (code-or-value (subseq string (1+ last-nonval-hyphen)))
         (rest (subseq string 0 (1+ last-nonval-hyphen)))
         (modifiers (butlast (str:split "-" rest))))
    (when (find "" modifiers :test #'string=)
      (error "Empty modifier(s)"))
    (when (and (<= 2 (length code-or-value))
               (string= (subseq code-or-value (1- (length code-or-value)))
                        "-"))
      (error "Missing key code or value"))
    (if (and (<= 2 (length code-or-value))
             (string= "#" (subseq code-or-value 0 1)))
        (setf code (or (parse-integer code-or-value :start 1 :junk-allowed t)
                       code))
        (setf value code-or-value))
    (make-key :code code :value value :modifiers modifiers)))

(declaim (ftype (function (string) (types:proper-list key)) keyspecs->keys))
(defun keyspecs->keys (spec)
  "Parse SPEC and return corresponding list of keys."
  ;; TODO: Return nil if SPEC is invalid?
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
Parents are ordered by priority, the first parent has highest priority.")
   (default :accessor default
            :initarg :default
            :initform nil
            :type symbol
            :documentation "Default symbol when no binding is found.")))

(declaim (ftype (function (&key (:default symbol) (:parents (types:proper-list keymap))) keymap) make-keymap))
(defun make-keymap (&key default parents)
  ;; We coerce to 'keymap because otherwise SBCL complains "type assertion too
  ;; complex to check: (VALUES KEYMAP::KEYMAP &REST T)."
  (coerce
   (make-instance 'keymap
                  :parents parents
                  :default default
                  ;; We cannot use the standard (make-hash-table :test #'equalp)
                  ;; because then (set "a") and (set "A") would be the same thing.
                  :entries (fset:empty-map default))
   'keymap))

(defun keymap-p (object)
  (typep object 'keymap))

(deftype keyspecs-type ()               ; TODO: Rename to KEYDESC?
  `(satisfies keyspecs->keys))

;; We need a macro to check that bindings are valid at compile time.
;; This is because most Common Lisp implementations or not capable of checking
;; types that use `satisfies' for non-top-level symbols.
;; We can verify this with:
;;
;;   (compile 'foo (lambda () (keymap::define-key keymap "C-x C-f" 'find-file)))
(defmacro define-key (keymap &rest binding-sym-pairs)
  "Bind BINDING to SYM in KEYMAP.
Return KEYMAP.

BINDING is either a `keyspecs-type' or a list of arguments passed to invocations
of `make-key's.

Examples:

  (define-key foo-map \"C-x C-f\" 'find-file)

  (define-key foo-map
              \"C-x C-f\" 'find-file
              \"C-h k\" 'describe-key)

\"C-M-1 x\" on a QWERTY:

  (define-key foo-map '((:code 10 :modifiers (\"C\" \"M\") (:value \"x\"))) 'find-file)

or the shorter:

  (define-key foo-map \"C-M-#1\" 'find-file)"
  ;; The type checking of KEYMAP is done by `define-key*'.
  (loop :for (binding sym . rest) :on binding-sym-pairs :by #'cddr
        :do (check-type binding (or keyspecs-type list)))
  `(progn
     ,@(loop :for (binding sym . rest) :on binding-sym-pairs :by #'cddr
             :collect (list 'define-key* keymap binding sym))
     ,keymap))

(declaim (ftype (function (keymap (or keyspecs-type list) (or symbol keymap))) define-key*))
(defun define-key* (keymap binding sym)
  (let ((keys (if (listp binding)
                  (mapcar (alex:curry #'apply #'make-key) binding)
                  (keyspecs->keys binding))))
    (bind-key keymap keys sym)))

(defun bind-key (keymap keys sym)
  "Recursively bind the KEYS to keymaps starting from KEYMAP.
The last key is bound to SYM.
Return KEYMAP."
  (when (< (length keys) 1)
    (error "Empty key specifier."))
  (if (= (length keys) 1)
      (progn
        (when (fset:@ (entries keymap) (first keys))
          ;; TODO: Notify caller properly?
          (warn "Key was bound to ~a" (fset:@ (entries keymap) (first keys))))
        (setf (fset:@ (entries keymap) (first keys)) sym))
      (let ((submap (fset:@ (entries keymap) (first keys))))
        (unless (keymap-p submap)
          (setf submap (make-keymap))
          (setf (fset:@ (entries keymap) (first keys)) submap))
        (bind-key submap (rest keys) sym)))
  keymap)

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

(declaim (ftype (function (keymap
                           (types:proper-list keys)
                           (types:proper-list keymap))
                          symbol)
                lookup-keys-in-keymap))
(defun lookup-keys-in-keymap (keymap keys visited)
  "Return bound symbol or nil if there is none."
  (when keys
    (let ((hit (fset:@ (entries keymap) (first keys))))
      (when hit
        (if (and (keymap-p hit)
                 (rest keys))
            (the symbol (lookup-key* hit (rest keys) visited))
            hit)))))

(declaim (ftype (function (keymap
                           (types:proper-list keys)
                           (types:proper-list keymap))
                          symbol)
                lookup-translated-keys))
(defun lookup-translated-keys (keymap keys visited)
  "Return the symbol associated to KEYS in KEYMAP.
Return nil if there is non.
Keymap parents are looked up one after the other."
  (let ((sym (lookup-keys-in-keymap keymap keys visited)))
    (unless sym
      (find-if (lambda (map)
                 (setf sym (lookup-key* map keys visited)))
               (parents keymap)))
    sym))

(declaim (ftype (function (keymap
                           (types:proper-list keys)
                           (types:proper-list keymap))
                          symbol)
                lookup-key*))
(defun lookup-key* (keymap keys visited)
  "Internal function, see `lookup-key' for the user-facing function.
VISITED is used to detect cycles."
  (if (find keymap visited)
      (warn "Cycle detected in keymap ~a" keymap)
      (progn
        (let ((sym nil))
          (find-if (lambda (keys)
                     (setf sym (lookup-translated-keys keymap keys (cons keymap visited))))
                   (translate-keys keys))
          sym))))

(declaim (ftype (function (keymap (types:proper-list keys)) symbol) lookup-key))
(defun lookup-key (keymap keys)   ; TODO: Rename to lookup-keys? lookup-binding?
  "Return the symbol associated to keymap.
Return nil if there is none.
First keymap parents are lookup up one after the other.
Then keys translation are looked up one after the other."
  (or (lookup-key* keymap keys '())
      (coerce (default keymap) 'symbol)))

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
