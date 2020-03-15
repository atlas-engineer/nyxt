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

(defmethod fset:compare ((x modifier) (y modifier))
  "Needed to user the KEY structure as keys in Fset maps."
  (fset:compare-lexicographically (modifier-string x) (modifier-string y)))

(defvar +control+ (make-modifier :string "control" :shortcut "C"))
(defvar +meta+ (make-modifier :string "meta" :shortcut "M"))
(defvar +shift+ (make-modifier :string "shift" :shortcut "s"))
(defvar +super+ (make-modifier :string "super" :shortcut "S"))
(defvar +hyper+ (make-modifier :string "hyper" :shortcut "H"))

(defparameter *modifier-list*
  (list +control+ +meta+ +shift+ +super+ +hyper+)
  "List of known modifiers.
`make-key' and `define-key' raise an error when setting a modifier that is not
in this list.")

(deftype key-status-type ()
  `(or (eql :pressed) (eql :released)))

;; Must be a structure so that it can served as a key in a hash-table with
;; #'equalp tests.
(defstruct (key (:constructor %make-key (code value modifiers status))
                (:copier %copy-key))
  (code 0 :type integer) ; TODO: Can a keycode be 0?  I think not, so 0 might be a good non-value.
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

(declaim (ftype (function ((or string modifier)) modifier) modspec->modifier))
(defun modspec->modifier (string-or-modifier)
  "Return the `modifier' corresponding to STRING-OR-MODIFIER."
  (if (modifier-p string-or-modifier)
      string-or-modifier
      (let ((modifier (find-if (alex:curry #'modifier= string-or-modifier) *modifier-list*)))
        (or modifier
            (error "Unknown modifier ~a" string-or-modifier)))))

(declaim (ftype (function ((or (types:proper-list string)
                               fset:wb-set))
                          fset:wb-set)
                modspecs->modifiers))
(defun modspecs->modifiers (strings-or-modifiers)
  "Return the list of `modifier's corresponding to STRINGS-OR-MODIFIERS."
  (if (fset:set? strings-or-modifiers)
      strings-or-modifiers
      (coerce  (fset:convert 'fset:set
                             (delete-duplicates
                              (mapcar #'modspec->modifier strings-or-modifiers)
                              :test #'modifier=))
               'fset:wb-set)))

(declaim (ftype (function (&key (:code integer) (:value string)
                                (:modifiers list) (:status keyword))
                          key)
                make-key))
(defun make-key (&key (code 0 explicit-code) (value "" explicit-value)
                      modifiers
                      (status :pressed))
  "Modifiers can be either a `modifier' type or a string that will be looked up in `*modifier-list*'."
  ;; TODO: Display warning on duplicate modifiers?
  (unless (or explicit-code explicit-value)
    (error "One of CODE or VALUE must be given."))
  (%make-key
   code
   value
   (modspecs->modifiers modifiers)
   status))

(declaim (ftype (function (key &key (:code integer) (:value string)
                               (:modifiers fset:wb-set) (:status keyword))
                          key)
                copy-key))
(defun copy-key (key &key (code (key-code key)) (value (key-value key))
                        (modifiers (key-modifiers key))
                        (status (key-status key)))
  (let ((new-key (%copy-key key)))
    (setf (key-value new-key) value
          (key-code new-key) code
          (key-status new-key) status
          (key-modifiers new-key) (modspecs->modifiers modifiers))
    new-key))

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

MODIFIERS are hyphen-separated modifiers as per `*modifier-list*'.
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

(declaim (ftype (function (string) string) toggle-case))
(defun toggle-case (string)
  "Return the input with reversed case if it has only one character."
  (if (= 1 (length string))
      (let ((down (string-downcase string)))
        (if (string= down string)
            (string-upcase string)
            down))
      string))

(defun translate-remove-shift-toggle-case (keys)
  "With shift, keys without shift and with their key value case reversed:
'shift-a shift-B' -> 'A b'."
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when shift?
      (mapcar (lambda (key)
                (copy-key key :modifiers (fset:less (key-modifiers key) +shift+)
                              :value (toggle-case (key-value key))))
              keys))))

(defun translate-remove-shift (keys)
  "With shift, keys without shift: 'shift-a' -> 'a'."
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when shift?
      (mapcar (lambda (key)
                (copy-key key :modifiers (fset:less (key-modifiers key) +shift+)))
              keys))))

(defun translate-remove-but-first-control (keys)
  "With control, keys without control except for the first key:
'C-x C-c' -> 'C-x c'."
  (let ((control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find)))
    (when control?
      (cons (first keys)
            (mapcar (lambda (key)
                      (copy-key key :modifiers (fset:less (key-modifiers key) +control+)))
                    (rest keys))))))

(defun translate-remove-shift-but-first-control (keys)
  "With control and shift, keys without control except for the first key and
without shift everywhere: 'C-shift-C C-shift-f' -> 'C-C f. "
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find))
        (control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find)))
    (when (and control? shift?)
               (cons (copy-key (first keys)
                               :modifiers (fset:less (key-modifiers (first keys)) +shift+))
                     (mapcar (lambda (key)
                               (copy-key key :modifiers (fset:set-difference (key-modifiers key)
                                                                             (fset:set +control+ +shift+))))
                             (rest keys))))))

(defun translate-remove-shift-but-first-control-toggle-case (keys)
  "With control and shift, keys without control except for the first key and
without shift everywhere: 'C-shift-C C-shift-f' -> 'C-c F. "
  (let ((control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find))
        (shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when (and control? shift?)
               (cons (copy-key (first keys)
                               :value (toggle-case (key-value (first keys)))
                               :modifiers (fset:less (key-modifiers (first keys)) +shift+))
                     (mapcar (lambda (key)
                               (copy-key key
                                         :value (toggle-case (key-value key))
                                         :modifiers (fset:set-difference (key-modifiers key)
                                                                         (fset:set +control+ +shift+))))
                             (rest keys))))))

(defun translate-shift-control-combinations (keys)
  "Return the successive translations of
- `translate-remove-shift-toggle-case'
- `translate-remove-shift'
- `translate-remove-but-first-control'
- `translate-remove-shift-but-first-control'
- `translate-remove-shift-but-first-control-toggle-case'"
  (delete nil
          (mapcar (lambda (translator) (funcall translator keys))
                  (list #'translate-remove-shift-toggle-case
                        #'translate-remove-shift
                        #'translate-remove-but-first-control
                        #'translate-remove-shift-but-first-control
                        #'translate-remove-shift-but-first-control-toggle-case))))

(defvar *default-translator* #'translate-shift-control-combinations
  "Default key translator to use in `keymap' objects.")

;; TODO: Enable override of default and translator in lookup?
(defclass keymap ()
  ((entries :accessor entries
            :initarg :entries
            :initform nil
            :type fset:wb-map
            :documentation
            "Hash table of which the keys are key-chords and the values are a
symbol or a keymap.")
   (parents :accessor parents
            :initarg :parents
            :initform nil
            :type (types:proper-list keymap)
            :documentation "List of parent keymaps.
Parents are ordered by priority, the first parent has highest priority.")
   (default :accessor default
            :initarg :default
            :initform nil
            :type symbol
            :documentation "Default symbol when no binding is found.")
   (translator :accessor translator
               :initarg :translator
               :initform *default-translator*
               :type function
               :documentation "When no binding is found, call this function to
generate new bindings to lookup.  The function takes a list of `key' objects and
returns a list of list of keys.")))

(declaim (ftype (function (&key (:default symbol)
                                (:translator function)
                                (:parents (types:proper-list keymap)))
                          keymap)
                make-keymap))
(defun make-keymap (&key default translator parents)
  ;; We coerce to 'keymap because otherwise SBCL complains "type assertion too
  ;; complex to check: (VALUES KEYMAP::KEYMAP &REST T)."
  (coerce
   (make-instance 'keymap
                  :parents parents
                  :default default
                  :translator (or translator *default-translator*)
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
  (when (uiop:emptyp keys)
    (error "Empty key specifier."))
  (if (= (length keys) 1)
      (progn
        (when (fset:@ (entries keymap) (first keys))
          ;; TODO: Notify caller properly?
          (warn "Key was bound to ~a" (fset:@ (entries keymap) (first keys))))
        (setf (fset:@ (entries keymap) (first keys)) sym))
      (let ((submap (fset:@ (entries keymap) (first keys))))
        (unless (keymap-p submap)
          (setf submap (make-keymap :default (default keymap)
                                    :translator (translator keymap)))
          (setf (fset:@ (entries keymap) (first keys)) submap))
        (bind-key submap (rest keys) sym)))
  keymap)

(declaim (ftype (function (keymap
                           (types:proper-list keys)
                           (types:proper-list keymap))
                          (or symbol keymap))
                lookup-keys-in-keymap))
(defun lookup-keys-in-keymap (keymap keys visited)
  "Return bound symbol or keymap, or nil if there is none."
  (when keys
    (let ((hit (fset:@ (entries keymap) (first keys))))
      (when hit
        (if (and (keymap-p hit)
                 (rest keys))
            (coerce (lookup-key* hit (rest keys) visited) 'symbol)
            hit)))))

(declaim (ftype (function (keymap
                           (types:proper-list keys)
                           (types:proper-list keymap))
                          (or symbol keymap))
                lookup-translated-keys))
(defun lookup-translated-keys (keymap keys visited)
  "Return the symbol or keymap associated to KEYS in KEYMAP.
Return nil if there is none.
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
                          (or symbol keymap))
                lookup-key*))
(defun lookup-key* (keymap keys visited)
  "Internal function, see `lookup-key' for the user-facing function.
VISITED is used to detect cycles."
  (if (find keymap visited)
      (warn "Cycle detected in keymap ~a" keymap)
      (let ((sym nil))
        (find-if (lambda (keys)
                   (setf sym (lookup-translated-keys keymap keys (cons keymap visited))))
                 (cons keys (funcall (or (translator keymap) (constantly nil)) keys)))
        sym)))

(declaim (ftype (function (keymap (types:proper-list keys)) (or symbol keymap)) lookup-key))
(defun lookup-key (keymap keys)   ; TODO: Rename to lookup-keys? lookup-binding?
  "Return the symbol associated to keymap.
Return nil if there is none.
First keymap parents are lookup up one after the other.
Then keys translation are looked up one after the other."
  (or (lookup-key* keymap keys '())
      (coerce (default keymap) 'symbol)))

(defparameter *print-shortcut* t
  "Whether to print the short form of the modifiers.")

(declaim (ftype (function (key) keyspecs-type) key->keyspec))
(defun key->keyspec (key)
  "Return the keyspec of KEY."
  (let ((value (if (zerop (key-code key))
                   (key-value key)
                   (format nil "#~a" (key-code key))))
        (modifiers (fset:reduce (lambda (&rest mods) (str:join "-" mods))
                                (key-modifiers key)
                                :key (if *print-shortcut*
                                         #'modifier-shortcut
                                         #'modifier-string))))
    (coerce (str:concat (if (str:empty? modifiers) "" (str:concat modifiers "-"))
                        value)
            'keyspecs-type)))

(declaim (ftype (function ((types:proper-list key)) keyspecs-type) keys->keyspecs))
(defun keys->keyspecs (keys)
  "Return a keyspecs"
  (coerce (str:join " " (mapcar #'key->keyspec keys)) 'keyspecs-type))

(defun keymap->map* (keymap &optional visited)
  "Return a `fset:map' of (KEYSPEC SYM) from KEYMAP."
  (fset:reduce
   (lambda (result key sym)
     (let ((keyspec (key->keyspec key)))
       (if (keymap-p sym)
           (cond
             ((find sym visited)
              (warn "Cycle detected in keymap ~a" keymap)
              result)
             (t
              (fset:map-union result
                              (fset:image (lambda (subkey subsym)
                                            (values (format nil "~a ~a" keyspec subkey)
                                                    subsym))
                                          (keymap->map* sym (cons sym visited))))))
           (fset:with result keyspec sym))))
   (entries keymap)
   :initial-value (fset:empty-map)))

(defun keymap->map (&rest keymaps)
  "Return a `fset:map' of (KEYSPEC SYM) from KEYMAP.
Parent bindings are not listed; see `keymap-with-parents->map' instead.
This is convenient if the caller wants to list all the bindings.
When multiple keymaps are provided, return the union of the `fset:map' of each arguments.
Keymaps are ordered by precedence, highest precedence comes first."
  (let ((keymaps (reverse keymaps)))
    (reduce #'fset:map-union
            (mapcar #'keymap->map* keymaps))))

(defun keymap-with-parents->map (keymap)
  "List bindings in KEYMAP and all its parents.
See `keymap->map'."
  (labels ((list-keymaps (keymap visited)
             (if (find keymap visited)
                 (progn
                   (warn "Cycle detected in parent keymap ~a" keymap)
                   '())
                 (progn
                   (cons keymap
                         (alex:mappend (alex:rcurry #'list-keymaps (cons keymap visited))
                                       (parents keymap)))))))
    (apply #'keymap->map (list-keymaps keymap '()))))

;; TODO: (command-keys keymap sym) function to return list of keys known for SYM.
;; Support multiple keymaps?

;; TODO: Remap binding, e.g.
;; (define-key *foo-map* (remap 'bar-sym) 'new-sym)

;; TODO: We need a "lookup-key" that goes through a list of keymaps.

;; TODO: Function that composes keymaps.
;; (compose-keymaps ...) => KEYMAP

;; TODO: Add timeout support, e.g. "jk" in less than 0.1s could be ESC in VI-style.
