;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun description-constructor (lister &key (test #'eql))
  "LISTER is a function that returns a list of symbols or objects representing them
from the given packages. TEST is a function that checks for equality for the purpose of
deduplicating the result.
See `sym:package-functions' for an example of LISTER."
  (lambda (source)
    (delete-duplicates
     (append
      (funcall lister (packages source) (visibility source))
      (funcall lister (internal-visibility-packages source) :internal)
      (funcall lister (external-visibility-packages source) :external)
      (funcall lister (inherited-visibility-packages source) :inherited))
     :test test)))

(define-class describe-nyxt-source (prompter:source)
  ((visibility
    :any
    :type (member :internal :external :inherited :any)
    :documentation "Include symbol of this visibility from `packages'.")
   (packages
    (nyxt-user-packages)
    :type (maybe (list-of package))
    :documentation "Include symbols of `visibility' from the given packages.")
   (internal-visibility-packages
    nil
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages.")
   (external-visibility-packages
    (nyxt-packages)
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages.")
   (inherited-visibility-packages
    nil
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages.")
   (prompter:name (alex:required-argument 'prompter:name))
   (prompter:constructor (alex:required-argument 'prompter:constructor))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches))
  (:export-class-name-p nil)              ; Internal class.
  (:export-accessor-names-p t))

(define-class describe-non-nyxt-source (describe-nyxt-source)
  ((packages
    nil
    :type (maybe (list-of package))
    :documentation "Include symbols of `visibility' from the given packages.")
   (external-visibility-packages
    (non-nyxt-packages)
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages."))
  (:export-class-name-p nil)              ; Internal class.
  (:export-accessor-names-p t))

(define-class describe-internal-source (describe-nyxt-source)
  ((packages
    nil
    :type (maybe (list-of package))
    :documentation "Include symbols of `visibility' from the given packages.")
   (internal-visibility-packages
    (nyxt-packages)
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages.")
   (external-visibility-packages
    nil
    :type (maybe (list-of package))
    :documentation "Include internal symbols from the given packages."))
  (:export-class-name-p nil)              ; Internal class.
  (:export-accessor-names-p t))

(define-class function-source (describe-nyxt-source)
  ((prompter:name "Functions")
   (prompter:constructor (description-constructor #'sym:package-functions)))
  (:export-accessor-names-p t))

(define-class function-non-nyxt-source (function-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Functions")))

(define-class function-internal-source (function-source describe-internal-source)
  ((prompter:name "Internal Functions")))

(defun first-line (string)
  "Return first non-empty line in STRING."
  (find-if (complement #'uiop:emptyp) (sera:lines string)))

(defmethod prompter:object-attributes ((symbol symbol) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(prini-to-string symbol) (:width 1))
    ("Documentation" ,(or (cond
                            ((fboundp symbol)
                             (first-line (documentation symbol 'function)))
                            ((and (find-class symbol nil)
                                  (mopu:subclassp (find-class symbol)
                                                  (find-class 'standard-object)))
                             (first-line (documentation symbol 'type)))
                            ((find-package symbol)
                             (first-line (documentation (find-package symbol) t)))
                            (t
                             (first-line (documentation symbol 'variable))))
                          "")
                     (:width 4))
    ("Visibility" ,(prini-to-string (sym:symbol-visibility symbol)) (:width 1))))

;; Note that `package-source' is populated by symbols, not packages.
(defmethod prompter:object-attributes ((package package) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(package-name package) (:width 1))
    ("Documentation" ,(or (first-line (documentation package t)) "") (:width 4))
    ("Nicknames" ,(append (package-nicknames package)
                          ;; Old ASDF/UIOP don't know about package-local-nicknames.
                          (ignore-errors (uiop:symbol-call
                                          :uiop :package-local-nicknames package)))
                 (:width 1))))

(define-class class-source (describe-nyxt-source)
  ((prompter:name "Classes")
   (prompter:constructor (description-constructor #'sym:package-classes)))
  (:export-accessor-names-p t))

(define-class class-non-nyxt-source (class-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Classes")))

(define-class class-internal-source (class-source describe-internal-source)
  ((prompter:name "Internal Classes")))

(define-class slot-source (describe-nyxt-source)
  ((prompter:name "Slots")
   (prompter:constructor
    (description-constructor #'package-slots
                             :test (lambda (slot-a slot-b)
                                     (equal `(,(name slot-a) ,(class-sym slot-a))
                                            `(,(name slot-b) ,(class-sym slot-b)))))))
  (:export-accessor-names-p t))

(define-class slot-non-nyxt-source (slot-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Slots")))

(define-class slot-internal-source (slot-source describe-internal-source)
  ((prompter:name "Internal Slots")))

(defun non-keyword-package-variables (packages visibility)
  (remove-if #'keywordp (sym:package-variables packages visibility)))

(define-class variable-source (describe-nyxt-source)
  ((prompter:name "Variables")
   (prompter:constructor (description-constructor #'non-keyword-package-variables)))
  (:export-accessor-names-p t))

(define-class variable-non-nyxt-source (variable-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Variables")))

(define-class variable-internal-source (variable-source describe-internal-source)
  ((prompter:name "Internal Variables")))

(define-class package-source (prompter:source)
  ((prompter:name "Packages")
   (prompter:constructor (mapcar (compose #'intern #'package-name) (list-all-packages)))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)))

(define-internal-page-command-global describe-any (&key input)
    (buffer (format nil "*Describe-~a*" input) 'nyxt/mode/help:help-mode)
  "Inspect anything and show it in a help buffer.
When input exists, list all the symbols that may match it.
Otherwise prompt for matches."
  (when (symbolp input)
    (spinneret:with-html-string
      (:h1 (princ-to-string input))
      (:p (princ-to-string input)
          " may refer to several things. Please choose the one that you need.")
      (:dl
       (when (boundp input)
         (:dt "Variable")
         (:dd (:nxref :variable input)))
       (cond
         ((sym:mode-symbol-p input)
          (:dt "Mode")
          (:dd (:nxref :mode input)))
         ((sym:class-symbol-p input)
          (:dt "Class")
          (:nxref :class-name input))
         (t nil))
       (cond
         ((sym:command-symbol-p input)
          (:dt "Command")
          (:dd (:nxref :command input)))
         ((sym:macro-symbol-p input)
          (:dt "Macro")
          (:dd (:nxref :macro input)))
         ((sym:function-symbol-p input)
          (:dt "Function")
          (:dd (:nxref :function input))))
       (when (find-package input)
         (:dt "Package")
         (:dd (:nxref :package input)))
       (dolist (class (sym:package-classes (union (nyxt-packages) (list (symbol-package input)))
                                           :external))
         (when (find input (class-slots class))
           (:dt "Slot in " (:nxref :class-name class))
           (:dd (:nxref :class-name class :slot input))))))))

(defmethod describe-any :around (&key (input nil input-supplied-p)
                                   %buffer%)
  (declare (ignorable input %buffer%))
  (cond
    ((and input-supplied-p (symbolp input))
     (call-next-method))
    (t
     (let ((sources
             (list (make-instance
                    'command-source
                    :actions-on-return (lambda-command describe-command* (commands)
                                         (describe-command :command (name (first commands)))))
                   (make-instance
                    'mode-source
                    :actions-on-return (lambda-command describe-command* (modes)
                                         (describe-mode :mode (first modes))))
                   (make-instance
                    'variable-source
                    :actions-on-return (lambda-command describe-variable* (variables)
                                         (describe-variable :variable (first variables))))
                   (make-instance
                    'function-source
                    :actions-on-return (lambda-command describe-function* (functions)
                                         (describe-function :fn (first functions))))
                   (make-instance
                    'class-source
                    :actions-on-return (lambda-command describe-class* (classes)
                                         (describe-class :class (first classes))))
                   (make-instance
                    'slot-source
                    :actions-on-return (lambda-command describe-slot** (slots)
                                         (describe-slot :class (class-sym (first slots))
                                                        :name (name (first slots)))))
                   (make-instance
                    'variable-non-nyxt-source
                    :actions-on-return (lambda-command describe-variable* (variables)
                                         (describe-variable :variable (first variables))))
                   (make-instance
                    'function-non-nyxt-source
                    :actions-on-return (lambda-command describe-function* (functions)
                                         (describe-function :fn (first functions))))
                   (make-instance
                    'class-non-nyxt-source
                    :actions-on-return (lambda-command describe-class* (classes)
                                         (describe-class :class (first classes))))
                   (make-instance
                    'slot-non-nyxt-source
                    :actions-on-return (lambda-command describe-slot** (slots)
                                         (describe-slot :class (class-sym (first slots))
                                                        :name (name (first slots)))))
                   (make-instance
                    'variable-internal-source
                    :actions-on-return (lambda-command describe-variable* (variables)
                                         (describe-variable :variable (first variables))))
                   (make-instance
                    'function-internal-source
                    :actions-on-return (lambda-command describe-function* (functions)
                                         (describe-function :fn (first functions))))
                   (make-instance
                    'class-internal-source
                    :actions-on-return (lambda-command describe-class* (classes)
                                         (describe-class :class (first classes))))
                   (make-instance
                    'slot-internal-source
                    :actions-on-return (lambda-command describe-slot** (slots)
                                         (describe-slot :class (class-sym (first slots))
                                                        :name (name (first slots))))))))
       (prompt
        :prompt "Describe"
        :input input
        :sources sources)))))

(define-internal-page describe-value
    (&key id)
    (:title "*Help-value*" :page-mode 'nyxt/mode/help:help-mode)
  "Inspect value under ID and show it in a help buffer."
  (and-let* ((id id)
             (value (inspected-value id)))
    (spinneret:with-html-string
      (:h1 (:raw (escaped-literal-print value)))
      (:dl
       (:dt "Type")
       (:dd (:pre (if (sym:class-symbol-p (type-of value))
                      (:nxref :class-name (type-of value))
                      (prini-to-string (type-of value))))))
      (:p (:raw (value->html value))))))

(define-internal-page-command-global describe-package
    (&key (package
           (prompt1 :prompt "Describe package"
                    :sources 'package-source)))
    (buffer (str:concat "*Help-" (package-name (find-package package)) "*") 'nyxt/mode/help:help-mode)
  "Inspect a package and show it in a help buffer."
  (let* ((package (find-package package))
         (total-symbols (sym:package-symbols (list package)))
         (external-symbols (sym:package-symbols (list package) :visibility :external)))
    (flet ((package-markup (package)
             (spinneret:with-html
               (:a :href (nyxt-url 'describe-package :package (package-name package))
                   (package-name package)))))
      (spinneret:with-html-string
        (:nstyle (style buffer))
        (:h1 (package-name package))
        (:pre (:code (:raw (resolve-backtick-quote-links (documentation (find-package package) t) package))))
        (:h2 "Symbols:")
        (:ul
         (:li "External: " (length external-symbols))
         (:li "Internal: " (- (length total-symbols) (length external-symbols)))
         (:li "Total: " (length total-symbols)))
        (when (package-use-list package)
          (:h2 "Use list:")
          (:ul
           (dolist (use (safe-sort (package-use-list package) :key #'package-name))
             (:li (package-markup use)))))
        (when (package-used-by-list package)
          (:h2 "Used by list:")
          (:ul
           (dolist (use (safe-sort (package-used-by-list package) :key #'package-name))
             (:li (package-markup use)))))))))

(define-internal-page-command-global describe-variable
    (&key
     (variable (prompt1 :prompt "Describe variable"
                        :sources '(variable-source
                                   variable-non-nyxt-source
                                   variable-internal-source))))
    (buffer (str:concat "*Help-" (symbol-name variable) "*") 'nyxt/mode/help:help-mode)
  "Inspect a variable and show it in a help buffer."
  (let ((*print-case* :downcase))
    (if (boundp variable)
        (spinneret:with-html-string
          (:nstyle (style buffer))
          (:h1 (format nil "~s" variable)) ; Use FORMAT to keep package prefix.
          (:pre (:code (:raw (resolve-backtick-quote-links (documentation variable 'variable)
                                                           (symbol-package variable)))))
          (:h2 "Type")
          (:pre (princ-to-string (type-of (symbol-value variable))))
          (:h2 "Current Value:")
          (:p (:raw (value->html (symbol-value variable))))
          (:nsection
            :title "Describe"
           (:pre (:code (with-output-to-string (s) (describe variable s))))))
        (spinneret:with-html-string
          (:nstyle (style buffer))
          (:h1 (format nil "~s" variable))
          (:p "Unbound")))))

(defun format-function-type (function-type)
  (match function-type
    ((list 'function argument-types return-types)
     (with-output-to-string (s)
       (format s "Argument types: ~s~&" argument-types)
       (format s "Return types: ~s~&" return-types)))))

(define-internal-page-command-global describe-function
    (&key
     (fn (prompt1 :prompt "Describe function"
                  :sources '(function-source
                             function-non-nyxt-source
                             function-internal-source)))
     ;; This is to have a full-word alternative to :fn for those that prefer it.
     (function fn))
    (buffer (str:concat "*Help-" (symbol-name function) "*") 'nyxt/mode/help:help-mode)
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (if function
      (let ((input function))
        (flet ((fun-desc (input)
                 (spinneret:with-html-string
                   (:pre (:code (:raw (resolve-backtick-quote-links (documentation input 'function) (symbol-package input)))))
                   (when (sym:command-symbol-p input)
                     (let* ((key-keymap-pairs (nth-value 1 (keymaps:pretty-binding-keys input (all-keymaps) :print-style (keymaps:name (keyscheme buffer)))))
                            (key-keymapname-pairs (mapcar (lambda (pair)
                                                            (list (first pair)
                                                                  (keymaps:name (second pair))))
                                                          key-keymap-pairs)))
                       (when key-keymapname-pairs
                         (:nsection
                           :title "Bindings"
                           (:table
                            (:tr
                             (:th "Binding")
                             (:th "Keymap name"))
                            (loop for (binding keymapname) in key-keymapname-pairs
                                  collect (:tr (:td binding)
                                               (:td keymapname))))))))
                   (:nsection
                     :title "Argument list"
                     (:pre (:code (prini-to-string (trivial-arguments:arglist input)
                                                   :package (symbol-package input)))))
                   #+sbcl
                   (unless (or (macro-function input)
                               (eq 'function (sb-introspect:function-type input)))
                     (:nsection
                       :title "Type"
                       (:p (:pre (format-function-type (sb-introspect:function-type input))))))
                   (:nsection
                     :title "Describe"
                     (:pre (:code (with-output-to-string (s) (describe (symbol-function input) s)))))))
               (method-desc (method)
                 (spinneret:with-html-string
                   (:details
                    (:summary
                     (:h3 :style "display: inline"
                          (format nil "~s" input) " "
                          (:raw (format
                                 nil "(~{~a~^ ~})"
                                 (mapcar (lambda (class)
                                           (cond
                                             ((ignore-errors (mopu:subclassp class 'standard-object))
                                              (spinneret:with-html-string
                                                (:a :href (nyxt-url 'describe-class
                                                                    :class (class-name class))
                                                    (prini-to-string (class-name class)))))
                                             ((ignore-errors (eq t (class-name class)))
                                              "t")
                                             (t (nyxt::escaped-literal-print class))))
                                         (mopu:method-specializers method))))))
                    (:button
                     :class "button"
                     :onclick (ps:ps (nyxt/ps:lisp-eval (:buffer buffer :title "describe-function")
                                                        (remove-method (closer-mop:method-generic-function method)
                                                                       method)
                                                        (reload-current-buffer)))
                     "Remove method")
                    (:pre (:code (:raw (resolve-backtick-quote-links
                                        (documentation method 't) (symbol-package (mopu:method-name method))))))
                    (:nsection
                      :level 4
                      :title "Argument list"
                      (:pre (:code (prini-to-string (closer-mop:method-lambda-list method)
                                                    :package (symbol-package input)))))))))
          (spinneret:with-html-string
            (:nstyle (style buffer))
            (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                 (cond
                   ((macro-function input) " (macro)")
                   ((sym:command-symbol-p input)
                    " (command)")
                   ((typep (symbol-function input) 'generic-function)
                    " (generic function)")))
            (cond
              ((not (fboundp input))
               (:p "Unbound."))
              ((typep (symbol-function input) 'generic-function)
               (:raw (fun-desc input))
               (unless (sym:command-symbol-p input)
                 (:nsection
                   :title "Methods"
                   (:raw (sera:string-join
                          (mapcar #'method-desc
                                  (mopu:generic-function-methods
                                   (symbol-function input)))
                          "")))))
              (t
               (:raw (fun-desc input)))))))
      (prompt :prompt "Describe function"
              :sources 'function-source)))

(define-command-global describe-command
    (&key (command (name (prompt1 :prompt "Describe command"
                                  :sources 'command-source))))
  "Inspect a command and show it in a help buffer.
A command is a special kind of function that can be called with
`execute-command' and can be bound to a key."
  (when command
    (describe-function :fn command)))

(define-internal-page-command-global describe-slot
    (&key class name)
    (buffer (str:concat "*Help-" (symbol-name name) "*") 'nyxt/mode/help:help-mode)
  "Inspect a slot and show it in a help buffer."
  (if (and class name)
      (describe-slot* name class :independent-p t)
      (let ((slot (prompt1
                   :prompt "Describe slot"
                   :sources '(slot-source
                              slot-non-nyxt-source
                              slot-internal-source))))
        (describe-slot :class (class-sym slot) :name (name slot))
        "")))

(defun describe-slot* (slot class &key independent-p)
  "Create the HTML that represents a slot."
  ;; TODO: Adapt HTML sections / lists to describe-slot and describe-class.
  ;; TODO: Parse docstrings and highlight code samples.
  (let ((props (mopu:slot-properties (find-class class) slot))
        (*package* (symbol-package slot)))
    (spinneret:with-html-string
      (if independent-p
          (:h1 (prini-to-string slot))
          (:h3 (prini-to-string slot)))
      (:dl
       (when independent-p
         (:dt "Class")
         (:dd (:pre (:a :href (nyxt-url 'describe-class :class class) class))))
       (when (getf props :type)
         (:dt "Type ")
         (:dd (:pre (if (or (subtypep (getf props :type) 'standard-object)
                            (subtypep (getf props :type) 'structure-object))
                        (:a :href (nyxt-url 'describe-class
                                            :class (getf props :type))
                            (prini-to-string (getf props :type)))
                        (prini-to-string (getf props :type))))))
       (when (getf props :initform)
         (:dt "Default value")
         (:dd (:ncode (prini-to-string (getf props :initform)))))
       (when (getf props :documentation)
         (:dt "Documentation")
         (:dd (:pre (:code (:raw (resolve-backtick-quote-links
                                  (getf props :documentation) (symbol-package slot))))))))
      (unless independent-p
        (:br "")))))

(define-internal-page-command-global describe-class
    (&key
     (class (prompt1
             :prompt "Describe class"
             :sources '(class-source
                        class-non-nyxt-source
                        class-internal-source))))
    (buffer (str:concat "*Help-" (symbol-name class) "*") 'nyxt/mode/help:help-mode)
  "Inspect a class and show it in a help buffer."
  (if (find-class class nil)
      (let* ((slots (safe-sort (class-slots class :visibility :external)))
             (slot-descs (sera:string-join (mapcar (rcurry #'describe-slot* class) slots) ""))
             (*print-case* :downcase)
             (mode-p (subtypep class 'mode)))
        (spinneret:with-html-string
          (:nstyle (style buffer))
          (:h1 (symbol-name class) " (" (sera:class-name-of (find-class class)) ")")
          (:pre (:code (:raw (resolve-backtick-quote-links (documentation class 'type) (symbol-package class)))))
          ;; TODO: Show mode keybindings for a better mode help (would be a
          ;; killer one)? We'd need to do some hack to inspect the keybindings
          ;; from the class somehow. Maybe :allocation :class so that keymap is
          ;; allocated/modified in place?
          ;; REVIEW: Maybe (make-instance MODE-NAME)?
          (:nsection
            :title "Slots"
            (:raw slot-descs))
          (when mode-p
            (:nsection
              :title "Commands"
              (:ul
               (dolist (command (sym:package-commands (symbol-package class)))
                 (:li (:nxref :command command))))))
          (when-let ((methods (safe-sort
                               (remove-if
                                #'listp (mapcar #'mopu:generic-function-name
                                                (mopu:generic-functions class))))))
            (:nsection
              :title "Methods"
              (:ul (loop for method in methods
                         collect (:li (:a :href (nyxt-url 'describe-function :fn method) method))))))
          (when (mopu:direct-superclasses class)
            (:nsection
              :title "Direct superclasses"
              (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-superclasses class))
                         collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name))))))
          (when (mopu:direct-subclasses class)
            (:nsection
              :title "Direct subclasses"
              (:ul (loop for class-name in (safe-sort (mapcar #'class-name (mopu:direct-subclasses class)))
                         collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name))))))
          (:nsection
            :title "Describe"
            (:pre (:code (with-output-to-string (s) (describe class s)))))))
      (spinneret:with-html-string
        (:nstyle (style buffer))
        (:h2 (format nil "~s" class))
        (:p "Unbound."))))

(define-command-global describe-mode (&key (mode (prompt1 :prompt "Describe mode"
                                                          :sources 'mode-source)))
  "Inspect a mode and show it in a help buffer."
  (when mode
    (describe-class :class mode)))

(-> pretty-keyspec (string string) (values string &optional))
(defun pretty-keyspec (keyspec print-style)
  "Return KEYSPEC written in PRINT-STYLE.
\"cua\" spells modifiers the same way `nkeymaps:pretty-binding-keys' does.
Any other style keeps the keyspec."
  (if (string= print-style "cua")
      (str:replace-all
       "M-" #-(or darwin win32) "Alt+" #+darwin "Option+" #+win32 "Alt+"
       (str:replace-all
        "S-" #-(or darwin win32) "Super+" #+darwin "Command+" #+win32 "Win+"
        (str:replace-all
         "s-" #-(or darwin win32) "Shift+" #+darwin "Shift+" #+win32 "Shift+"
         (str:replace-all
          "C-" #-(or darwin win32) "Ctrl+" #+darwin "Ctrl+" #+win32 "Ctrl+"
          keyspec))))
      keyspec))

(-> keymap-section-title (string string) (values string &optional))
(defun keymap-section-title (name scheme-name)
  "Return a short heading for keymap NAME.
\"prompt-buffer-mode-emacs-map\" with scheme \"emacs\" becomes \"Prompt buffer\"."
  (let* ((schemes (sort (remove-if #'uiop:emptyp
                                   (remove-duplicates
                                    (remove nil (list scheme-name
                                                      "vi-normal" "vi-insert"
                                                      "emacs" "default" "cua"))
                                    :test #'string=))
                        #'> :key #'length))
         (stem (if (str:ends-with-p "-map" name)
                   (subseq name 0 (- (length name) 4))
                   name))
         (stem (or (loop for scheme in schemes
                         for suffix = (format nil "-~a" scheme)
                         when (str:ends-with-p suffix stem)
                           return (subseq stem 0 (- (length stem) (length suffix))))
                   stem))
         (stem (if (str:ends-with-p "-mode" stem)
                   (subseq stem 0 (- (length stem) 5))
                   stem))
         (words (str:split "-" stem :omit-nulls t)))
    (if words
        (format nil "~@(~a~)~{ ~a~}" (first words) (rest words))
        name)))

(-> binding-symbol (t) (values (or symbol null) &optional))
(defun binding-symbol (bound-value)
  "Return the symbol BOUND-VALUE names."
  (typecase bound-value
    (command (name bound-value))
    (symbol bound-value)))

(-> binding-label (t) (values string &optional))
(defun binding-label (bound-value)
  "Return the name to show for BOUND-VALUE."
  (let ((symbol (binding-symbol bound-value)))
    (if symbol
        (string-downcase (symbol-name symbol))
        (prini-to-string bound-value))))

(-> binding-summary (t) (values string &optional))
(defun binding-summary (bound-value)
  "Return the first documentation line of BOUND-VALUE."
  (or (when-let ((symbol (binding-symbol bound-value)))
        (documentation-line symbol 'function nil))
      ""))

(-> binding-target (t) (values (or string null) &optional))
(defun binding-target (bound-value)
  "Return the help page URL for BOUND-VALUE."
  (when-let ((symbol (binding-symbol bound-value)))
    (nyxt-url 'describe-function :fn symbol)))

;; Buffers can't be passed as argument since Nyxt URLs don't handle unreadable
;; objects.
(define-internal-page describe-bindings (&key (buffer-id (id (current-buffer))))
    (:title "*Help-bindings*" :page-mode 'nyxt/mode/help:help-mode)
  "Show a list of all available keybindings for buffer corresponding to BUFFER-ID."
  ;; One table, grouped by keymap. Rows are a single 44px line: command,
  ;; shortcut, description. 17px separates a group label from its rows and
  ;; gutters the columns. 37px separates groups. Hairlines sit on the row
  ;; edge and drop out under the hover, the way a Safari table does.
  (if-let ((buffer (buffer-get buffer-id)))
    (if (input-buffer-p buffer)
        (let ((print-style (keymaps:name (keyscheme buffer))))
          (labels ((binding-pairs (keymap)
                     "Return KEYMAP's bindings as (KEYSPEC . VALUE) pairs, sorted by command."
                     (sort (alex:hash-table-alist (keymaps:keymap-with-parents->map keymap))
                           #'string<
                           :key (lambda (pair)
                                  (format nil "~a ~a"
                                          (binding-label (cdr pair))
                                          (car pair)))))
                   (binding-row (keyspec bound-value)
                     "Present one binding as a command, its shortcut, and its summary."
                     (let* ((label (binding-label bound-value))
                            (summary (binding-summary bound-value))
                            (key (pretty-keyspec keyspec print-style)))
                       (spinneret:with-html
                         (:tr
                          (:td :class "command"
                               (:a :href (binding-target bound-value)
                                   :title (if (str:emptyp summary) label summary)
                                   label))
                          (:td :class "shortcut" key)
                          (:td :class "detail" summary)))))
                   (groups ()
                     "Return (TITLE PAIRS) for every keymap that has bindings."
                     (loop for keymap in (current-keymaps buffer)
                           for pairs = (binding-pairs keymap)
                           when pairs
                             collect (list (keymap-section-title
                                            (or (keymaps:name keymap) "")
                                            print-style)
                                           pairs))))
            (spinneret:with-html-string
              (:nstyle
                '("table.bindings"
                  :width "100%"
                  :margin "0"
                  :border "none"
                  :border-collapse "separate"
                  :border-spacing "0"
                  :table-layout "fixed")
                `("table.bindings th, table.bindings td"
                  :border "none"
                  :border-radius "0"
                  :background-color "transparent"
                  :color ,theme:on-background-color
                  :font-size "17px"
                  :font-weight "400"
                  :line-height "24px"
                  :text-align "left"
                  :vertical-align "middle"
                  :padding "0"
                  :overflow "hidden"
                  :text-overflow "ellipsis"
                  :white-space "nowrap")
                `("table.bindings thead th"
                  :position "sticky"
                  :top "0"
                  :z-index "1"
                  :height "32px"
                  :padding "0 17px 7px 0"
                  :font-size "11px"
                  :font-weight "600"
                  :line-height "17px"
                  :color ,theme:primary-color
                  :background-color ,theme:background-color
                  :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
                '("table.bindings thead th:last-child"
                  :padding-right "0")
                '("table.bindings thead th:nth-child(1)"
                  :width "26%")
                '("table.bindings thead th:nth-child(2)"
                  :width "22%")
                `("table.bindings tbody td"
                  :height "44px"
                  :padding-right "17px"
                  :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
                '("table.bindings tbody td:last-child"
                  :padding-right "0")
                '("table.bindings tbody tr:last-child td"
                  :box-shadow "none")
                '("table.bindings tbody tr:has(+ tr.group) td"
                  :box-shadow "none")
                `("table.bindings tbody tr:not(.group):hover td"
                  :background-color ,theme:background-color-
                  :box-shadow "none")
                '("table.bindings tbody tr:not(.group):has(+ tr:not(.group):hover) td"
                  :box-shadow "none")
                '("table.bindings tbody tr:not(.group):hover td:first-child"
                  :border-top-left-radius "7px"
                  :border-bottom-left-radius "7px")
                '("table.bindings tbody tr:not(.group):hover td:last-child"
                  :border-top-right-radius "7px"
                  :border-bottom-right-radius "7px")
                `("table.bindings td.command a"
                  :display "block"
                  :overflow "hidden"
                  :text-overflow "ellipsis"
                  :white-space "nowrap"
                  :color ,theme:on-background-color
                  :text-decoration "none"
                  :opacity "1")
                '("table.bindings td.command a:hover"
                  :opacity "1")
                `("table.bindings td.command a:focus-visible"
                  :outline "2px solid"
                  :outline-color ,theme:action-color
                  :outline-offset "2px")
                '("table.bindings td.shortcut"
                  :font-weight "600")
                `("table.bindings td.detail"
                  :color ,theme:primary-color)
                `("table.bindings tr.group th"
                  :height "auto"
                  :padding "37px 0 7px"
                  :border-radius "0"
                  :font-size "17px"
                  :font-weight "600"
                  :line-height "24px"
                  :color ,theme:on-background-color
                  :background-color "transparent"
                  :box-shadow "none"
                  :overflow "visible"
                  :white-space "normal")
                '("table.bindings tbody tr.group:first-of-type th"
                  :padding-top "17px")
                '("table.bindings tr:last-of-type td"
                  :border-radius "0")
                '("table.bindings th:first-of-type, table.bindings th:last-of-type"
                  :border-radius "0"))
              (:h1 "Bindings")
              (if-let ((groups (groups)))
                (:table :class "bindings"
                  (:thead
                   (:tr
                    (:th "Command")
                    (:th "Shortcut")
                    (:th "Description")))
                  (:tbody
                   (dolist (group groups)
                     (:tr :class "group"
                          (:th :colspan 3 :scope "rowgroup" (first group)))
                     (dolist (pair (second group))
                       (binding-row (car pair) (cdr pair))))))
                (:p "This buffer has no keybindings.")))))
        (spinneret:with-html-string
          (:h1 "Bindings")
          (:p "This buffer has no keybindings.")))
    (spinneret:with-html-string
      (:h1 "Bindings")
      (:p (format nil "Buffer with ID ~a does not exist." buffer-id)))))

(define-command-global describe-bindings (&key (buffer (current-buffer)))
  "Show a list of all available keybindings in the current buffer."
  (buffer-load-internal-page-focus 'describe-bindings :buffer-id (id buffer)))

(defun describe-key-dispatch (command)
  ;; TODO: Show when something is NOT bound!
  (unwind-protect (describe-command :command (typecase command
                                               (symbol command)
                                               (command (name command))))
    (setf (command-dispatcher *browser*) #'dispatch-command)
    (echo-dismiss)))

(define-command describe-key ()
  "Display binding of user-inputted keys."
  (setf (command-dispatcher *browser*) #'describe-key-dispatch)
  (echo "Press a key sequence to describe:"))

(export-always 'system-information)
(defun system-information ()
  (str:concat
   "Nyxt version: " +version+ +newline+
   "Web Renderer: " (name *renderer*) +newline+
   "OS: " (software-type) " " (software-version) +newline+
   "Lisp implementation: " (lisp-implementation-type) " " (lisp-implementation-version)
   #+sbcl
   (format nil " (Dynamic space: ~aMB)~%" (/ (sb-ext:dynamic-space-size) 1024 1024))
   "ASDF version: " (asdf:asdf-version) +newline+
   "Features: " (prin1-to-string *features*)))
