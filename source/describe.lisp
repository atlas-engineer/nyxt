;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun description-constructor (lister)
  "LISTER is a function return a list of symbols from the given packages.
See `sym:package-functions' for an example."
  (lambda (source)
    (delete-duplicates
     (append
      (funcall lister (packages source) (visibility source))
      (funcall lister (internal-visibility-packages source) :internal)
      (funcall lister (external-visibility-packages source) :external)
      (funcall lister (inherited-visibility-packages source) :inherited)))))

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
   (prompter:constructor (alex:required-argument 'prompter:constructor)))
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

(defun has-attributes-method-p (object) ; TODO: Unused?
  "Return non-nil if OBJECT has `prompter:object-attributes' specialization."
  (has-method-p object #'prompter:object-attributes))

(defmethod prompter:object-attributes ((symbol symbol) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(prini-to-string symbol))
    ("Visibility" ,(prini-to-string (sym:symbol-visibility symbol)))
    ("Documentation"
     ,(or (cond
           ((fboundp symbol)
            (first-line (documentation symbol 'function)))
           ((and (find-class symbol nil)
                 (mopu:subclassp (find-class symbol) (find-class 'standard-object)))
            (first-line (documentation symbol 'type)))
           ((find-package symbol)
            (first-line (documentation (find-package symbol) t)))
           (t
            (first-line (documentation symbol 'variable))))
          "")
     nil 4)))

(defmethod prompter:object-attributes ((package package) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(package-name package))
    ("Nicknames" ,(append (package-nicknames package)
                          ;; Old ASDF/UIOP don't know about package-local-nicknames.
                          (ignore-errors (uiop:symbol-call
                                          :uiop :package-local-nicknames package))))
    ("Documentation" ,(or (first-line (documentation package t)) "") nil 4)))

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
   (prompter:constructor (description-constructor #'package-slots)))
  (:export-accessor-names-p t))

(define-class slot-non-nyxt-source (slot-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Slots")))

(define-class slot-internal-source (slot-source describe-internal-source)
  ((prompter:name "Internal Slots")))

(define-class variable-source (describe-nyxt-source)
  ((prompter:name "Variables")
   (prompter:constructor (description-constructor #'sym:package-variables)))
  (:export-accessor-names-p t))

(define-class variable-non-nyxt-source (variable-source describe-non-nyxt-source)
  ((prompter:name "Non-Nyxt Variables")))

(define-class variable-internal-source (variable-source describe-internal-source)
  ((prompter:name "Internal Variables")))

(define-class package-source (prompter:source)
  ((prompter:name "Packages")
   (prompter:constructor (mapcar (compose #'intern #'package-name) (list-all-packages)))))

(define-internal-page-command-global describe-any (&key input)
    (buffer (format nil "*Describe-~a*" input) 'nyxt/help-mode:help-mode)
  "Inspect anything and show it in a help buffer.
When input exists, list all the symbols that may match it.
Otherwise prompt for matches."
  (typecase input
    ((or string null)
     (run-thread "describe-any prompter"
       (let* ((*interactive-p* t)
              (preprocessor (if (uiop:emptyp input)
                                'prompter:delete-inexact-matches
                                'prompter:filter-exact-match))
              (sources
                (list (make-instance
                       'variable-source
                       :actions-on-return (lambda-command describe-variable* (variables)
                                            (describe-variable :variable (first variables)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'variable-non-nyxt-source
                       :actions-on-return (lambda-command describe-variable* (variables)
                                            (describe-variable :variable (first variables)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'variable-internal-source
                       :actions-on-return (lambda-command describe-variable* (variables)
                                            (describe-variable :variable (first variables)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'function-source
                       :actions-on-return (lambda-command describe-function* (functions)
                                            (describe-function :fn (first functions)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'function-non-nyxt-source
                       :actions-on-return (lambda-command describe-function* (functions)
                                            (describe-function :fn (first functions)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'function-internal-source
                       :actions-on-return (lambda-command describe-function* (functions)
                                            (describe-function :fn (first functions)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'command-source
                       :actions-on-return (lambda-command describe-command* (commands)
                                            (describe-command :command (name (first commands))))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'class-source
                       :actions-on-return (lambda-command describe-class* (classes)
                                            (describe-class :class (first classes)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'class-non-nyxt-source
                       :actions-on-return (lambda-command describe-class* (classes)
                                            (describe-class :class (first classes)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'class-internal-source
                       :actions-on-return (lambda-command describe-class* (classes)
                                            (describe-class :class (first classes)))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'slot-source
                       :actions-on-return (lambda-command describe-slot** (slots)
                                            (describe-slot :class (class-sym (first slots))
                                                           :name (name (first slots))))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'slot-non-nyxt-source
                       :actions-on-return (lambda-command describe-slot** (slots)
                                            (describe-slot :class (class-sym (first slots))
                                                           :name (name (first slots))))
                       :filter-preprocessor preprocessor)
                      (make-instance
                       'slot-internal-source
                       :actions-on-return (lambda-command describe-slot** (slots)
                                            (describe-slot :class (class-sym (first slots))
                                                           :name (name (first slots))))
                       :filter-preprocessor preprocessor))))
         (let ((suggestion+action-pairs
                 (and input
                      (loop with result = '()
                            for source in sources
                            do (loop for suggestion in (prompter:suggestions source)
                                     while (< (length result) 2)
                                     when (string-equal input (prompter:attributes-default suggestion))
                                       do (push (list (prompter:value suggestion)
                                                      (prompter:default-action-on-return source))
                                                result))
                            return result))))
           (match suggestion+action-pairs
             ((list (list suggestion action))
              (funcall action (list suggestion)))
             (_ (prompt
                 :prompt "Describe"
                 :input input
                 :sources sources)))))
       (buffer-delete buffer))
     "")
    (symbol
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
            (:dd (:nxref :class-name class :slot input)))))))))

(define-internal-page describe-value
    (&key id)
    (:title "*Help-value*" :page-mode 'nyxt/help-mode:help-mode)
  "Inspect value under ID and show it in a help buffer."
  (sera:and-let* ((id id)
                  (value (inspected-value id)))
    (spinneret:with-html-string
      (:h1 (:raw (escaped-literal-print value)))
      (:dl
       (:dt "Type")
       (:dd (if (sym:class-symbol-p (type-of value))
                (:nxref :class-name (type-of value))
                (prini-to-string (type-of value)))))
      (:p (:raw (value->html value))))))

(define-internal-page-command-global describe-package
    (&key (package
           (prompt1 :prompt "Describe package"
                    :sources 'package-source)))
    (buffer (str:concat "*Help-" (package-name (find-package package)) "*") 'nyxt/help-mode:help-mode)
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
    (buffer (str:concat "*Help-" (symbol-name variable) "*") 'nyxt/help-mode:help-mode)
  "Inspect a variable and show it in a help buffer."
  (let ((*print-case* :downcase))
    (if (boundp variable)
        (spinneret:with-html-string
          (:nstyle (style buffer))
          (:h1 (format nil "~s" variable)) ; Use FORMAT to keep package prefix.
          (:pre (:code (:raw (resolve-backtick-quote-links (documentation variable 'variable)
                                                           (symbol-package variable)))))
          (:h2 "Type")
          (:p (princ-to-string (type-of (symbol-value variable))))
          (:h2 "Current Value:")
          (:button
           :class "button"
           :onclick (ps:ps (nyxt/ps:lisp-eval
                            (:title "change-value")
                            (handler-case
                                (setf variable
                                      (first
                                       (evaluate
                                        (prompt1
                                         :prompt (format nil "Set ~a to" variable)
                                         :sources 'prompter:raw-source))))
                              (prompt-buffer-canceled nil))))
           "Change value")
          (:p (:raw (value->html (symbol-value variable))))
          (:h2 "Describe")
          (:pre (:code (with-output-to-string (s) (describe variable s)))))
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
    (buffer (str:concat "*Help-" (symbol-name function) "*") 'nyxt/help-mode:help-mode)
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (if function
      (let ((input function))
        (flet ((fun-desc (input)
                 (spinneret:with-html-string
                   (:pre (:code (:raw (resolve-backtick-quote-links (documentation input 'function) (symbol-package input)))))
                   (:h2 "Argument list")
                   (:p (:pre (prini-to-string (arglist input) :package (symbol-package input))))
                   (when (sym:command-symbol-p input)
                     (let* ((key-keymap-pairs (nth-value 1 (keymaps:binding-keys input (all-keymaps))))
                            (key-keymapname-pairs (mapcar (lambda (pair)
                                                            (list (first pair)
                                                                  (keymaps:name (second pair))))
                                                          key-keymap-pairs)))
                       (when key-keymapname-pairs
                         (:h2 "Bindings")
                         (:table
                          (:tr
                           (:th "Binding")
                           (:th "Keymap name"))
                          (loop for (binding keymapname) in key-keymapname-pairs
                                collect (:tr (:td binding)
                                             (:td keymapname)))))))
                   #+sbcl
                   (unless (or (macro-function input)
                               (eq 'function (sb-introspect:function-type input)))
                     (:h2 "Type")
                     (:p (:pre (format-function-type (sb-introspect:function-type input)))))
                   (alex:when-let* ((definition (swank:find-definition-for-thing (symbol-function input)))
                                    (not-error-p (null (getf definition :error)))
                                    (file (first (rest (getf definition :location)))))
                     (:h2 (format nil "Source (~a)" file))
                     (:ncode :file file
                       (multiple-value-bind (listing form)
                           (function-lambda-string (symbol-function input))
                         (or form listing))))
                   (:h2 "Describe")
                   (:pre (:code (with-output-to-string (s) (describe (symbol-function input) s))))))
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
                    (:h4 "Argument list")
                    (:p (:pre (prini-to-string (closer-mop:method-lambda-list method)
                                               :package (symbol-package input))))
                    (multiple-value-bind (source form file)
                        (source-for-thing method)
                      (:h2 (format nil "Source (~a)" file))
                      (:ncode :file file
                        (or form source)))))))
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
               (:h2 "Methods")
               (:raw (sera:string-join
                      (mapcar #'method-desc
                              (mopu:generic-function-methods
                               (symbol-function input)))
                      "")))
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
    (buffer (str:concat "*Help-" (symbol-name name) "*") 'nyxt/help-mode:help-mode)
  "Inspect a slot and show it in a help buffer."
  (unless (and class name)
    (let ((slot (prompt1
                 :prompt "Describe slot"
                 :sources '(slot-source
                            slot-non-nyxt-source
                            slot-internal-source))))
      (setf name (name slot)
            class (class-sym slot))
      ""))
  (describe-slot* name class :independent-p t))

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
      (when (user-class-p class)
        (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "configure-slot")
                                  (nyxt::configure-slot slot class :type (getf props :type))))
                 "Configure"))
      (:dl
       (when independent-p
         (:dt "Class")
         (:dd (:a :href (nyxt-url 'describe-class :class class) class)))
       (when (getf props :type)
         (:dt "Type ")
         (:dd (if (or (subtypep (getf props :type) 'standard-object)
                      (subtypep (getf props :type) 'structure-object))
                  (:a :href (nyxt-url 'describe-class
                                      :class (getf props :type))
                      (prini-to-string (getf props :type)))
                  (prini-to-string (getf props :type)))))
       (when (getf props :initform)
         (:dt "Default value")
         (:dd (:ncode (prini-to-string (getf props :initform)))))
       (when (getf props :documentation)
         (:dt "Documentation")
         (:dd (:pre (:code (:raw (resolve-backtick-quote-links
                                  (getf props :documentation) (symbol-package slot)))))))))))

(define-internal-page-command-global describe-class
    (&key
     (class (prompt1
             :prompt "Describe class"
             :sources '(class-source
                        class-non-nyxt-source
                        class-internal-source))))
    (buffer (str:concat "*Help-" (symbol-name class) "*") 'nyxt/help-mode:help-mode)
  "Inspect a class and show it in a help buffer."
  (if (find-class class nil)
      (let* ((slots (safe-sort (class-slots class :visibility :external)))
             (slot-descs (sera:string-join (mapcar (rcurry #'describe-slot* class) slots) ""))
             (*print-case* :downcase))
        (spinneret:with-html-string
          (:nstyle (style buffer))
          (:h1 (symbol-name class) " (" (sera:class-name-of (find-class class)) ")")
          (:pre (:code (:raw (resolve-backtick-quote-links (documentation class 'type) (symbol-package class)))))
          (when (mopu:direct-superclasses class)
            (:h2 "Direct superclasses:")
            (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-superclasses class))
                       collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name)))))
          (when (mopu:direct-subclasses class)
            (:h2 "Direct subclasses:")
            (:ul (loop for class-name in (safe-sort (mapcar #'class-name (mopu:direct-subclasses class)))
                       collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name)))))
          (:h2 "Slots:")
          (:raw slot-descs)
          (:h2 "Methods:")
          (:ul (loop for method in (safe-sort
                                    (remove-if
                                     #'listp (mapcar #'mopu:generic-function-name
                                                     (mopu:generic-functions class))))
                     collect (:li (:a :href (nyxt-url 'describe-function :fn method) method))))
          (:h2 "Source:")
          (multiple-value-bind (source s-expr file)
              (source-for-thing (find-class class))
            (declare (ignore source))
            (:ncode :file file s-expr))
          (:h2 "Describe")
          (:pre (:code (with-output-to-string (s) (describe class s))))))
      (spinneret:with-html-string
        (:nstyle (style buffer))
        (:h2 (format nil "~s" class))
        (:p "Unbound."))))

(define-command-global describe-mode (&key (mode (prompt1 :prompt "Describe mode"
                                                          :sources 'mode-source)))
  "Inspect a mode and show it in a help buffer."
  (when mode
    (describe-class :class mode)))

(define-internal-page describe-bindings (&key (id (id (current-buffer))))
    (:title "*Help-bindings*" :page-mode 'nyxt/help-mode:help-mode)
  "Show a buffer with the list of all known bindings for the current buffer."
  (alex:if-let ((buffer (nyxt::buffers-get id)))
    (spinneret:with-html-string
      (:h1 "Bindings")
      (:p (loop for keymap in (current-keymaps buffer)
                collect (:div
                         (:h3 (keymaps:name keymap))
                         (:table
                          (:tr
                           (:th "Binding")
                           (:th "Command")
                           (:th "Documentation"))
                          (loop for keyspec being the hash-keys
                                  in (keymaps:keymap-with-parents->map keymap)
                                    using (hash-value bound-value)
                                collect (:tr
                                         (:td keyspec)
                                         (:td (typecase bound-value
                                                (sym:command-symbol (:nxref :command bound-value))
                                                (command (:nxref :command (name bound-value)))
                                                (t (prini-to-string bound-value))))
                                         (:td (or (first (sera::lines (documentation bound-value 'function)))
                                                  "")))))))))
    (spinneret:with-html-string
      (:h1 "Bindings")
      (:p (format nil "Buffer with ID ~a does not exist." id)))))

(define-command-global describe-bindings (&key (buffer (current-buffer)))
  "Show a buffer with the list of all known bindings for the current buffer."
  (buffer-load-internal-page-focus 'describe-bindings :id (id buffer)))

(defun describe-key-dispatch (command)
  (unwind-protect
       (describe-command
        :command (typecase command
                   (symbol command)
                   (command (name command))))
    (setf (command-dispatcher (current-window)) #'dispatch-command
          (input-skip-dispatcher (current-window)) #'dispatch-input-skip)))

(defun skip-describe-dispatch (keyspec)
  (declare (ignore keyspec))
  (echo "Canceled describe-key.")
  (setf (command-dispatcher (current-window)) #'dispatch-command
        (input-skip-dispatcher (current-window)) #'dispatch-input-skip))

(define-command describe-key ()
  "Display binding of user-inputted keys."
  (setf (command-dispatcher (current-window)) #'describe-key-dispatch
        (input-skip-dispatcher (current-window)) #'skip-describe-dispatch)
  (echo "Press a key sequence to describe:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Move rest somewhere else?  Maybe too low-level for help.lisp.

(export-always 'system-information)
(defun system-information ()            ; TODO: Rename report-system-information?
  "Return a system information report as a string."
  (labels ((->string (obj) (princ-to-string obj))
           #+quicklisp
           (quicklisp-information ()
             (str:concat
              "Quicklisp dist version: " (getf +quicklisp-build-information+ :dist-version) +newline+
              "Quicklisp client version: " (getf +quicklisp-build-information+ :client-version) +newline+
              "Local project directories: " (->string (getf +quicklisp-build-information+ :local-project-directories)) +newline+
              "Critical dependencies" (->string (getf +quicklisp-build-information+ :critical-dependencies)) +newline+))
           (asdf-information ()
             (str:concat
              "ASDF version: " (getf +asdf-build-information+ :version) +newline+
              "ASDF registries: " (->string asdf:*default-source-registries*) +newline+
              "Critical dependencies: " (->string (getf +asdf-build-information+ :critical-dependencies)) +newline+))
           (guix-information ()       ; TODO: Test in Live Nyxt.
             (getf +guix-build-information+ :version)))
    (str:concat
     "Nyxt version: " +version+ +newline+
     "Renderer: " (name *renderer*) +newline+
     "Operating system kernel: " (software-type) " " (software-version) +newline+
     "Lisp implementation: " (lisp-implementation-type) " " (lisp-implementation-version)
     #+sbcl
     (format nil " (Dynamic space size: ~a)" (sb-ext:dynamic-space-size))
     +newline+

     "Features: " (prin1-to-string *features*) +newline+
     +newline+

     (asdf-information) +newline+

     #+quicklisp
     (str:concat
      (quicklisp-information) +newline+)

     (when (sera:resolve-executable "guix")
       (str:concat "Guix version: " (guix-information) +newline+)))))

(defun dump-command-descriptions (file)
  "Dump the command descriptions as an HTML file."
  (with-open-file (f file :direction :output :if-exists :supersede)
    (format f "~a" (spinneret:with-html-string
                     (:p "Listed below are the current commands, their
                         documentation, and their source. Non-command
                         based features are currently unlisted.")
                     (:h1 "Commands")))
    (format f "~a" (spinneret:with-html-string
                     (:nstyle (lass:compile-and-write '(.nyxt-source :overflow auto)))))
    (format f "~{~a ~%~}"
            (loop for command in (list-commands)
                  collect (spinneret:with-html-string
                            (:details
                             (:summary (format nil "~(~a~)" (symbol-name (name command))))
                             (:p (:pre (documentation command t)))
                             (:pre :class "nyxt-source" (:code (function-lambda-string command)))))))))
