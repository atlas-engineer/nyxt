;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class function-source (prompter:source)
  ((universal nil)
   (prompter:name "Functions")
   (prompter:constructor (lambda (source)
                           (package-functions
                            (when (universal source)
                              (list-all-packages))))))
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun first-line (string)
  "Return first non-empty line in STRING."
  (find-if (complement #'uiop:emptyp) (sera:lines string)))

(defmethod prompter:object-attributes ((symbol symbol))
  `(("Name" ,(write-to-string symbol))
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
          ""))))

(defmethod prompter:object-attributes ((package package))
  `(("Name" ,(package-name package))
    ("Nicknames" ,(princ-to-string
                   (append (package-nicknames package)
                           ;; Old ASDF/UIOP don't know about package-local-nicknames.
                           (ignore-errors (uiop:symbol-call
                                           :uiop :package-local-nicknames package)))))
    ("Documentation" ,(or (first-line (documentation package t)) ""))))

(define-class class-source (prompter:source)
  ((universal nil)
   (prompter:name "Classes")
   (prompter:constructor (lambda (source)
                           (package-classes
                            (when (universal source)
                              (list-all-packages))))))
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class slot-source (prompter:source)
  ((universal nil)
   (prompter:name "Slots")
   (prompter:constructor (lambda (source)
                           (package-slots
                            (when (universal source)
                              (list-all-packages))))))
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class variable-source (prompter:source)
  ((universal nil)
   (prompter:name "Variables")
   (prompter:constructor (lambda (source)
                           (package-variables
                            (when (universal source)
                              (list-all-packages))))))
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class package-source (prompter:source)
  ((prompter:name "Packages")
   (prompter:constructor (mapcar (alex:compose #'intern #'package-name) (list-all-packages)))))

(define-command describe-any (&optional input universal)
  "Inspect anything and show it in a help buffer.
When INPUT  has a unique exact match in the sources, describe it
directly without prompting.
When INPUT does not have a unique match, prompt for the list of exact matches."
  (let* ((preprocessor (if (uiop:emptyp input)
                           'prompter:delete-inexact-matches
                           'prompter:filter-exact-match))
         (sources (list (make-instance 'variable-source
                                       :actions (list (make-command describe-variable* (variables)
                                                        (describe-variable :variable (first variables))))
                                       :filter-preprocessor preprocessor
                                       :universal universal)
                        (make-instance 'function-source
                                       :actions (list (make-command describe-function* (functions)
                                                        (describe-function :function (first functions))))
                                       :filter-preprocessor preprocessor
                                       :universal universal)
                        (make-instance 'command-source
                                       :actions (list (make-command describe-command* (commands)
                                                        (describe-command :command (name (first commands)))))
                                       :filter-preprocessor preprocessor
                                       :universal universal)
                        (make-instance 'class-source
                                       :actions (list (make-command describe-class* (classes)
                                                        (describe-class :class (first classes))))
                                       :filter-preprocessor preprocessor
                                       :universal universal)
                        (make-instance 'slot-source
                                       :actions (list (make-command describe-slot** (slots)
                                                        (describe-slot :class (class-sym (first slots))
                                                                       :name (name (first slots)))))
                                       :filter-preprocessor preprocessor
                                       :universal universal))))
    (let ((suggestion+action-pairs
            (and input
                 (loop with result = '()
                       for source in sources
                       do (loop for suggestion in (prompter:suggestions source)
                                while (< (length result) 2)
                                when (string-equal input (prompter:attributes-default suggestion))
                                  do (push (list (prompter:value suggestion)
                                                 (prompter:default-action source))
                                           result))
                       return result))))
      (match suggestion+action-pairs
        ((list (list suggestion action))
         (funcall action (list suggestion)))
        (_ (prompt
            :prompt "Describe:"
            :input input
            :sources sources))))))

(define-command-global universal-describe-any ()
  "Inspect anything from any package and show it in a help buffer."
  (describe-any nil t))

(define-internal-page-command-global describe-value
    (&key id)
    (buffer "*Help-value*" 'nyxt/help-mode:help-mode)
  "Inspect value under ID and show it in a help buffer."
  (sera:and-let* ((id id)
                  (value (inspected-value id)))
    (spinneret:with-html-string
      (:h1 (:raw (escaped-literal-print value)))
      (:p (:raw (value->html value))))))

(defun has-attributes-method-p (object)
  "Return non-nil if OBJECT has `prompter:object-attributes' specialization."
  (has-method-p object #'prompter:object-attributes))

(defun resolve-backtick-quote-links (string parent-symbol)
  (flet ((resolve-regex (target-string start end match-start match-end reg-starts reg-ends)
           (declare (ignore start end reg-starts reg-ends))
           ;; Excluding backtick & quote.
           (let* ((name (subseq target-string (1+ match-start) (1- match-end)))
                  (symbol (let ((*package* (symbol-package parent-symbol)))
                            (ignore-errors (read-from-string name nil))))
                  (url (when symbol
                         (ps:ps (nyxt/ps:lisp-eval `(nyxt::describe-any ,(princ-to-string symbol)))))))
             (let ((*print-pretty* nil))
               ;; Disable pretty-printing to avoid spurious space insertion within links:
               ;; https://github.com/ruricolist/spinneret/issues/37#issuecomment-884740046
               (spinneret:with-html-string
                 (if url
                     (:a :href (javascript-url url)
                         (:code name))
                     (:code name)))))))
    (if string
        ;; FIXME: Spaces are disallowed, but |one can use anything in a symbol|.
        ;; Maybe allow it?  The problem then is that it increases the chances of
        ;; false-positives when the "`" character is used for other reasons.
        (spinneret:with-html-string
          (:pre
           (:code (:raw (ppcre:regex-replace-all "`[^'\\s]+'" string #'resolve-regex)))))
        "")))

(define-internal-page-command-global describe-package
    (&key (package
           (prompt1
             :prompt "Describe package:"
             :sources (make-instance 'package-source))))
    (buffer (str:concat "*Help-" (package-name package) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a package and show it in a help buffer."
  (let ((total-symbols (package-defined-symbols nil (list package)))
        (external-symbols (package-defined-symbols (list package)))
        (*print-case* :downcase))
    (flet ((package-markup (package)
             (spinneret:with-html
                 (:a :href (nyxt-url 'describe-package :package (package-name package))
                     (package-name package)))))
      (spinneret:with-html-string
          (:style (style buffer))
        (:h1 (package-name package))
        (:raw (resolve-backtick-quote-links (documentation (find-package package) t) package))
        (:h2 "Symbols:")
        (:ul
         (:li "External: " (length external-symbols))
         (:li "Internal: " (- (length total-symbols) (length external-symbols)))
         (:li "Total: " (length total-symbols)))
        (:h2 "Use list:")
        (:ul
         (dolist (use (package-use-list package))
           (:li (package-markup use))))
        (:h2 "Used by list:")
        (:ul
         (dolist (use (package-used-by-list package))
           (:li (package-markup use))))))))

(define-internal-page-command-global describe-variable
    (&key
     universal
     (variable
      (prompt1
        :prompt "Describe variable:"
        :sources (make-instance 'variable-source :universal universal))))
    (buffer (str:concat "*Help-" (symbol-name variable) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a variable and show it in a help buffer."
  (let ((*print-case* :downcase))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (format nil "~s" variable)) ; Use FORMAT to keep package prefix.
      (:raw (resolve-backtick-quote-links (documentation variable 'variable) variable))
      (:h2 "Current Value:")
      (:button
       :class "button"
       :onclick (ps:ps (nyxt/ps:lisp-eval
                        `(handler-case
                             (setf ,variable
                                   (first
                                    (evaluate
                                     (prompt1
                                       :prompt (format nil "Set ~a to" (quote ,variable))
                                       :sources (make-instance 'prompter:raw-source)))))
                           (nyxt-prompt-buffer-canceled nil))))
       "Change value")
      (:p (:raw (value->html (symbol-value variable)))))))

(define-command-global universal-describe-variable ()
  "Inspect a variable from any Nyxt-accessible package and show it in a help buffer."
  (describe-variable :universal t))

(define-internal-page-command-global describe-function
    (&key
     universal
     (function (prompt1
                 :prompt "Describe function"
                 :sources (make-instance 'function-source :universal universal))))
    (buffer (str:concat "*Help-" (symbol-name function) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (if function
      (let ((input function)
            (*print-case* :downcase))
        (flet ((method-desc (method)
                 (spinneret:with-html-string
                   (:h1 (format nil "~s" input) " "
                        (:raw (format
                               nil "(~{~a~^ ~})"
                               (mapcar (lambda (class)
                                         (cond
                                           ((ignore-errors (mopu:subclassp class 'standard-object))
                                            (spinneret:with-html-string
                                              (:a :href (nyxt-url 'describe-class
                                                                  :class (class-name class))
                                                  (write-to-string (class-name class)))))
                                           ((ignore-errors (eq t (class-name class)))
                                            "t")
                                           (t (nyxt::escaped-literal-print class))))
                                       (mopu:method-specializers method)))))
                   (:raw (resolve-backtick-quote-links (documentation method 't)
                                                       (mopu:method-name method)))
                   (:h2 "Argument list")
                   (:p (write-to-string (closer-mop:method-lambda-list method)))
                   (alex:when-let* ((definition (swank:find-definition-for-thing method))
                                    (not-error-p (null (getf definition :error)))
                                    (file (rest (getf definition :location)))
                                    (location (alex:assoc-value (rest definition) :snippet)))
                     (:h2 (format nil "Source ~a" file))
                     (:pre (first location))))))
          (if (typep (symbol-function input) 'generic-function)
              (spinneret:with-html-string
                (:style (style buffer))
                (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                     (when (macro-function input) " (macro)"))
                (:raw (resolve-backtick-quote-links (documentation input 'function) input))
                (:raw (apply #'str:concat (mapcar #'method-desc
                                                  (mopu:generic-function-methods
                                                   (symbol-function input))))))
              (spinneret:with-html-string
                (:style (style buffer))
                (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                     (when (macro-function input) " (macro)"))
                (:raw (resolve-backtick-quote-links (documentation input 'function) input))
                (:h2 "Argument list")
                (:p (write-to-string (mopu:function-arglist input)))
                #+sbcl
                (unless (macro-function input)
                  (:h2 "Type")
                  (:p (format nil "~s" (sb-introspect:function-type input))))
                (alex:when-let* ((definition (swank:find-definition-for-thing (symbol-function input)))
                                 (not-error-p (null (getf definition :error)))
                                 (file (rest (getf definition :location)))
                                 (location (alex:assoc-value (rest definition) :snippet)))
                  (:h2 (format nil "Source ~a" file))
                  (:pre (first location)))))))
      (prompt
       :prompt "Describe function"
       :sources (make-instance 'function-source))))

(define-command-global universal-describe-function ()
  "Inspect a function from any Nyxt-accessible package and show it in a help buffer."
  (describe-function :universal t))

(define-internal-page-command-global describe-command
    (&key (command (name (prompt1
                           :prompt "Describe command"
                           :sources (make-instance 'command-source)))))
    (buffer (str:concat "*Help-" (symbol-name command) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a command and show it in a help buffer.
A command is a special kind of function that can be called with
`execute-command' and can be bound to a key."
  (let* ((command (find command (list-commands) :key #'name))
         (key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                         command
                                         (all-keymaps))))
         (key-keymapname-pairs (mapcar (lambda (pair)
                                         (list (first pair)
                                               (keymap:name (second pair))))
                                       key-keymap-pairs))
         (source-file
           (alex:when-let ((location (getf (swank:find-definition-for-thing (fn command))
                                           :location)))
             (alex:last-elt location)))
         (*print-case* :downcase))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (symbol-name (name command))
           (unless (eq (find-package :nyxt)
                       (symbol-package (name command)))
             (format nil " (~a)"
                     (package-name (symbol-package (name command))))))
      (:p (:raw
           ;; TODO: This only displays the first method,
           ;; i.e. the first command of one of the modes.
           ;; Ask for modes instead?
           (resolve-backtick-quote-links (documentation (fn command) t)
                                         (swank-backend:function-name (fn command)))))
      (:h2 "Bindings")
      (:p (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
      (:h2 (format nil "Source~a: " (if source-file
                                        (format nil " (~a)" source-file)
                                        "")))
      (:pre (:code (let ((*print-case* :downcase))
                     (write-to-string (sexp command))))))))

(define-internal-page-command-global describe-slot
    (&key class name universal)
    (buffer (str:concat "*Help-" (symbol-name name) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a slot and show it in a help buffer."
  (unless (and class name)
    (let ((slot (prompt1
                  :prompt "Describe slot"
                  :sources (make-instance 'slot-source :universal universal))))
      (setf name (name slot)
            class (class-sym slot))
      ""))
  (describe-slot* name class :mention-class-p t))

(define-command-global universal-describe-slot ()
  "Inspect a Nyxt-accessible slot and show it in a help buffer."
  (describe-slot :universal t))

(defun describe-slot* (slot class &key mention-class-p)
  "Create the HTML that represents a slot."
  ;; TODO: Adapt HTML sections / lists to describe-slot and describe-class.
  ;; TODO: Parse docstrings and highlight code samples.
  (let ((props (mopu:slot-properties (find-class class) slot))
        (*print-case* :downcase))
    (spinneret:with-html-string
      (:ul
       (:li (symbol-name slot))
       (:ul
        (when mention-class-p
          (:li (format nil "Class: ~s" class)))
        (when (getf props :type)
          (:li
           (:raw (format nil "Type: ~a"
                         (if (and (subtypep (getf props :type) 'standard-object))
                             (spinneret:with-html-string
                               (:a :href (nyxt-url 'describe-class
                                                   :class (getf props :type))
                                   (getf props :type))
                              (getf props :type)))))))
        (when (getf props :initform)
          (let* ((initform-string (let ((*print-case* :downcase))
                                    (write-to-string (getf props :initform))))
                 (multiline-form? (search +newline+ initform-string)))
            (if multiline-form?
                (:li "Default value: " (:pre (:code initform-string)))
                (:li "Default value: " (:code initform-string)))))
        (when (getf props :documentation)
          (:li "Documentation: " (:raw (resolve-backtick-quote-links
                                        (getf props :documentation) slot))))
        (when (user-class-p class)
          (:li (:button :class "button"
                        :onclick (ps:ps (nyxt/ps:lisp-eval
                                         `(nyxt::configure-slot ',slot ',class :type ',(getf props :type))))
                        "Configure"))))))))

(define-internal-page-command-global describe-class
    (&key
     universal
     (class (prompt1
              :prompt "Describe class"
              :sources (make-instance 'class-source :universal universal))))
    (buffer (str:concat "*Help-" (symbol-name class) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a class and show it in a help buffer."
  (let* ((slots (class-public-slots class))
         (slot-descs (apply #'str:concat (mapcar (alex:rcurry #'describe-slot* class) slots)))
         (*print-case* :downcase))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (symbol-name class))
      (:p (:raw (resolve-backtick-quote-links (documentation class 'type) class)))
      (when (mopu:direct-superclasses class)
        (:h2 "Direct superclasses:")
        (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-superclasses class))
                   collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name)))))
      (when (mopu:direct-subclasses class)
        (:h2 "Direct subclasses:")
        (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-subclasses class))
                   collect (:li (:a :href (nyxt-url 'describe-class :class class-name) class-name)))))
      (:h2 "Slots:")
      (:raw slot-descs)
      (:h2 "Methods:")
      (:ul (loop for method in (remove-if
                                #'listp (mapcar #'mopu:generic-function-name
                                                (mopu:generic-functions class)))
                 collect (:li (:a :href (nyxt-url 'describe-function :function method) method)))))))

(define-command-global universal-describe-class ()
  "Inspect a Nyxt-accessible class and show it in a help buffer."
  (describe-class :universal t))

;; FIXME: Arglist used to have prompt-buffer, but it's not URL-serializable.
;; Maybe have prompt-buffers have IDs so that we can identify those by IDs?
;; How do we actually identify prompt-buffers?
(define-internal-page-command nyxt/prompt-buffer-mode::describe-prompt-buffer ()
    (buffer (str:concat "*Help-" (prompter:prompt (current-prompt-buffer)) "-prompter*")
            'nyxt/help-mode:help-mode)
  "Describe a prompt buffer instance."
  (let* ((prompt-buffer (current-prompt-buffer))
         (modes (modes prompt-buffer))
         (sources (prompter:sources prompt-buffer)))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (prompter:prompt prompt-buffer))
      (:p (:raw (resolve-backtick-quote-links (documentation 'prompt-buffer 'type) 'prompt-buffer)))
      (:h2 "Modes:")
      (:ul
       (loop for mode in modes
             collect (:li (:a :href
                              (nyxt-url
                               'describe-class
                               :class (sera:class-name-of mode))
                              (string (sera:class-name-of mode))))))
      (:h2 "Sources:")
      (:ul
       (loop for source in sources
             collect (:li (:a :href
                              (nyxt-url
                               'describe-class
                               :class (sera:class-name-of source))
                              (string (sera:class-name-of source)))))))))

(defun configure-slot (slot class &key
                                    (type (getf (mopu:slot-properties (find-class class) slot)
                                                :type)))
  "Set the value of a slot in `*auto-config-file*'.
CLASS is a class symbol."
  (sera:nlet lp ()
    (let ((input (read-from-string
                  (prompt1
                    :prompt (format nil "Configure slot value ~a" slot)
                    :sources (make-instance 'prompter:raw-source)))))
      (cond
        ((and type (not (typep input type)))
         (echo-warning "Type mismatch for ~a: got ~a, expected ~a."
                       slot (type-of input) type)
         (lp))
        (t
         (auto-configure :class-name class :slot slot :slot-value input)
         (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input))))))

(define-internal-page-command-global common-settings ()
    (buffer "*Settings*" 'nyxt/help-mode:help-mode)
  "Configure a set of frequently used settings."
  (spinneret:with-html-string
    (:h1 "Common Settings")
    (:p "Set the values for frequently configured settings. "
        "Changes only apply to newly created buffers.")
    (:h2 "Keybinding style")
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/emacs-mode:emacs-mode :activate nil))
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/vi-mode:vi-normal-mode :activate nil)))))
                 "Use default (CUA)"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/vi-mode:vi-normal-mode :activate nil))
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/emacs-mode:emacs-mode :activate t)))))
                 "Use Emacs"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  `(progn
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/emacs-mode:emacs-mode :activate nil))
                                     (nyxt::auto-configure
                                      :class 'buffer
                                      :form (nyxt/vi-mode:vi-normal-mode :activate t)))))
                 "Use vi"))
    (flet ((generate-colors (theme-symbol text)
             (spinneret:with-html-string
               (:p (:button :class "button"
                            :style (format nil "background-color: ~a; color: ~a"
                                           (theme:primary-color (symbol-value theme-symbol))
                                           (theme:background-color (symbol-value theme-symbol)))
                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                             `(nyxt::auto-configure
                                               :class 'browser
                                               :slot 'theme
                                               :slot-value ',theme-symbol)))
                            text))
               (:p "Colors:")
               (:dl
                (loop for (name color text-color) in '(("Text" theme:text-color theme:background-color)
                                                       ("Accent" theme:accent-color theme:background-color)
                                                       ("Primary" theme:primary-color theme:background-color)
                                                       ("Secondary" theme:secondary-color theme:background-color)
                                                       ("Tertiary" theme:tertiary-color theme:text-color)
                                                       ("Quaternary" theme:quaternary-color theme:text-color)
                                                       ("Background" theme:background-color theme:text-color))
                      collect (:dt name ": ")
                      collect (:dd (:span :style (format nil "background-color: ~a; color: ~a; border-radius: 0.2em"
                                                         (slot-value (symbol-value theme-symbol) color)
                                                         (slot-value (symbol-value theme-symbol) text-color))
                                          (slot-value (symbol-value theme-symbol) color))))))))
      (:h2 "Theme style")
      (:raw (generate-colors 'theme::+light-theme+ "Use default (Light theme)"))
      (:raw (generate-colors 'theme::+dark-theme+ "Use Dark theme")))
    (:h2 "Default new buffer URL")
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval
                              `(nyxt::configure-slot 'default-new-buffer-url 'browser :type 'STRING)))
             "Set default new buffer URL")
    (:h2 "Default zoom ratio")
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval
                              `(nyxt::configure-slot 'current-zoom-ratio 'buffer)))
             "Set default zoom ratio")
    (:h2 "Disable compositing")
    (:p "On some systems, compositing can cause issues with rendering. If you
     are experiencing blank web-views, you can try to disable compositing. After
     disabling compositing, you will need to restart Nyxt.")
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval
                              `(nyxt::append-configuration
                                '(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1"))))
             "Disable compositing")
    (:h2 "Edit configuration")
    (:p "Edit user configuration and other files in external text editor.")
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt::edit-user-file-with-external-editor)))
             "Edit user files"))))

(define-internal-page-command-global describe-bindings ()
    (buffer "*Help-bindings*" 'base-mode)
  "Show a buffer with the list of all known bindings for the current buffer."
  (spinneret:with-html-string
    (:h1 "Bindings")
    (:p (loop for keymap in (current-keymaps (current-buffer))
              collect (:div
                       (:h3 (keymap:name keymap))
                       (:table
                        (loop for keyspec being the hash-keys
                                in (keymap:keymap-with-parents->map keymap)
                                  using (hash-value bound-value)
                              collect (:tr
                                       (:td keyspec)
                                       (:td (format nil "~(~a~)" bound-value))))))))))

(define-command print-bindings-cheatsheet ()
  "Print the buffer with the list of all known bindings for the current buffer
optimizing the use of space."
  (nyxt::html-set-style
   (theme:themed-css (theme *browser*)
     (h3 :font-size "10px"
         :font-family theme:font-family
         :font-weight 500)
     (tr :font-size "7px")
     (div :display inline-block))
   (nyxt:describe-bindings))
  (print-buffer))

(defun tls-help (buffer url)
  "This function is invoked upon TLS certificate errors to give users
help on how to proceed."
  (setf (slot-value buffer 'status) :failed)
  (html-set
   (spinneret:with-html-string
     (:h1 (format nil "TLS Certificate Error: ~a" (render-url url)))
     (:p "The address you are trying to visit has an invalid
certificate. By default Nyxt refuses to establish a secure connection
to a host with an erroneous certificate (e.g. self-signed ones). This
could mean that the address you are attempting the access is
compromised.")
     (:p "If you trust the address nonetheless, you can add an exception
for the current hostname with the "
         (:code "add-domain-to-certificate-exceptions")
         " command.  The "
         (:code "certificate-exception-mode")
         " must be active for the current buffer (which is the
default).")
     (:p "To persist hostname exceptions in your initialization
file, see the "
         (:code "add-domain-to-certificate-exceptions")
         " documentation."))
   buffer))

(defun describe-key-dispatch (command)
  (unwind-protect
       (describe-command :command (name (function-command (symbol-function command))))
    (setf (command-dispatcher (current-window)) #'dispatch-command
          (input-skip-dispatcher (current-window)) #'dispatch-input-skip)))

(defun skip-describe-dispatch (keyspec)
  (declare (ignore keyspec))
  (echo "Cancelled describe-key.")
  (setf (command-dispatcher (current-window)) #'dispatch-command
        (input-skip-dispatcher (current-window)) #'dispatch-input-skip))

(define-command describe-key ()
  "Display binding of user-inputted keys."
  (setf (command-dispatcher (current-window)) #'describe-key-dispatch
        (input-skip-dispatcher (current-window)) #'skip-describe-dispatch)
  (echo "Press a key sequence to describe:"))

(defun evaluate (string)
  "Evaluate all expressions in STRING and return the last result as a list of values.
The list of values is useful when the last result is multi-valued, e.g. (values 'a 'b).
You need not wrap multiple values in a PROGN, all top-level expressions are
evaluated in order."
  (let ((channel (make-channel 1)))
    (run-thread "evaluator"
      (calispel:!
       channel
       (with-input-from-string (input string)
         (first
          (last
           (loop for object = (read input nil :eof)
                 until (eq object :eof)
                 collect (multiple-value-list
                          (handler-case
                              (eval object)
                            (error (c) (format nil "~a" c))))))))))
    (calispel:? channel)))

(defun evaluate-async (string)
  "Like `evaluate' but does not block and does not return the result."
  (run-thread "async evaluator"
    (with-input-from-string (input string)
      (loop for object = (read input nil :eof)
            until (eq object :eof)
            collect (funcall (lambda () (eval object)))))))

(define-class condition-handler ()
  ((condition-itself
    (error "condition-handler should always wrap a condition.")
    :type condition
    :documentation "The condition itself.")
   (restarts
    '()
    :type list
    :documentation "A list of restarts for the given condition.
Stored in the format given by `compute-restarts'.")
   (channel
    nil
    :type (or null calispel:channel)
    :documentation "The channel to send the chosen restart through.")
   (prompt-text
    "[restart prompt]"
    :type string
    :documentation "The prompt text debugger requires."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The wrapper for condition.

Made so that `debugger-hook' can wait for the condition to be resolved based on
the channel, wrapped alongside the condition and its restarts."))

(defvar *debug-conditions* (make-hash-table)
  "A hash-table from numeric condition ID to the `condition-handler' lists.")

(defun debugger-hook (condition hook)
  ;; FIXME: It handles recursive errors, but has no way to fall back to the
  ;; default debugger, like SLY.
  (declare (ignore hook))
  (when *debug-on-error*
    (let* ((*debugger-hook* #'debugger-hook)
           (id (parse-integer (get-unique-identifier *browser*)))
           (restarts (compute-restarts condition))
           (channel (make-channel 1))
           (handler (make-instance 'condition-handler
                                   :condition-itself condition
                                   :restarts restarts
                                   :channel channel))
           (*query-io*
             (make-two-way-stream
              ;; TODO: Understand how Swank makes those streams.
              (swank-backend:make-input-stream
               (lambda ()
                 (str:concat
                  (prompt1
                    :prompt (prompt-text handler)
                    :sources (list (make-instance 'prompter:raw-source)))
                  +newline+)))
              (swank-backend:make-output-stream
               (lambda (string) (setf (prompt-text handler) string)))))
           (debug-buffer (open-debugger :id id)))
      (setf (gethash id *debug-conditions*) handler)
      ;; FIXME: Waits indefinitely. Should it?
      (invoke-restart-interactively (calispel:? channel))
      (remhash id *debug-conditions*)
      (buffer-delete debug-buffer))))

(defun debug->html (condition id &optional restarts)
  "Produce HTML code for the CONDITION with RESTARTS."
  (spinneret:with-html-string
    (:h* (symbol-name (type-of condition)))
    (:pre (format nil "~a" condition))
    (:section
     (loop for restart in restarts
           for i from 0
           collect (:button :class "button"
                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                             `(progn
                                                (let ((condition (gethash ,id *debug-conditions*)))
                                                  (calispel:! (channel condition)
                                                              (nth ,i (restarts condition)))))))
                            (format nil "[~d] ~a" i (restart-name restart))))
     (:h* "Backtrace")
     ;; TODO: SLIME and SLY provide introspectable backtraces. How?
     (:pre (with-output-to-string (s) (uiop:print-backtrace :stream s :condition condition))))))

;; FIXME: Not for interactive use?
(define-internal-page-command open-debugger (&key id)
    ;; TODO: Introduce debug-mode with keys invoking restarts and toggling backtrace.
    (buffer (format nil "*Debug-~d*" id) 'base-mode)
  "Open the debugger with the condition indexed by ID."
  (with-slots (condition-itself restarts channel)
      (gethash id *debug-conditions*)
    (declare (ignore channel))
    (debug->html condition-itself id restarts)))

(define-command-global toggle-debug-on-error (&optional (value nil value-provided-p))
  "Toggle Nyxt-native debugging.

See `*debug-on-error*'."
  (let ((value (if value-provided-p value (not *debug-on-error*))))
    (setf *debug-on-error* value)
    ;; FIXME: This messes up SLIME/SLY debugging in REPL, as they set this too.
    (swank-backend:install-debugger-globally (when value #'debugger-hook))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))

(defun error-buffer (&optional (title "Unknown error") (text ""))
  (sera:lret* ((error-buffer (make-instance 'web-buffer)))
    (with-current-buffer error-buffer
      (html-set (error-help title text)
                error-buffer))))

(defun error-in-new-window (title text)
  (sera:lret* ((window (window-make *browser*))
               (error-buffer (error-buffer title text)))
    (window-set-buffer window error-buffer)))

(define-command nyxt-version ()
  "Version number of this version of Nyxt.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(-> binding-keys (function-symbol &key (:modes list)) *)
(defun binding-keys (fn &key (modes (if (current-buffer)
                                        (modes (current-buffer))
                                        (mapcar (alex:compose #'make-instance #'mode-name) %default-modes))))
  ;; We can't use `(modes (make-instance 'buffer))' because modes are only
  ;; instantiated after the buffer web view, which is not possible if there is
  ;; no *browser*.
  (let* ((current-buffer (current-buffer))
         (buffer (or (current-buffer)
                     (make-instance 'buffer)))
         (keymaps (cons (override-map buffer)
                        (delete nil (mapcar #'keymap modes)))))
    (unwind-protect
         (or (first (keymap:binding-keys fn keymaps))
             "UNBOUND")
      (unless current-buffer
        (buffer-delete buffer)))))

(define-internal-page-command-global new ()
    (buffer "*New buffer*" 'base-mode)
  "Open up a buffer with useful links suitable for a `default-new-buffer-url'."
  (spinneret:with-html-string
    (:style (:raw 
             (theme:themed-css (theme *browser*)
               ("#logo"
                :width "260px"
                :height "260px")
               (.container
                :display "grid"
                :grid-template-columns "256px 1fr 1fr"
                :grid-gap "10px"
                :margin "50px"
                :max-width "800px")
               ("li"
                :margin-bottom "12px"
                :border-radius "3px"
                :list-style-type "none")
               (".button"
                :width "100%"
                :text-align "left")
               ("ul"
                :margin-top "0")
               ("@keyframes gradient"
                ("0%" :background-position "0% 50%")
                ("50%" :background-position "100% 50%")
                ("100%" :background-position "0% 50%"))
               ("#secondary-links"
                :opacity "40%")
               (.copyright
                :color theme:tertiary
                :position "absolute"
                :bottom "1em"
                :right "1em"))))
    (:div :class "container"
          (:div :class "logo"
                (:img :src "data:image/png;base64,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cC8xLjAvIgogICB4bXBNTTpEb2N1bWVudElEPSJnaW1wOmRvY2lkOmdpbXA6ZTEyMDY2YzktYWIxZi00OTI0LWE0YzQtNjUzYmRiMjQyNjIwIgogICB4bXBNTTpJbnN0YW5jZUlEPSJ4bXAuaWlkOjQ3MmE0MDYzLWYwZmYtNGYyZS1hOTZiLTYzNTAwMmE4MWY3MCIKICAgeG1wTU06T3JpZ2luYWxEb2N1bWVudElEPSJ4bXAuZGlkOjlhN2QzZWU2LWYyNzktNGViNC05MTFhLTNjZjcyZDkxMTE1MCIKICAgZGM6Rm9ybWF0PSJpbWFnZS9wbmciCiAgIEdJTVA6QVBJPSIyLjAiCiAgIEdJTVA6UGxhdGZvcm09IkxpbnV4IgogICBHSU1QOlRpbWVTdGFtcD0iMTY1MDE3MTgyNzM3NzUwOSIKICAgR0lNUDpWZXJzaW9uPSIyLjEwLjMwIgogICB0aWZmOk9yaWVudGF0aW9uPSIxIgogICB4bXA6Q3JlYXRvclRvb2w9IkdJTVAgMi4xMCI+CiAgIDx4bXBNTTpIaXN0b3J5PgogICAgPHJkZjpTZXE+CiAgICAgPHJkZjpsaQogICAgICBzdEV2dDphY3Rpb249InNhdmVkIgogICAgICBzdEV2dDpjaGFuZ2VkPSIvIgogICAgICBzdEV2dDppbnN0YW5jZUlEPSJ4bXAuaWlkOjZlN2RjNjRlLTZiZGQtNGE5OC04ZDJkLTIwOGRkNDE5NTAzMiIKICAgICAgc3RFdnQ6c29mdHdhcmVBZ2VudD0iR2ltcCAyLjEwIChMaW51eCkiCiAgICAgIHN0RXZ0OndoZW49IjIwMjItMDQtMTdUMDA6MDM6NDctMDU6MDAiLz4KICAgIDwvcmRmOlNlcT4KICAgPC94bXBNTTpIaXN0b3J5PgogIDwvcmRmOkRlc2NyaXB0aW9uPgogPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAg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"
                      :id "logo"))
          (:div :id "primary-links"
                (:ul
                 (:li (:a :class "button accent"
                          :href "#"
                          :onclick (ps:ps (nyxt/ps:lisp-eval '(set-url :prefill-current-url-p nil)))
                          "Start searching!"))
                 (:li (:a :class "button" :href (nyxt-url 'tutorial)
                          :title "An introduction to Nyxt core concepts."
                          "Tutorial"))
                 (:li (:a :class "button" :href (nyxt-url 'manual)
                          :title "Full documentation about Nyxt, how it works and how to configure it."
                          "Manual"))
                 (:li (:a :class "button" :href (nyxt-url 'changelog)
                          :title "Information about changes between Nyxt versions."
                          "Change Log"))
                 (:li (:a :class "button" :href (nyxt-url 'describe-bindings)
                          :title "List all bindings for the current buffer."
                          "List bindings"))
                 (:li (:a :class "button" :href (nyxt-url 'common-settings)
                          :title "Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level."
                          "⚙ Settings"))))
          (:div :id "secondary-links"
                (:ul
                 (:li (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/"
                          :title "Your contribution will be much appreciated :)"
                          "Source Code"))
                 (:li (:a :class "button" :href "https://www.youtube.com/channel/UC11mZYESUvaKFTaa3zZWHMQ"
                          :title "A channel with tips and tricks of Nyxt by one of the developers."
                          "Nyxt Academy"))
                 (:li (:a :class "button" :href "https://nyxt.atlas.engineer/articles"
                          :title "Learn more about why's and how's behind Nyxt features."
                          "Articles"))
                 (:li (:a :class "button" :href "https://nyxt.atlas.engineer/applications"
                          :title "Check out the applications built on top of Nyxt!"
                          "Applications"))
                 (:li (:a :class "button" :href "https://store.nyxt.atlas.engineer/"
                          :title "Buy Nyxt merchandise and support the development!"
                          "Store"))
                 (:li (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/blob/master/documents/README.org"
                          :title "Helpful tips for Nyxt hacking and contributing."
                          "Developer Manual"))
                 (:li (:a :class "button" :href "https://discourse.atlas.engineer/"
                          :title "A forum for questions and ideas on Nyxt."
                          "Forum"))
                 (:li (:a :class "button" :href "https://kiwiirc.com/nextclient/irc.libera.chat/nyxt"
                          :title "Chat with developers and other Nyxt users."
                          "Chat"))))
     
     (:p :class "copyright"
         (format nil "Nyxt/~a ~a" +renderer+ +version+)
         (:br)
         (format nil "Atlas Engineer LLC, 2018-~a" (local-time:timestamp-year (local-time:now)))))))

(define-internal-page-command-global manual ()
    (buffer "*Manual*" 'nyxt/help-mode:help-mode)
  "Show the manual."
  (spinneret:with-html-string (:style (style buffer))
    (:style (cl-css:css '(("body"
                           :max-width "80ch"))))
    (:raw (manual-content))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/help-mode:help-mode)
  "Show the tutorial."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (cl-css:css '(("body"
                           :max-width "80ch"))))
    (:h1 "Nyxt tutorial")
    (:p "The following tutorial introduces core concepts and
basic usage.  For more details, especially regarding configuration, see
the "
        (:code (command-markup 'manual)) ".")
    (:raw (tutorial-content))))

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
     "Renderer version: " +renderer+ +newline+
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

(define-internal-page-command-global show-system-information ()
    (buffer "*System information*" 'base-mode)
  "Show buffer with Lisp version, Lisp features, OS kernel, etc.
System information is also saved into the clipboard."
  (let* ((*print-length* nil)
         (nyxt-information (system-information)))
    (prog1
        (spinneret:with-html-string
          (:h1 "System information")
          (:pre nyxt-information))
      (copy-to-clipboard nyxt-information)
      (log:info nyxt-information)
      (echo "System information copied to clipboard."))))

(define-internal-page-command-global dashboard ()
    (buffer "*Dashboard*" 'base-mode)
  "Print a dashboard."
  (flet ((list-bookmarks (&key (separator " → "))
           (spinneret:with-html-string
             (or (let ((bookmarks (nfiles:content (bookmarks-file (current-buffer)))))
                   (loop for bookmark in bookmarks
                         collect (:li (title bookmark) separator
                                      (:a :href (render-url (url bookmark))
                                          (render-url (url bookmark))))))
                 (:p (format nil "No bookmarks in ~s." (nfiles:expand (bookmarks-file (current-buffer)))))))))
    (let ((dashboard-style (theme:themed-css (theme *browser*)
                             (body
                              :color theme:text
                              :background-color theme:background
                              :margin-top 0
                              :margin-bottom 0)
                             ("#title"
                              :font-size "400%")
                             ("#subtitle"
                              :color theme:tertiary)
                             (.section
                              :border-style "solid none none none"
                              :border-color theme:secondary
                              :margin-top "10px"
                              :overflow "scroll"
                              :min-height "150px")
                             ("h3"
                              :color theme:tertiary)
                             ("ul"
                              :list-style-type "circle"))))
      (spinneret:with-html-string
        (:style dashboard-style)
        (:div
         (:h1 :id "title" "Nyxt " (:span :id "subtitle" "browser ☺"))
         (:h3 (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
         (:button :class "button" :onclick (ps:ps (nyxt/ps:lisp-eval
                                                   `(nyxt::restore-history-by-name)))
                  "🗁 Restore Session")
         (:a :class "button" :href (nyxt-url 'manual) "🕮 Manual")
         (:button :class "button"
                  :onclick (ps:ps (nyxt/ps:lisp-eval `(nyxt::execute-command)))
                  "≡ Execute Command")
         (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
        (:h3 (:b "Bookmarks"))
        (:ul (:raw (list-bookmarks)))
        (:h3 (:b "Recent URLs"))
        (:ul (:raw (history-html-list)))))))

(defun dump-command-descriptions (file)
  "Dump the command descriptions as an HTML file."
  (with-open-file (f file :direction :output :if-exists :supersede)
    (format f "~a" (spinneret:with-html-string
                     (:p "Listed below are the current commands, their
                         documentation, and their source. Non-command
                         based features are currently unlisted.")
                     (:h1 "Commands")))
    (format f "~a" (spinneret:with-html-string
                     (:style (cl-css:css
                              '((".nyxt-source"
                                 :overflow "auto"))))))
    (format f "~{~a ~%~}"
            (loop for command in (list-commands)
                  collect (spinneret:with-html-string
                            (:details
                             (:summary (format nil "~(~a~)" (symbol-name (name command))))
                             (:p (:pre (documentation (fn command) t)))
                             (:pre :class "nyxt-source" (:code (let ((*print-case* :downcase))
                                                                 (write-to-string (sexp command)))))))))))
