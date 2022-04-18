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
                :width "256px"
                :height "256px"
                :border (format nil  "2px solid ~a" theme:primary)
                :border-radius "4px")
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
                (:img :src "data:image/png;base64,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QPAZuNLb/o0GMP1Jer2txY+Avm3g4rqtKXvA5Q4w8GTIpuxKQZpCsQi8n9E35YHoLdCz6vXW2sfpA5ClrtI3wMEhMFKi7DWfd3d39vbvmVZ/PyqkcopKFzUhAAANGmlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNC40LjAtRXhpdjIiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iCiAgICB4bWxuczpzdEV2dD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL3NUeXBlL1Jlc291cmNlRXZlbnQjIgogICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICAgeG1sbnM6R0lNUD0iaHR0cDovL3d3dy5naW1wLm9yZy94bXAvIgogICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgIHhtcE1NOkRvY3VtZW50SUQ9ImdpbXA6ZG9jaWQ6Z2ltcDoxY2Y2MjNmYi1kOTAwLTRhZTctYmE2Yy01N2QyNDhlMWFmMTYiCiAgIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6YTVhZmFkMTMtZTEwYi00NGE2LWI4MWYtMWU0OTlhYjZmYmIwIgogICB4bXBNTTpPcmlnaW5hbERvY3VtZW50SUQ9InhtcC5kaWQ6YjQ4YTM5NGYtOTUyOS00MGFkLTgyODItYzMzNDRlOGM1MTE4IgogICBkYzpGb3JtYXQ9ImltYWdlL3BuZyIKICAgR0lNUDpBUEk9IjIuMCIKICAgR0lNUDpQbGF0Zm9ybT0iTGludXgiCiAgIEdJTVA6VGltZVN0YW1wPSIxNjUwMjIyMjYyODAwMjI3IgogICBHSU1QOlZlcnNpb249IjIuMTAuMzAiCiAgIHRpZmY6T3JpZW50YXRpb249IjEiCiAgIHhtcDpDcmVhdG9yVG9vbD0iR0lNUCAyLjEwIj4KICAgPHhtcE1NOkhpc3Rvcnk+CiAgICA8cmRmOlNlcT4KICAgICA8cmRmOmxpCiAgICAgIHN0RXZ0OmFjdGlvbj0ic2F2ZWQiCiAgICAgIHN0RXZ0OmNoYW5nZWQ9Ii8iCiAgICAgIHN0RXZ0Omluc3RhbmNlSUQ9InhtcC5paWQ6NDEzNzM2MTItMDBkZi00N2YwLTkxNWEtYmMyNjY4NjZjZGMzIgogICAgICBzdEV2dDpzb2Z0d2FyZUFnZW50PSJHaW1wIDIuMTAgKExpbnV4KSIKICAgICAgc3RFdnQ6d2hlbj0iMjAyMi0wNC0xN1QxNDowNDoyMi0wNTowMCIvPgogICAgPC9yZGY6U2VxPgogICA8L3htcE1NOkhpc3Rvcnk+CiAgPC9yZGY6RGVzY3JpcHRpb24+CiA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgCjw/eHBhY2tldCBlbmQ9InciPz61g3E2AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5gQREwQWnzSpeQAAABl0RVh0Q29tbWVudABDcmVhdGVkIHdpdGggR0lNUFeBDhcAABb4SURBVHja7Z13VFRH+8efhd0FBRZUbCgmQWOJMVg4GBuJqNgQu9ET44ktFmwn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"
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
