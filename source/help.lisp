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
                :height "256px")
               (.container
                :display "grid"
                :grid-template-columns "256px 1fr 1fr"
                :grid-gap "10px"
                :margin "50px"
                :max-width "800px")
               ("li"
                :background-color "rgb(240, 240, 240)"
                :margin-bottom "10px"
                :padding "0"
                :border-radius "3px"
                :list-style-type "none")
               ("ul"
                :margin-top "0")
               (".laser"
                :background (format nil "linear-gradient(90deg, ~a, ~a, ~a, ~a)"
                                    theme:primary
                                    theme:accent
                                    theme:accent
                                    theme:primary)
	        :background-size "400% 400%"
	        :animation "gradient 10s ease infinite")
               ("@keyframes gradient"
                ("0%" :background-position "0% 50%")
                ("50%" :background-position "100% 50%")
                ("100%" :background-position "0% 50%"))
               (.copyright
                :color "rgb(180, 180, 180)"
                :position "absolute"
                :bottom "1em"
                :right "1em"))))
    (:div :class "container"
          (:div :class "logo"
                (:img :src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAYAAABccqhmAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AYXCys1Lxm/GQAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAXf0lEQVR42u2deVhVxf/HPwfuvWDAdUMzBCs006xw4aFApUTFDXFt8cl8yiUX1HyyJ5QUlzSzUh9NXCtzw1IsCSL3LZEQLVNcMVOBVG7IIi6Xy2V+f3yznybJzOEu557zfj3PPI9cZ/nM58y8z8ycOXMkupdQIoolojAiakAAALVgIqKDRDSXiDLu/CjdFWElEQ2HnwBQPZ8T0QgiInd0fgA0R1si8ieiFOnvYf9B+AQAzRHmTkSLiagFfAGA5vCViKiAsOAHgBYxSUTE4AcAtIkbXAAABAAAAAEAAEAAAAAQAAAABAAAAAEAAEAAAAAQAAAABAAAAAEAAEAAAAAQAAAABAAAAAEAAEAAAAAQAAAABAAAAAEAAEAAAAAQAAAABAAA4CB0cIFt8Pb2psDAQAoICKDg4GBq1qwZBQQEkJeXF0mSREVFRVRUVES///47ZWZmUl5eHuXk5FBJSQmcByAArkiTJk2ob9++FBsbS35+frLyKC4uplmzZlFiYiJdvnzZZX0xduxY8vX15YpbUFBAS5YsUWQ9XnrpJWrVqhVf59HpaMqUKS7fjplagwgi+bZv357l5OQwW5ORkcEee+wxl/T12LFjuetpNpsVW4/8/HzueqxZs0YN/QQCwBhjDRo0qDa/5s2bs9zcXGZv1qxZw9zd3V3K10ajUaiOI0eOVFwd/Pz8hOoQHBwMAVCLAFR3MT/++GPmSMxmM2vSpIlL+TslJYW7fufPn1ec/T/88AO3/deuXVNLP4EAMMbYgAEDqszDYDCws2fPMmfRtm1bl/F3hw4dhOqmtFHO9evXuW2PiYmBAKhJACZOnHhfei8vL3bz5k1WWVnJnEnLli1dxufl5eXc9frqq68UY3fPnj2FronRaFRFH8E+gL959NFH7/nbYDBQQUEB1apViyRJcqptJ0+eJB8fH5fwY1xcHHfc3r17K8buuXPncsdNS0uj0tJS1bR9jAAYY8nJyfekPXfuHFMSWVlZLuHzhg0bCtXrmWeeUYTdIqO88PBwNfUTCABjjB07duyfdGvXrmVK5K233nIJv+/fv5+7TocOHXK6vfHx8dz2Wq1WVfUR6Y4KqJH/Pd7no7S0lGrXrk09evSgtLQ0oXKuXr1Kf/31F928eZPKy8tJkiTS6/VUp04d8vX1pbp169qkPhaLhQwGg+L93qlTJ9q9e7fL1Ck/P597I1dcXBzNmTNHXf0EI4D/3wzEs4hltVrZTz/9xKKiovhUVpJYfHw8u3jxYo1HAe+++67qfO/MFXUfHx8hW/38/NTWTyAAd1i+fHm1cTZt2lQjmyIiIlhpaalsASguLnYJ30+dOpW7Tvn5+U6z89tvv+W28/Dhw2rsJxAA3o0f/v7+NrMtMzNTti2dOnVSvO8DAgKE6qTT6ZxiZ0lJCbeN3bt3hwBoUQBOnjxpF/v27dsny56kpCSX8H92djZ3nTZs2OBw+0JCQuz2vggEQCUCcOrUKbvaeO3aNWGbKisrXcL/kZGR3HUqKSlxuH2//fYbt31z586FAGhNAMrKyuxu43PPPSfLNld4EUWSJKE6BQUFKXbXYrNmzVTZR7AT8AFERETYvYzMzEz66aefhNP16NHDJR7DfvLJJ9zx165d6zDbxo0bR3q9nituXl4enTt3TrXtHCOAKli5cqXD7IyIiBC2b+vWrS5xDZo2bcpdp/LycofZ9eeff9b4RTFMAVQsAJ6eng619fbt20L23bhxw2Wug8gZCuPHj7f/sNfNTdFtAQLgZAFYunSpw21dvXq1sJ0Gg8ElrkNUVBR3nUwmk93tEdnqraQ3FiEADhIAZxzLNXDgQGE7lfIiTXXBw8NDUcIm8uy/VatWqhYALAL+i3PnztGFCxccXm56erpwmhYtWriET81mM61cuZI7/oYNG+xmyxNPPEFGo5Er7s2bN+nEiROqbu8QgH+xbt06p5R7+fJloZeXiIgCAwNdxq8iTwO6detmNztExCUmJkb17R0C8C9SUlKcVvbFixeF4jdo0MBl/JqTk8N9iIaXlxe1adPGLnY8/fTT3HG3bNkCAdASjDH65ZdfnNpJRGjYsKFL+XfYsGHccb/++mublz9o0CDy8PDgirt9+3YqLi6GAGiJU6dOObV8k8mkagHYunWrU6c38+fP5447efJkTbR5CMBdZGdnu5QA1KpVy6X8W1ZWRps3b+aKq9PpaOLEiTYt/+GHH+aO68yRIATASTh7xffWrVtC8XmHs0pC5FNa8fHxNit34cKF3Ie72lp4IAAuQm5urlPLLy8vF4rv6enpcj4+ffo0VVZWcsU1Go02q+PQoUO54yYmJkIAtMjVq1edWn5FRYVQfN6XWZSGSGfctGlTjctr2LAheXt7c48Cr1y5AgHQIgUFBXCCA0hOTuaO27Vr1xqX9+2333LHHTVqlKauBQTgLm7fvg0nOIDi4mLat28fV1wPDw9q165djcoLDg7mjpuRkQEB0CpmsxlOcBAiu+xE7uD/pkuXLtyLpR9//DFZrVYIgFaxWCxwgoMQeeLSuHFj2eWsXr2aO+5nn32muesAAbgLZ38DUGuMHz+eK567uzu99957sspo1KgRVzyTyUR5eXkQAAAchcjjthkzZgjnP23aNHJz42viQ4YM0eQ1gAAAp1FYWEjHjx/niuvp6UkPPfSQUP6xsbHccXkXJSEAANgQkTuvyGKgl5cX91bpxMRE4V2YEAAAbADvCICIqHPnztxxk5KSuOO+//77mvU/BAA4FavVStOmTeOKq9PpKCQkxKZiYbVanXICFAQAgL9JSEjgjstzSEfr1q25t0kPHjxY076HAACnU1hYyP0IjueVXpGTfNLS0iAAADibl156ia/BurlVe1hHQEAAV14HDhzgPqYMAgCAHTly5Ah33A8++OA//2/06NHcz/6HDx+ueb9DAIAisFgstHjxYq647u7u5OPjU+X/iRz7dfbsWQgAmh5QCtOnT+eO+91331UpDLwv/rzzzjvCx7BDAACwI4WFhdxz8qoe861Zs4b7fY5Vq1bB4RAAoDT69evHHTcsLOyevwcNGsSV7vz585o48hsCAFwOkU+k3X2yUGBgIPfdf+DAgXA0BAAoEbPZTN988w1XXF9f33/+nZqayl2GyPZjCAAADmbs2LHcce8cM877odRPP/1U+PBVNaODC4DS+Ouvv6iiooJ0uuqb58yZM+nUqVPcw/9Zs2bBwRgBAKXDuzNQkiTuLzqbTCYqKSmBcyEAQOmIfEeQ9+Mhffr0gWMhAMAVuH37Nu3du9emeYpsN4YAAOBkeJ/r85CYmCj86TUIAABOxJafauM9gRgCAIBCYIzRsGHDbJJXYWEhHAoBAK7G+vXra5zHhAkT4EgIAHBFzGZzjXfurVixAo6EAABXpXfv3rLTXrt2TbNHfkMAgCq4dOmS7LR48QcCAFwcxli15wD+FwcPHoQDIQDA1Zk3b55wmsOHD+OT7xAAoAYsFovwVADDfwgAUBGvv/66UPzc3Fw4DQIA1MKiRYu4486fP58qKyvhNAgAUAN6vZ6CgoK448fFxcFpEACgFqKjo7nj3rx5E4t/EACgJjZu3MgdF+/9QwCAiqhfvz73576IiPbs2QOnQQCAWqjqK0D/xc6dO8lqtcJpEACgBgwGA3Xs2JE7Pp79QwCAirhz7DcPVqsVh35CAIBa0Ol0NHXqVNz97QxDQFBimDJlChNBkiT4TTzACQjKC3q9Xqjzr1+/Hn6TEaQ7KgCAkti2bRtFRkbyz2Xd3IgxNGWsAQCXJzQ0VKjznzx5Ep1fJhgBAEUREBAg/NpvnTp1sPqPEQBwdYYMGSLc+fPz89H5awgWQxCcGgYNGsROnz7N5NCgQQP4sAYBnwcHdsHX15c2bNhQ5Ym8bm5u5OPjQ02bNqXGjRvLLiMzM5NMJhOcjTUAoIa5vNCwlTGhF4QA1gCAiujWrRucAAEAWmTu3Lm0Y8cOOAICALTGpk2baNKkSXAEBABojfnz59PLL78MR9gQPAUAisdqtVJUVBRt3boVzsAIAGiJGTNmkE6nQ+fHCABoAcYYbdu2jdatW0fr16+HQ+wM9gEAu2A0Gik2NpYsFssDO7vZbKaioiLKzc2lEydO0MWLF+E8CAAAAGsAAAAIAAAAAgAAgAAAACAAAAAIAAAAAgAAgAAALRIVFUU///wzHAEBAFoiISGBGGOUkpJCBoMBDpEB3gUALkXt2rVp7969FBQURJIkwSEYAQAt0L59eyotLaXi4mJq3bo1Oj8EAGiB+Ph4YozRgQMHyMfHBw7BFABogZEjR9KyZcvgCIwAgBbx9PSEEyAAAAAIAAAAAgAAgAAAACAAAAAIAAAAAgAAgAAAACAAAAAIAAAAAgAAgAAAACAAAAAIAAAAAgAABAAAAAEAAEAAAHA65eXlQvErKyvhNJkwNYQLFy4wUVJTUxVbHzns37/fafZ27dqVaYmPPvpIFf1GNSOAZ555RjhNr1696M0331RcXXbt2iWu4oxReHg4bmeOumsyhimAkrh+/Tr169dPON2XX35JDRs2VEw9Ro4cSREREcLpnnvuOfRKoO01gC1bttDy5cuF0x05ckQR9jdu3FjWUdgzZ86krKwstGagbQEgIho1ahT98ccfQmn8/f1pwYIFTrf90KFDwmmysrJo2rRpaMkAAnCHoKAg4TQTJkygF1980Wk2L1y4kPz8/ITThYaGohUDCIAt1gN2797tFHs7depE48ePF04XERFBVqsVrRhAAGyxHiBJEu3du9fhtspZ9Z83bx7t2bMHLRhAAB60HnD+/HmhNC+88AKNHTvWYTbu2bNH+Eu3J0+epHfffRetF9QYif63IUC1eHt70/Xr14XTPf7443ThwgW72jZ69GhasmSJcLqHHnqIbt26pSg/e3h42PRx6tChQ2n69Onc8U+cOEGdOnVy2DcFS0pKqLS0VBV9hKk99OnTR3inl8lksqtN/v7+snagRUVFMS1cs7ffflvIL7/88osm/ELYCShOcnKy8HqAr68vrVy50m42yXnkt3z5ckpNTcW4FWAEICecO3dO+I7bs2dPm9uxaNEiYTv++OMPTd2ZMALACMDmtG7dWjhNamoq6fV6m9kQERFB48aNE07Xpk0b3KqAzdGUAJSVlVGfPn3EVkkliQ4ePGgzG3bu3CmcZtCgQVRcXIzWCiAANeX7778X3m8fHBwsa2PRv5HzyG/9+vX09ddfo6UCrAE4ez2gTp06sssbM2aMcHlXrlzR7NwUawBYA1DcesDFixdlldWkSRNKSEhwiI0AYArAuR4QHR0tlMZoNNKKFSuEy8rMzBROM3z4cLpy5QpaKMAUwJ5h6dKlwkPzsLAw7vwTEhKE8//uu+80PzTFFMBhAU7IyckR7qQ6na7afDt37iycb0lJCRolBABrAEpfDzh27Fi1cXbs2CGcL573A6wBOJgbN25Q7969hdK0bNmSYmNj//P/9+3bJ/zIb8KECcJvLwKANQAbhSVLlggP2Zs1a3ZfPp6ensL5bN++HdcAUwBMAZzJmDFjKCcnRyjN8ePH7/vt9u3b1LdvX+48zGYzRUZG4gIATAGcTdu2bYXie3p6UnJy8n2/Jycn0+rVq7nyCA4OhuMBBEAJlJWVCa8HREdHU//+/e/7/Y033qBLly49MO2UKVMoOzsbjgdYA3Dl9YDy8nK2devW+/J55JFH/jPNgQMH4GusAWANQKnrAWfOnOGOr9frqVu3bhQTE3PP75cvX6bXXnvtftXFp7wApgDKRs7cfPHixdS8efN7fktMTKSNGzfe81tYWBi+aAsgAEpfD4iKihJOV9VxX6+88goVFBQQEdGcOXPo559/hoMBBEDp/PDDD8In99auXZu2bdtW5Yji119/pbi4ODgWQABchZiYGKH1ACKiyMjI+47+ys3NxSM/AAFwRdq1ayecZtGiRfTkk0/e8xvm/QAC4ILcuHGDevXqZZP1AAAgAC5IWloalZeXC6UxGo3CZxACAAFQIElJSWQwGITTjRw5kurUqQMHAgiAqzJ06FAaMGCA7PTVbQkGAAKgUJo1a0ZffPFFjfLw8fGp8qUhACAACufIkSM2ySc6OppeffVVOBRAAFyF1NRUMhqNNstvw4YNsh4pAgABcDCjR4+W9eivOjIyMsjNDa4HEADF0rJlS+EtwLzo9Xo6fPgwnAwgAEolKyvLrvm3adOGZs+eDUcDCIDS2LFjB3l5eQmlKSkpES4nLi6OIiIi4HAAAVAKEyZMoC5duginCw8PpzFjxgin27VrF/n4+MDxAALgbJ599llasGCBcLpp06bRsWPHaOnSpZSWliac/ujRo3A+gAA4G9FDOhhjdOjQIZo5c+Y/v/Xq1YtKS0uF8gkMDJT10VEAIAA2Yv/+/VSrVi2hNJIkUYcOHe77Xc47/yNGjKjRVmMAIAAyiY2NpY4dOwqn69ChA1kslvt+z8nJoYkTJwrnl5SURH5+frggwKFo+ljkdu3aMTl88skn1ea9a9cu4XwLCgpwXDURi4mJEfLb0aNH4Td8HlwsSJLEzGazcCc9duwYdxm3bt0Szn/z5s2ab5jDhg0T8tnp06fRmfFdADEyMzNlvd8fGhrKHTckJEQ4//79+9OIESM0PSw1m81C8fEoFVMAoRAfHy9r6N+9e3fhsqZMmSKrrBYtWmj2zhQdHS3kq4qKCtzRMQXgG/aHhobK6pDLli2TXW56erpwedevX9dswwwJCRH2FzozBKDaoNfrWUVFhXDjysnJqVG57u7uzGKxCJe7b98+TTZMf39/YV+1bNkSHRprAA8mKyuL3N3dhdPJmcvfjdVqpbCwMOF04eHhNGnSJM3NS/Py8oTTyHmUCzSkdrNnz5Y19B84cKDTbQgJCWGSJGnq7lRUVCTko+TkZNzVMQWoOrzwwguyOt66detsbsuRI0dYZWWlkB0Wi4UZDAZNNc69e/cK+aiyshIdGgJwf6hVq5Zwh2OMsfz8fLvY4+HhIUuMRPYfqCHMmjVL2Edvv/02OjUE4N5w4sQJWR3ukUcesZtN4eHhdtuBqJYg10damypBAB4Q5s2bJ6sRvfnmm3a3bcGCBbJsi4yM1EwDlcOvv/6Kjg0BINa1a1dZDWjLli0OszE7O1uWjXXr1tVEA92yZYss/2RnZzOdTocOrlUBMBqNshpOYWGhQ+309vaWZeeFCxc00UB79OjB5GKxWFhCQgJzc3NDR9eaAOTk5MhqNIGBgQ63tXv37rJsXbVqlSYaaX5+Pqspp06dYmvXrmXDhg1jQUFBzMfHB51frQKQkJAgq5GMGzfOaTYvW7ZMls0vv/yy6hvpq6++ypTGvHnzIABKDD179pR1Qbdv3+6yo5YmTZqoXgQyMjIUJQBz5syBACgt1K9fX9bFLCsrU4T9devWlWW/yWRSvQDIXdOBAGjoXQC5H/F8/vnnFWF/UVER9e/fXzidr68vpaSkqHq/emlpKTVv3hwb9/EuQNXh888/l6XkkydPVlxdVq9eLasuo0ePVv1IoGnTprLe5sQIQMVTgH79+sm6iOnp6Yqt06VLl2TVqVWrVqoXAW9vb/bjjz9CACAAxB5++GFZF7C8vFzRG0Xk1ksp6xmOCO3bt2dnzpyBAGh5DUDuvL9Dhw5UUVGh2HpdvXqVBg8eLJzOy8uL0tPTNTF3TU9PpyeffJJat25NCxcuxGRea2sAcoeBH374ocvUMSkpSVYdFyxYoMmNLU899RQbMmQI27x5M7ty5QpGANUdkXdnHgCAWmnUqBHVq1eP6tWrR7Vr1ya9Xk8Gg4H0ej1JkiQrz+zsbFV80xECAICGwafBAIAAAAAgAAAACAAAAAIAAIAAAAAgAAAACAAAAAIAAIAAAAAgAAAACAAAAAIAAIAAAAAgAAAACAAAAAIAAIAAAAAgAAAApwuACW4AQJOY3IjoIPwAgCY5KBFRKEQAAE0S5k5EeUTkT0Rt4Q8ANMPnRLTE/e8/UiACAGiq848gInK/68cUItpORL5EVJ+IvOAnAFSDiYh2EtFQIlpy58f/AwzX1I3qLRmhAAAAAElFTkSuQmCC"
                      :id "logo"))
          (:div :class "primary-links"
                (:ul
                 (:li (:button :class "button laser"
                               :type "submit"
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
          (:div :class "secondary-links"
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
