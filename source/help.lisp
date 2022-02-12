;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class function-source (prompter:source)
  ((prompter:name "Functions")
   (prompter:constructor (package-functions))))

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
  ((prompter:name "Classes")
   (prompter:constructor (package-classes))))

(define-class slot-source (prompter:source)
  ((prompter:name "Slots")
   (prompter:constructor (package-slots))))

(define-class variable-source (prompter:source)
  ((prompter:name "Variables")
   (prompter:constructor (package-variables))))

(define-class package-source (prompter:source)
  ((prompter:name "Packages")
   (prompter:constructor (mapcar (alex:compose #'intern #'package-name) (list-all-packages)))))

(define-command describe-any (&optional input)
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
                                       :filter-preprocessor preprocessor)
                        (make-instance 'function-source
                                       :actions (list (make-command describe-function* (functions)
                                                        (describe-function :function (first functions))))
                                       :filter-preprocessor preprocessor)
                        (make-instance 'user-command-source
                                       :actions (list (make-command describe-command* (commands)
                                                        (describe-command :command (name (first commands)))))
                                       :filter-preprocessor preprocessor)
                        (make-instance 'class-source
                                       :actions (list (make-command describe-class* (classes)
                                                        (describe-class :class (first classes))))
                                       :filter-preprocessor preprocessor)
                        (make-instance 'slot-source
                                       :actions (list (make-command describe-slot** (slots)
                                                        (describe-slot :class (class-sym (first slots))
                                                                       :name (name (first slots)))))
                                       :filter-preprocessor preprocessor))))
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

;; FIXME: Prints with infinite nesting for lists, but only one level of nesting
;; for hash-tables/objects. Make it uniform?
(defun value->html (value &optional nested-p)
  "Return the HTML representation of VALUE."
  (let ((spinneret:*html-style* :tree)
        (*print-case* :downcase))
    (spinneret:with-html-string
      (labels ((print-scalar (object)
                 (format nil "~s" object))
               (link-to (object)
                 (spinneret:with-html-string
                   (let ((help-mode (current-mode 'help))
                         (id (get-unique-identifier *browser*)))
                     (if (or (scalar-p object)
                             (null help-mode))
                         (:code (print-scalar object))
                         (progn
                           (setf (nyxt/help-mode:inspected-value help-mode id) object)
                           (:a :href (nyxt-url 'describe-value :id id)
                               (print-scalar object))))))))
        (cond
          ((scalar-p value)
           (:code (print-scalar value)))
          ((trivial-types:property-list-p value)
           (:dl
            (loop for (key val . rest) on value by #'cddr
                  collect (:dt (format nil "~a" key))
                  collect (:dd (:raw (value->html val t))))))
          ((trivial-types:association-list-p value)
           (:dl
            (dolist (e value)
              (:dt (format nil "~a" (car e)))
              (:dd (:raw (value->html (cdr e) t))))))
          ((and (sequence-p value)
                ;; Dotted lists are bad.
                (not (and (consp value)
                          (cdr value)
                          (not (consp (cdr value))))))
           (:ul
            (dotimes (i (length value))
              (:li (:raw (value->html (elt value i) t))))))
          (nested-p
           (:raw (link-to value)))
          ((hash-table-p value)
           (alex:when-let ((keys (alex:hash-table-keys value)))
             (:dl
              (dolist (key keys)
                (:dt (format nil "~a" key))
                (:dd (:raw (value->html (gethash key value) t)))))))
          ((typep value '(or standard-object structure-object))
           (alex:when-let ((slot-names (mapcar #'closer-mop:slot-definition-name
                                               (closer-mop:class-slots (class-of value)))))
             (:dl
              (dolist (slot-name slot-names)
                (:dt (format nil "~a" slot-name))
                (:dd (:raw (value->html (slot-value value slot-name) t)))))))
          (t
           (:code (print-scalar value))))))))

(define-internal-page-command-global describe-value
    (&key id)
    (buffer "*Help-value*" 'nyxt/help-mode:help-mode)
  "Inspect value under ID and show it in a help buffer."
  (sera:and-let* ((id id)
                  (help-mode (find-mode buffer 'help-mode))
                  (value (nyxt/help-mode:inspected-value help-mode id)))
    (spinneret:with-html-string
      (:h1 (princ-to-string value))
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
        (external-symbols (package-defined-symbols (list package))))
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
    (&key (variable
           (prompt1
             :prompt "Describe variable:"
             :sources (make-instance 'variable-source))))
    (buffer (str:concat "*Help-" (symbol-name variable) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a variable and show it in a help buffer."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 (format nil "~s" variable)) ; Use FORMAT to keep package prefix.
    (:raw (resolve-backtick-quote-links (documentation variable 'variable) variable))
    (:h2 "Current Value:")
    (:p (:raw (value->html (symbol-value variable))))))

(define-internal-page-command-global describe-function
    (&key (function (prompt1
                      :prompt "Describe function"
                      :sources (make-instance 'function-source))))
    (buffer (str:concat "*Help-" (symbol-name function) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (if function
      (let ((input function))
        (flet ((method-desc (method)
                 (spinneret:with-html-string
                   (:h1 (symbol-name input) " " (write-to-string (mopu:method-specializers method)))
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

(define-internal-page-command-global describe-command
    (&key (command (name (prompt1
                           :prompt "Describe command"
                           :sources (make-instance 'user-command-source)))))
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
             (alex:last-elt location))))
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
    (&key class name)
    (buffer (str:concat "*Help-" (symbol-name name) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a slot and show it in a help buffer."
  (unless (and class name)
    (let ((slot (prompt1
                  :prompt "Describe slot"
                  :sources (make-instance 'slot-source))))
      (setf name (name slot)
            class (class-sym slot))
      ""))
  (describe-slot* name class :mention-class-p t))

(defun describe-slot* (slot class &key mention-class-p)
  "Create the HTML that represents a slot."
  ;; TODO: Adapt HTML sections / lists to describe-slot and describe-class.
  ;; TODO: Parse docstrings and highlight code samples.
  (let ((props (mopu:slot-properties (find-class class) slot)))
    (spinneret:with-html-string
      (:ul
       (:li (symbol-name slot))
       (:ul
        (when mention-class-p
          (:li (format nil "Class: ~s" class)))
        (when (getf props :type)
          (:li (format nil "Type: ~s" (getf props :type))))
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
        (unless (user-class-p class)
          (:li (:button :class "button"
                        :onclick (ps:ps (nyxt/ps:lisp-eval
                                         `(nyxt::configure-slot ',slot ',class :type ',(getf props :type))))
                        "Configure"))))))))

(define-internal-page-command-global describe-class
    (&key (class (prompt1
                   :prompt "Describe class"
                   :sources (make-instance 'class-source))))
    (buffer (str:concat "*Help-" (symbol-name class) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a class and show it in a help buffer."
  (let* ((slots (class-public-slots class))
         (slot-descs (apply #'str:concat (mapcar (alex:rcurry #'describe-slot* class) slots))))
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
                                    (value nil new-value-supplied-p)
                                    ;; SLOT may also be a method, as with
                                    ;; `default-modes', in which case there is no type.
                                    (type (ignore-errors
                                           (getf (mopu:slot-properties (find-class class) slot)
                                                 :type))))
  "Set the value of a slot in a users auto-config.lisp.
CLASS is a class symbol."
  (flet ((set-slot (slot class input)
           (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input)
           (append-configuration `(define-configuration ,class
                                    ((,slot ,input))))))
    (if new-value-supplied-p
        (set-slot slot class value)
        (let ((accepted-input
                (loop while t do
                  (let ((input (read-from-string
                                (prompt1
                                  :prompt (format nil "Configure slot value ~a" slot)
                                  :sources (make-instance 'prompter:raw-source)))))
                    (cond ((not type) (return input))
                          ((typep input type) (return input))
                          (t (progn
                               (echo-warning
                                "There's a type mismatch: ~a should be a ~a, while you provided ~a"
                                slot type (type-of input))
                               nil)))))))
          (set-slot slot class accepted-input)
          (eval `(define-configuration ,class
                   ((,slot ,accepted-input))))))))

(defun append-configuration (form &key (format-directive "~&~s~%"))
  (let ((path (nfiles:expand *auto-config-file*)))
    (with-open-file (file path
                          :direction :output
                          :if-exists :append)
      (let ((*print-case* :downcase))
        (log:info "Appending to ~s:~&~s" path form)
        (format file format-directive form)))))

(define-internal-page-command-global common-settings ()
    (buffer "*Settings*" 'nyxt/help-mode:help-mode)
  "Configure a set of frequently used settings."
  (let ((spinneret:*html-style* :tree))
    (spinneret:with-html-string
      (:style (style (current-buffer)))
      (:h1 "Common Settings")
      (:p "Set the values for frequently configured settings. "
          "Changes only apply to newly created buffers.")
      (:h2 "Keybinding style")
      (:p (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(progn (nyxt::configure-slot
                                             'default-modes 'buffer
                                             :value '%slot-default%)
                                            (nyxt/emacs-mode:emacs-mode :activate nil)
                                            (nyxt/vi-mode:vi-normal-mode :activate nil))))
                   "Use default (CUA)"))
      (:p (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(progn (nyxt::configure-slot
                                             'default-modes 'buffer
                                             :value '(append '(emacs-mode) %slot-default%))
                                            (nyxt/emacs-mode:emacs-mode :activate t)
                                            (nyxt/vi-mode:vi-normal-mode :activate nil))))
                   "Use Emacs"))
      (:p (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    `(progn (nyxt::configure-slot
                                             'default-modes 'buffer
                                             :value '(append '(vi-normal-mode) %slot-default%))
                                            (nyxt/emacs-mode:emacs-mode :activate nil)
                                            (nyxt/vi-mode:vi-normal-mode :activate t))))
                   "Use vi"))
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
               "Disable compositing"))))

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

(defun describe-key-dispatch-input (event buffer window printable-p)
  "Display documentation of the value bound to the keys pressed by the user.
Cancel with 'escape escape'.
Input is not forwarded.
This function can be used as a `window' `input-dispatcher'."
  (declare (ignore event buffer printable-p))
  (handler-case
      (progn
        (with-accessors ((key-stack key-stack)) window
          (log:debug "Intercepted key ~a" (first (last key-stack)))
          (let ((escape-key (keymap:make-key :value "escape"))
                (bound-value (the (or symbol keymap:keymap null)
                                  (keymap:lookup-key key-stack
                                                     (current-keymaps (current-buffer))))))
            (cond
              ((and bound-value (not (keymap:keymap-p bound-value)))
               ;; TODO: Highlight hit bindings and display translation if any.
               ;; For this, we probably need to call `lookup-key' on key-stack.
               (describe-command :command (name (function-command (symbol-function bound-value))))
               (setf key-stack nil)
               (setf (input-dispatcher window) #'dispatch-input-event))
              ((not bound-value)
               (echo "Unbound: ~a"
                     (keyspecs-with-optional-keycode key-stack))
               (setf key-stack nil)
               (setf (input-dispatcher window) #'dispatch-input-event))
              ((and (<= 2 (length key-stack))
                    (every (lambda (key) (keymap:key= key escape-key))
                           (last key-stack 2)))
               (echo "Cancelled.")
               (setf key-stack nil)
               (setf (input-dispatcher window) #'dispatch-input-event))
              (t
               (echo "Press a key sequence to describe (cancel with 'escape escape'): ~a"
                     (keyspecs-with-optional-keycode key-stack)))))))
    (error (c)
      (echo-warning "~a" c)
      (setf (key-stack window) nil)
      (setf (input-dispatcher window) #'dispatch-input-event)))
  ;; Never forward events.
  t)

(define-command describe-key ()
  "Display binding of user-inputted keys."
  (setf (input-dispatcher (current-window)) #'describe-key-dispatch-input)
  (echo "Press a key sequence to describe (cancel with 'escape escape'):"))

(defun evaluate (string)
  "Evaluate all expressions in string and return the last result as a list of values.
The list of values is useful when the last result is multi-valued, e.g. (values 'a 'b).
You need not wrap multiple values in a PROGN, all top-level expression are
evaluate in order."
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

(define-command-global toggle-debug-on-error (&key (value nil value-provided-p))
  "Toggle Nyxt-native debugging.

See `*debug-on-error*'."
  (let ((value (if value-provided-p value (not *debug-on-error*))))
    (setf *debug-on-error* value)
    ;; FIXME: This messes up SLIME/SLY debugging in REPL, as they set this too.
    (swank-backend:install-debugger-globally (when value #'debugger-hook))
    (echo "Nyxt-native debugging ~:[dis~;en~]abled." value)))

(defun error-buffer (&optional (title "Unknown error") (text ""))
  (sera:lret* ((error-buffer (make-instance 'user-web-buffer)))
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
                     (make-instance 'user-buffer)))
         (keymaps (cons (override-map buffer)
                        (delete nil (mapcar #'keymap modes)))))
    (unwind-protect
         (or (first (keymap:binding-keys fn keymaps))
             "UNBOUND")
      (unless current-buffer
        (buffer-delete buffer)))))

(define-internal-page-command-global help ()
    (buffer "*Help*" 'nyxt/help-mode:help-mode)
  "Open up a small help buffer."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (cl-css:css '(("#documentation .button"
                           :min-width "100px"))))
    (:h1 "Welcome to Nyxt :-)")
    (:p (:a :href "https://nyxt.atlas.engineer" "https://nyxt.atlas.engineer"))
    (:h2 "Quick configuration")
    (:p (:a :class "button" :href (nyxt-url 'common-settings) "Common settings")
        " Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level.")
    (:h2 "Documentation")
    (:table :id "documentation"
            (:tr (:td (:a :class "button" :href (nyxt-url 'describe-bindings) "List bindings"))
                 (:td "List all bindings for the current buffer."))
            (:tr (:td (:button :class "button"
                               :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt::edit-user-file-with-external-editor)))
                               "Edit user files"))
                 (:td "Edit user configuration and other files in external text editor."))
            (:tr (:td (:a :class "button" :href (nyxt-url 'tutorial) "Tutorial"))
                 (:td "An introduction to Nyxt core concepts."))
            (:tr (:td (:a :class "button" :href (nyxt-url 'manual) "Manual"))
                 (:td "Full documentation about Nyxt, how it works and how to configure it."))
            (:tr (:td (:a :class "button" :href (nyxt-url 'changelog) "Change Log"))
                 (:td "Information about changes between Nyxt versions.")))))

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
