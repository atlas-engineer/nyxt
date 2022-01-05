;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class function-source (prompter:source)
  ((prompter:name "Functions")
   (prompter:constructor (package-functions))))

(defmethod prompter:object-attributes ((symbol symbol))
  `(("Name" ,(write-to-string symbol))
    ("Documentation"
     ,(or (cond
            ((fboundp symbol)
             (first (sera:lines (documentation symbol 'function))))
            ((and (find-class symbol nil)
                  (mopu:subclassp (find-class symbol) (find-class 'standard-object)))
             (first (sera:lines (documentation symbol 'type))))
            (t
             (first (sera:lines (documentation symbol 'variable)))))
          ""))))

(define-class class-source (prompter:source)
  ((prompter:name "Classes")
   (prompter:constructor (package-classes))))

(define-class slot-source (prompter:source)
  ((prompter:name "Slots")
   (prompter:constructor (package-slots))))

(define-class variable-source (prompter:source)
  ((prompter:name "Variables")
   (prompter:constructor (package-variables))))

(define-command describe-any ()
  "Inspect anything and show it in a help buffer."
  (prompt
   :prompt "Describe:"
   :sources (list (make-instance 'variable-source
                                 :actions (list (make-command describe-variable* (variable)
                                                  (describe-variable :variable variable))))
                  (make-instance 'function-source
                                 :actions (list (make-command describe-function* (function)
                                                  (describe-function :function function))))
                  (make-instance 'user-command-source
                                 :actions (list (make-command describe-command* (command)
                                                  (describe-command :command command))))
                  (make-instance 'class-source
                                 :actions (list (make-command describe-class* (class)
                                                  (describe-class :class class))))
                  (make-instance 'slot-source
                                 :actions (list (make-command describe-slot** (slot)
                                                  (describe-slot :slot slot)))))))

(defun value->html (value &key (help-mode (current-mode 'help)))
  "Return the HTML representation of VALUE."
  (spinneret:with-html-string
    (cond
      ((consp value)
       (:ul
        (loop for e in value
              for i below (length value)
              collect (:li (:a :href (lisp-url `(nyxt::%describe-value
                                                 (nth ,i ,(nyxt/help-mode:inspected-value help-mode))))
                               e)))))
      ((has-attributes-method-p value)
       (:ul
        (loop for (attribute-key attribute-value) in (prompter:object-attributes value)
              collect (:li attribute-key ": " (:code attribute-value)))))
      (t
       (:raw (princ-to-string value))))))

(define-internal-page-command describe-value
    (&key value)
    (buffer "*Help-value*" 'nyxt/help-mode:help-mode)
  "Inspect VALUE and show it in a help buffer."
  (when value
    (let ((help-mode (find-mode buffer 'help-mode)))
      (setf (nyxt/help-mode:inspected-value help-mode) value)
      (spinneret:with-html-string
        (:style (style buffer))
        (:h1 (princ-to-string value))
        (:p (:raw (value->html value :help-mode help-mode)))))))

(defun has-attributes-method-p (object)
  "Return non-nil if OBJECT has `prompter:object-attributes' specialization."
  (has-method-p object #'prompter:object-attributes))

(define-internal-page-command describe-variable
    (&key (variable
           (first (prompt
                   :prompt "Describe variable:"
                   :sources (make-instance 'variable-source)))))
    (buffer (str:concat "*Help-" (symbol-name variable) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a variable and show it in a help buffer."
  (let ((help-mode (find-mode buffer 'help-mode)))
    (setf (nyxt/help-mode:inspected-value help-mode) variable)
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (format nil "~s" variable)) ; Use FORMAT to keep package prefix.
      (:pre (documentation variable 'variable))
      (:h2 "Current Value:")
      (:p (:raw (value->html (symbol-value variable) :help-mode help-mode))))))

(define-internal-page-command describe-function
    (&key (function (first (prompt
                            :prompt "Describe function"
                            :sources (make-instance 'function-source)))))
    (buffer (str:concat "*Help-" (symbol-name function) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (if function-suggestion
      (let ((input function-suggestion))
        (flet ((method-desc (method)
                 (spinneret:with-html-string
                   (:h1 (symbol-name input) " " (write-to-string (mopu:method-specializers method)))
                   (:pre (documentation method 't))
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
                (:pre (documentation input 'function))
                (:raw (apply #'str:concat (mapcar #'method-desc
                                                  (mopu:generic-function-methods
                                                   (symbol-function input))))))
              (spinneret:with-html-string
                (:style (style buffer))
                (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                     (when (macro-function input) " (macro)"))
                (:pre (documentation input 'function))
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

(define-internal-page-command describe-command
    (&key (command (first (prompt
                           :prompt "Describe command"
                           :sources (make-instance 'user-command-source)))))
    (buffer (str:concat "*Help-" (symbol-name (name command)) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a command and show it in a help buffer.
A command is a special kind of function that can be called with
`execute-command' and can be bound to a key."
  (let* ((key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                         (name command)
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
      (:p (:pre
           ;; TODO: This only displays the first method,
           ;; i.e. the first command of one of the modes.
           ;; Ask for modes instead?
           (documentation (fn command) t)))
      (:h2 "Bindings")
      (:p (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
      (:h2 (format nil "Source~a: " (if source-file
                                        (format nil " (~a)" source-file)
                                        "")))
      (:pre (:code (let ((*print-case* :downcase))
                     (write-to-string (sexp command))))))))

(define-internal-page-command describe-slot
    (&key (slot (first (prompt
                        :prompt "Describe slot"
                        :sources (make-instance 'slot-source)))))
    (buffer (str:concat "*Help-" (symbol-name (name slot)) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a slot and show it in a help buffer."
  (str:concat (spinneret:with-html-string (:style (style buffer)))
              (describe-slot* (name slot) (class-sym slot)
                              :mention-class-p t)))

(defun describe-slot* (slot class &key mention-class-p)
  "Create the HTML that represents a slot."
  ;; TODO: Adapt HTML sections / lists to describe-slot and describe-class.
  ;; TODO: Parse docstrings and highlight code samples.
  (let ((props (mopu:slot-properties (find-class class) slot)))
    (spinneret:with-html-string
      (:ul
       (:li slot)
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
          ;; We use :pre for documentation so that code samples get formatted properly.
          (:li "Documentation: " (:pre (getf props :documentation))))
        (unless (user-class-p class)
          (:li (:a :class "button"
                   :href (lisp-url `(nyxt::configure-slot ',slot ',class :type ',(getf props :type)))
                   "Configure"))))))))

(define-internal-page-command describe-class
    (&key (class (first (prompt
                         :prompt "Describe class"
                         :sources (make-instance 'class-source)))))
    (buffer (str:concat "*Help-" (symbol-name class) "*")
            'nyxt/help-mode:help-mode)
  "Inspect a class and show it in a help buffer."
  (let* ((slots (class-public-slots class))
         (slot-descs (apply #'str:concat (mapcar (alex:rcurry #'describe-slot* class) slots))))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (symbol-name class))
      (:p (:pre (documentation class 'type)))
      (when (mopu:direct-superclasses class)
        (:h2 "Direct superclasses:")
        (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-superclasses class))
                   collect (:li (:a :href (about-url 'describe-class :class class-name) class-name)))))
      (when (mopu:direct-subclasses class)
        (:h2 "Direct subclasses:")
        (:ul (loop for class-name in (mapcar #'class-name (mopu:direct-subclasses class))
                   collect (:li (:a :href (about-url 'describe-class :class class-name) class-name)))))
      (:h2 "Slots:")
      (:raw slot-descs)
      (:h2 "Methods:")
      (:ul (loop for method in (remove-if
                                #'listp (mapcar #'mopu:generic-function-name
                                                (mopu:generic-functions class)))
                 collect (:li (:a :href (about-url 'describe-function :function method) method)))))))

(define-internal-page-command nyxt/prompt-buffer-mode::describe-prompt-buffer
    (&key (prompt-buffer (current-prompt-buffer)))
    (buffer (str:concat "*Help-" (prompter:prompt prompt-buffer) "-prompter*")
            'nyxt/help-mode:help-mode)
  "Describe a prompt buffer instance."
  (let* ((modes (modes prompt-buffer))
         (sources (prompter:sources prompt-buffer)))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 (prompter:prompt prompt-buffer))
      (:p (:pre (documentation prompt-buffer 'type)))
      (:h2 "Modes:")
      (:ul
       (loop for mode in modes
             collect (:li (:a :href
                              (about-url
                               'describe-class
                               :class (sera:class-name-of mode))
                              (string (sera:class-name-of mode))))))
      (:h2 "Sources:")
      (:ul
       (loop for source in sources
             collect (:li (:a :href
                              (about-url
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
           (echo "Update slot ~s to ~s." slot input)
           (append-configuration `(define-configuration ,class
                                    ((,slot ,input))))))
    (if new-value-supplied-p
        (progn
          (set-slot slot class value)
          (eval `(define-configuration ,class
                   ((,slot ,value)))))
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
  (with-data-file (file *auto-config-file-path*
                        :direction :output
                        :if-exists :append)
    (log:info "Appending to ~s:~&~s" (expand-path *auto-config-file-path*) form)
    (format file format-directive form)))

(define-internal-page-command common-settings ()
    (buffer "*Settings*" 'nyxt/help-mode:help-mode)
  "Configure a set of frequently used settings."
  (let ((spinneret:*html-style* :tree))
    (spinneret:with-html-string
      (:style (style (current-buffer)))
      (:h1 "Common Settings")
      (:p "Set the values for frequently configured settings. "
          "Changes only apply to newly created buffers.")
      (:h2 "Keybinding style")
      (:p (:a :class "button"
              :href (lisp-url `(nyxt::configure-slot
                                'default-modes 'buffer
                                :value '%slot-default%)
                              `(nyxt/emacs-mode:emacs-mode :activate nil)
                              `(nyxt/vi-mode:vi-normal-mode :activate nil)
                              `(nyxt::%common-settings))
              "Use default (CUA)"))
      (:p (:a :class "button"
              :href (lisp-url `(nyxt::configure-slot
                                'default-modes 'buffer
                                :value '(append '(emacs-mode) %slot-default%))
                              `(nyxt/emacs-mode:emacs-mode :activate t)
                              `(nyxt/vi-mode:vi-normal-mode :activate nil)
                              `(nyxt::%common-settings))
              "Use Emacs"))
      (:p (:a :class "button"
              :href (lisp-url `(progn
                                 (nyxt::configure-slot
                                  'default-modes 'buffer
                                  :value '(append '(vi-normal-mode) %slot-default%))
                                 (nyxt::configure-slot
                                  'default-modes
                                  'prompt-buffer
                                  :value '(append '(vi-insert-mode) %slot-default%)))
                              `(nyxt/vi-mode:vi-normal-mode :activate t)
                              `(nyxt/emacs-mode:emacs-mode :activate nil)
                              `(nyxt::%common-settings))
              "Use vi"))
      (:h2 "Default new buffer URL")
      (:a :class "button"
          :href (lisp-url `(nyxt::configure-slot 'default-new-buffer-url 'browser :type 'STRING)
                          `(nyxt::%common-settings))
          "Set default new buffer URL")
      (:h2 "Default zoom ratio")
      (:a :class "button"
          :href (lisp-url `(nyxt::configure-slot 'current-zoom-ratio 'buffer)
                          `(nyxt::%common-settings*))
          "Set default zoom ratio")
      (:h2 "Disable compositing")
      (:p "On some systems, compositing can cause issues with rendering. If you
     are experiencing blank web-views, you can try to disable compositing. After
     disabling compositing, you will need to restart Nyxt.")
      (:a :class "button"
          :href (lisp-url `(nyxt::append-configuration
                            '(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1"))
                          `(nyxt::%common-settings))
          "Disable compositing"))))

(define-internal-page-command describe-bindings ()
    (buffer "*Help-bindings*" 'base-mode)
  "Show a buffer with the list of all known bindings for the current buffer."
  (spinneret:with-html-string
    (:style (style buffer))
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
               (describe-command :command (function-command (symbol-function bound-value)))
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
    (run-thread
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
  (run-thread
    (with-input-from-string (input string)
      (loop for object = (read input nil :eof)
            until (eq object :eof)
            collect (funcall (lambda () (eval object)))))))

(define-internal-page-command error-buffer (&key title text)
    (buffer title 'nyxt/help-mode:help-mode)
  "Print some help."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Error occured:")
    (:pre text)))

(defun error-in-new-window (title text)
  (let* ((window (window-make *browser*))
         (error-buffer (error-buffer :title title :text text)))
    (window-set-buffer window error-buffer)
    error-buffer))

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
  (let ((keymaps (cons (override-map (or (current-buffer)
                                         (make-instance 'user-buffer)))
                       (delete nil (mapcar #'keymap modes)))))
    (or (first (keymap:binding-keys fn keymaps))
        "UNBOUND")))

(define-internal-page-command help ()
    (buffer "*Help*" 'nyxt/help-mode:help-mode)
  "Open up a small help buffer."
  (spinneret:with-html-string
    (:style (style buffer))
    (:style (cl-css:css '(("#documentation .button"
                           :min-width "100px"))))
    (:h1 "Welcome to Nyxt :-)")
    (:p (:a :href "https://nyxt.atlas.engineer" "https://nyxt.atlas.engineer"))
    (:h2 "Quick configuration")
    (:p (:a :class "button" :href (about-url 'common-settings) "Common settings")
        " Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level.")
    (:h2 "Documentation")
    (:table :id "documentation"
            (:tr (:td (:a :class "button" :href (about-url 'describe-bindings) "List bindings"))
                 (:td "List all bindings for the current buffer."))
            (:tr (:td (:a :class "button" :href (about-url 'nyxt::edit-user-file-with-external-editor) "Edit user files"))
                 (:td "Edit user configuration and other files in external text editor."))
            (:tr (:td (:a :class "button" :href (about-url 'tutorial) "Tutorial"))
                 (:td "An introduction to Nyxt core concepts."))
            (:tr (:td (:a :class "button" :href (about-url 'manual) "Manual"))
                 (:td "Full documentation about Nyxt, how it works and how to configure it."))
            (:tr (:td (:a :class "button" :href (about-url 'changelog) "Change Log"))
                 (:td "Information about changes between Nyxt versions.")))))

(define-internal-page-command manual ()
    (buffer "*Manual*" 'nyxt/help-mode:help-mode)
  "Show the manual."
  (spinneret:with-html-string (:style (style buffer))
    (:style (cl-css:css '(("body"
                           :max-width "80ch"))))
    (:raw (manual-content))))

(define-internal-page-command tutorial ()
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

(define-internal-page-command show-system-information ()
    (buffer "*System information*" 'base-mode)
  "Show buffer with Lisp version, Lisp features, OS kernel, etc.
System information is also saved into the clipboard."
  (let* ((*print-length* nil)
         (nyxt-information (system-information)))
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 "System information")
      (:pre nyxt-information))
    (copy-to-clipboard nyxt-information)
    (log:info nyxt-information)
    (echo "System information copied to clipboard.")))

(define-internal-page-command dashboard ()
    (buffer "*Dashboard*" 'base-mode)
  "Print a dashboard."
  (flet ((list-bookmarks (&key (separator " → "))
           (with-data-unsafe (bookmarks (bookmarks-path (current-buffer)))
             (spinneret:with-html-string
               (loop for bookmark in bookmarks
                     collect (:li (title bookmark) separator
                                  (:a :href (render-url (url bookmark))
                                      (render-url (url bookmark)))))))))
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
                             (".section h3"
                              :color theme:tertiary)
                             ("#container"
                              :display "flex"
                              :flex-flow "column"
                              :height "100vh")
                             ("ul"
                              :list-style-type "circle"))))
      (spinneret:with-html-string
        (:style (style buffer))
        (:style dashboard-style)
        (:div :id "container"
              (:div
               (:h1 :id "title" "Nyxt " (:span :id "subtitle" "browser ☺"))
               (:h3 (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
               (:a :class "button" :href (lisp-url `(nyxt::restore-history-by-name)) "🗁 Restore Session")
               (:a :class "button" :href (lisp-url `(nyxt::manual)) "🕮 Manual")
               (:a :class "button" :href (lisp-url `(nyxt::execute-command)) "≡ Execute Command")
               (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
              (:div :class "section" :style "flex: 3"
                    (:h3 (:b "Bookmarks"))
                    (:ul (:raw (list-bookmarks))))
              (:div :class "section" :style "flex: 5"
                    (:h3 (:b "Recent URLs"))
                    (:ul (:raw (history-html-list)))))))))

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
