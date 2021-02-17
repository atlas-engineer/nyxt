;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defstruct variable-suggestion
  (name))
(defmethod object-string ((variable variable-suggestion))
  (string-downcase (format nil "~s" (variable-suggestion-name variable))))
(defmethod object-display ((variable variable-suggestion))
  (object-string variable))

(defstruct function-suggestion
  (name))
(defmethod object-string ((fun function-suggestion))
  (string-downcase (format nil "~s" (function-suggestion-name fun))))
(defmethod object-display ((fun function-suggestion))
  (object-string fun))

(defstruct class-suggestion
  (name))
(defmethod object-string ((class class-suggestion))
  (string-downcase (format nil "~s" (class-suggestion-name class))))
(defmethod object-display ((class class-suggestion))
  (object-string class))

(defun variable-suggestion-filter ()
  (let* ((variables (mapcar (lambda (v) (make-variable-suggestion :name v))
                            (package-variables))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) variables))))

(defun function-suggestion-filter ()
  (let ((functions (mapcar (lambda (v) (make-function-suggestion :name v))
                           (package-functions))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) functions))))

(defun class-suggestion-filter ()
  (let ((classes (mapcar (lambda (v) (make-class-suggestion :name v))
                         (package-classes))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) classes))))

(defun slot-suggestion-filter ()
  (let ((slots (package-slots)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) slots))))

(define-command describe-variable ()
  "Inspect a variable and show it in a help buffer."
  (let* ((input (variable-suggestion-name
                 (prompt-minibuffer
                  :suggestion-function (variable-suggestion-filter)
                  :input-prompt "Describe variable"))))
    (with-current-html-buffer (buffer
                               (str:concat "*Help-" (symbol-name input) "*")
                               'nyxt/help-mode:help-mode)
      (markup:markup
       (:style (style buffer))
       (:h1 (format nil "~s" input)) ; Use FORMAT to keep package prefix.
       (:pre (documentation input 'variable))
       (:h2 "Current Value:")
       (:pre (object-display (symbol-value input)))))))

(declaim (ftype (function (command)) describe-command*))
(defun describe-command* (command)
  "Display NAME command documentation in a new focused buffer."
  (with-current-html-buffer (buffer
                             (str:concat "*Help-" (symbol-name (sym command)) "*")
                             'nyxt/help-mode:help-mode)
    (let* ((key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                           (sym command)
                                           (all-keymaps))))
           (key-keymapname-pairs (mapcar (lambda (pair)
                                           (list (first pair)
                                                 (keymap:name (second pair))))
                                         key-keymap-pairs))
           (source-file (getf (getf (swank:find-definition-for-thing (command-function command))
                                    :location)
                              :file)))
      (markup:markup
       (:style (style buffer))
       (:h1 (symbol-name (sym command))
            (unless (eq (find-package :nyxt)
                        (symbol-package (sym command)))
              (format nil " (~a)"
                      (package-name (symbol-package (sym command))))))
       (:p (:pre   ; See describe-slot* for why we use :pre.
            ;; TODO: This only displays the first method,
            ;; i.e. the first command of one of the modes.
            ;; Ask for modes instead?
            (documentation (command-function command) t)))
       (:h2 "Bindings")
       (:p (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
       (:h2 (format nil "Source (~a): " source-file))
       (:pre (:code (let ((*print-case* :downcase))
                      (write-to-string (sexp command)))))))))

(defun dump-command-descriptions (file)
  "Dump the command descriptions as an HTML file."
  (with-open-file (f file :direction :output :if-exists :overwrite)
    (format f "~a" (markup:markup
                    (:p "Listed below are the current commands, their
                         documentation, and their source. Non-command
                         based features are currently unlisted.")
                    (:h1 "Commands")))
    (format f "~a" (markup:markup
                    (:style (cl-css:css
                             '((".nyxt-source"
                                :overflow "auto"))))))
    (format f "~{~a ~%~}"
            (loop for command in *command-list*
                  collect (markup:markup
                           (:details
                            (:summary (format nil "~(~a~)" (symbol-name (sym command))))
                            (:p (:pre (documentation (command-function command) t)))
                            (:pre :class "nyxt-source" (:code (let ((*print-case* :downcase))
                                                                (write-to-string (sexp command)))))))))))

(define-command describe-function ()
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (let ((input (prompt-minibuffer
                :input-prompt "Describe function"
                :suggestion-function (function-suggestion-filter))))
    (setf input (function-suggestion-name input))
    (flet ((method-desc (method)
             (markup:markup
              (:h1 (symbol-name input) " " (write-to-string (mopu:method-specializers method)))
              (:pre (documentation method 't))
              (:h2 "Argument list")
              (:p (write-to-string (closer-mop:method-lambda-list method))))))
      (with-current-html-buffer (buffer
                                 (str:concat "*Help-" (symbol-name input) "*")
                                 'nyxt/help-mode:help-mode)
        (if (typep (symbol-function input) 'generic-function)
            (apply #'str:concat (mapcar #'method-desc
                                        (mopu:generic-function-methods
                                         (symbol-function input))))
            (str:concat
             (markup:markup
              (:style (style buffer))
              (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                   (when (macro-function input) " (macro)"))
              (:pre (documentation input 'function))
              (:h2 "Argument list")
              (:p (write-to-string (mopu:function-arglist input))))
             #+sbcl
             (unless (macro-function input)
               (markup:markup
                (:h2 "Type")
                (:p (format nil "~s" (sb-introspect:function-type input)))))))))))

(define-command describe-command ()
  "Inspect a command and show it in a help buffer.
A command is a special kind of function that can be called with
`execute-command' and can be bound to a key."
  (let ((input (prompt-minibuffer
                :input-prompt "Describe command"
                :suggestion-function (command-suggestion-filter))))
    (describe-command* input)))

(defun describe-slot* (slot class &key mention-class-p)      ; TODO: Adapt HTML sections / lists to describe-slot and describe-class.
  (let ((props (mopu:slot-properties (find-class class) slot)))
    (markup:markup
     (:ul
      (:li slot)
      (:ul
       (when mention-class-p
         (list (markup:markup (:li (format nil "Class: ~s" class)))))
       (when (getf props :type)
         (list (markup:markup (:li (format nil "Type: ~s" (getf props :type))))))
       (when (getf props :initform)
         (let* ((initform-string (let ((*print-case* :downcase))
                                   (write-to-string (getf props :initform))))
                (multiline-form? (search (string #\newline) initform-string)))
           (if multiline-form?
               (list (markup:markup (:li "Default value: " (:pre (:code initform-string)))))
               (list (markup:markup (:li "Default value: " (:code initform-string)))))))
       (when (getf props :documentation)
         ;; We use :pre for documentation so that code samples get formatted properly.
         ;; TODO: Parse docstrings and highlight code samples.
         (list (markup:markup (:li "Documentation: " (:pre (getf props :documentation))))))
       (unless (user-class-p class)
         (list (markup:markup
                (:li (:a :class "button"
                         :href (lisp-url `(nyxt::configure-slot ',slot ',class))
                         "Configure"))))))))))

(define-command describe-class ()
  "Inspect a class and show it in a help buffer."
  (let* ((input (class-suggestion-name
                 (prompt-minibuffer
                  :input-prompt "Describe class"
                  :suggestion-function (class-suggestion-filter)))))
    (with-current-html-buffer (buffer
                               (str:concat "*Help-" (symbol-name input) "*")
                               'nyxt/help-mode:help-mode)
      (let* ((slots (class-public-slots input))
             (slot-descs (apply #'str:concat (mapcar (alex:rcurry #'describe-slot* input) slots))))
        (str:concat
         (markup:markup
          (:style (style buffer))
          (:h1 (symbol-name input))
          (:p (:pre (documentation input 'type)))
          (:h2 "Slots:"))
         slot-descs)))))

(defun configure-slot (slot class &key (value nil new-value-supplied-p) (type nil))
  "Set the value of a slot in a users auto-config.lisp.
CLASS can be a class symbol or a list of class symbols, as with
`define-configuration'."
  (flet ((set-slot (slot class input)
           (echo "Slot ~a updated with value ~s." slot input)
           (append-configuration `(define-configuration ,class
                                    ((,slot ,input))))))
    (if new-value-supplied-p
        (progn
          (set-slot slot class value)
          (eval `(define-configuration ,class
                   ((,slot ,value)))))
        (let ((accepted-input
                (loop while t do
                         (let ((input (prompt-minibuffer
                                       :input-prompt (format nil "Configure slot value ~a" slot))))
                           ;; no type specified, no need to keep querying
                           (unless type (return input))
                           (when (typep (read-from-string input) type)
                             (return input))))))
          (set-slot slot class accepted-input)
          (eval `(define-configuration ,class
                   ((,slot (read-from-string ,accepted-input)))))))))

(defun append-configuration (form)
  (with-data-file (file *auto-config-file-path*
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :append)
    (log:info "Appending configuration form ~a to ~s." form (expand-path *auto-config-file-path*))
    (format file "~&~a~%" form)))

(define-command describe-slot ()
  "Inspect a slot and show it in a help buffer."
  (let* ((input (prompt-minibuffer
                 :input-prompt "Describe slot"
                 :suggestion-function (slot-suggestion-filter))))
    (with-current-html-buffer (buffer
                               (str:concat "*Help-" (symbol-name (name input)) "*")
                               'nyxt/help-mode:help-mode)
      (str:concat (markup:markup (:style (style buffer)))
                  (describe-slot* (name input) (class-sym input)
                                  :mention-class-p t)))))

(define-command common-settings ()
  "Configure a set of frequently used settings."
  (with-current-html-buffer (buffer "*Settings*" 'nyxt/help-mode:help-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Common Settings")
     (:p "Set the values for frequently configured
            settings. Changes made will apply to newly created
            buffers.")
     (:h2 "Keybinding style")
     (:p (:a :class "button"
             :href (lisp-url `(nyxt::configure-slot
                               'default-modes
                               '(buffer web-buffer)
                               :value '%slot-default)
                             `(nyxt/emacs-mode:emacs-mode :activate nil)
                             `(nyxt/vi-mode:vi-normal-mode :activate nil))
             "Use default (CUA)"))
     (:p (:a :class "button"
             :href (lisp-url `(nyxt::configure-slot
                               'default-modes
                               '(buffer web-buffer)
                               :value '(append '(emacs-mode) %slot-default))
                             `(nyxt/emacs-mode:emacs-mode :activate t)
                             `(nyxt/vi-mode:vi-normal-mode :activate nil))
             "Use Emacs"))
     (:p (:a :class "button"
             :href (lisp-url `(nyxt::configure-slot
                               'default-modes
                               '(buffer web-buffer)
                               :value '(append '(vi-normal-mode) %slot-default))
                             `(nyxt/vi-mode:vi-normal-mode :activate t)
                             `(nyxt/emacs-mode:emacs-mode :activate nil))
             "Use vi"))
     (:h2 "Default new buffer URL")
     (:a :class "button"
         :href (lisp-url `(nyxt::configure-slot 'default-new-buffer-url 'web-buffer :type 'STRING))
         "Set default new buffer URL")
     (:h2 "Default zoom ratio")
     (:a :class "button"
         :href (lisp-url `(nyxt::configure-slot 'current-zoom-ratio 'buffer))
         "Set default zoom ratio"))))

(define-command describe-bindings ()
  "Show a buffer with the list of all known bindings for the current buffer."
  (with-current-html-buffer (buffer "*Help-bindings" 'nyxt/help-mode:help-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Bindings")
     (:p
      (loop for keymap in (current-keymaps (current-buffer))
            collect (markup:markup
                     (:h3 (keymap:name keymap))
                     (:table
                      (loop for keyspec being the hash-keys in (keymap:keymap-with-parents->map keymap)
                              using (hash-value bound-value)
                            collect (markup:markup
                                     (:tr
                                      (:td keyspec)
                                      (:td (string-downcase bound-value))))))))))))

(defun tls-help (buffer url)
  "This function is invoked upon TLS certificate errors to give users
help on how to proceed."
  ;; Set (url buffer) so that the user can simply reload the page after
  ;; allowing the exception:
  (setf (url buffer) url)
  (html-set
   (markup:markup
    (:h1 (format nil "TLS Certificate Error: ~a" (object-display url)))
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
               (describe-command* (function-command (symbol-function bound-value)))
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
    (pexec ()
      (calispel:!
       channel
       (with-input-from-string (input string)
         (first
          (last
           (loop for object = (read input nil :eof)
                 until (eq object :eof)
                 collect (multiple-value-list  (eval object))))))))
    (calispel:? channel)))

(defun evaluate-async (string)
  "Like `evaluate' but does not block and does not return the result."
  (pexec ()
    (with-input-from-string (input string)
      (loop for object = (read input nil :eof)
            until (eq object :eof)
            collect (funcall-safely (lambda () (eval object)))))))

(defun error-buffer (title text)
  "Print some help."
  (with-current-html-buffer (buffer title 'nyxt/help-mode:help-mode)
    (markup:markup
     (:style (style buffer))
     (:h1 "Error occured:")
     (:p text))))

(defun error-in-new-window (title text)
  (let* ((window (window-make *browser*))
         (error-buffer (error-buffer title text)))
    (window-set-active-buffer window error-buffer)
    error-buffer))

(define-command nyxt-version ()
  "Version number of this version of Nyxt.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(declaim (ftype (function (function-symbol &key (:modes list))) binding-keys))
(defun binding-keys (fn &key (modes (if (current-buffer)
                                        (modes (current-buffer))
                                        (mapcar #'make-instance (default-mode-symbols)))))
  ;; We can't use `(modes (make-instance 'buffer))' because modes are only
  ;; instantiated after the buffer web view, which is not possible if there is
  ;; no *browser*.
  (let ((keymaps (cons (override-map (or (current-buffer)
                                         (make-instance 'user-buffer)))
                       (delete nil (mapcar #'keymap modes)))))
    (or (first (keymap:binding-keys fn keymaps))
        "UNBOUND")))

(define-command help (&key no-history-p)
  "Print help information."
  (with-current-html-buffer (buffer "*Help*" 'nyxt/help-mode:help-mode
                             :no-history-p no-history-p)
    (markup:markup
     (:style (style buffer))
     (:style (cl-css:css '((:h2
                            :font-weight 300
                            :padding-top "10px"))))
     (:h1 "Welcome to Nyxt ☺")
     (:p "Attention: Nyxt is under active development. Feel free to "
         (:a :href "https://github.com/atlas-engineer/nyxt/issues"
             "report")
         " bugs, instabilities or feature wishes.")
     (:p "You can help with Nyxt development by supporting us in various ways:"
         (:ul
          (:li "Support continuous development on "
               (:a :href "https://www.patreon.com/nyxt"
                   "Patreon")
               ".")
          (:li "Spread the word on social media and "
               (:a :href "https://github.com/atlas-engineer/nyxt"
                   "star the project on GitHub")
               ".")))
     (:hr )
     (:h2 "Quick configuration")
     (:p (:a :class "button" :href (lisp-url `(nyxt::common-settings)) "Common settings")
         " Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level.")
     (:h2 "Documentation")
     (:p (:a :class "button" :href (lisp-url `(nyxt::describe-bindings)) "List bindings")
         " List all bindings for the current buffer.")
     (:p (:a :class "button" :href (lisp-url `(nyxt::tutorial)) "Tutorial")
         " An introduction to Nyxt core concepts.")
     (:p (:a :class "button" :href (lisp-url `(nyxt::manual)) "Manual")
         " Full documentation about Nyxt, how it works and how to configure it."))))

(define-command manual ()
  "Show the manual."
  (with-current-html-buffer (buffer "*Manual*" 'nyxt/help-mode:help-mode)
    (str:concat (markup:markup (:style (style buffer)))
                (manual-content))))

(define-command tutorial ()
  "Show the tutorial."
  (with-current-html-buffer (buffer "*Tutorial*" 'nyxt/help-mode:help-mode)
    (str:concat
     (markup:markup
      (:style (style buffer))
      (:h1 "Nyxt tutorial")
      (:p "The following tutorial introduces the core concepts and the
basic usage.  For more details, especially regarding the configuration, see
the "
          (:code (command-markup 'manual)) "."))
     (tutorial-content))))

(define-command copy-system-information ()
  "Save system information into the clipboard."
  (let* ((*print-length* nil)
         (nyxt-information (format nil
                                   (str:concat "Nyxt version: ~a ~%"
                                               "Operating system kernel: ~a ~a~%"
                                               "Lisp implementation: ~a ~a~%"
                                               "Features: ~a~%")
                                   +version+
                                   (software-type) (software-version)
                                   (lisp-implementation-type) (lisp-implementation-version)
                                   *features*)))
    (copy-to-clipboard nyxt-information)
    (log:info nyxt-information)
    (echo "System information copied to clipboard.")))

(define-command dashboard ()
  "Print a dashboard. Usable as a buffer-fn for make-startup-function."
  (flet ((list-bookmarks (&key (separator " → "))
           (with-data-unsafe (bookmarks (bookmarks-path (current-buffer)))
             (loop for bookmark in bookmarks
                   collect (markup:markup (:li (title bookmark) separator
                                               (:a :href (object-string (url bookmark))
                                                   (object-display (url bookmark)))))))))
    (let ((dashboard-style (cl-css:css
                            '((body
                               :margin-top 0
                               :margin-bottom 0)
                              ("a"
                               :color "gray")
                              ("#title"
                               :font-size "400%")
                              (.section
                               :border-top "solid lightgray"
                               :margin-top "10px"
                               :overflow "scroll"
                               :min-height "150px")
                              (".section h3"
                               :color "dimgray")
                              ("#container"
                               :display "flex"
                               :flex-flow "column"
                               :height "100vh")
                              ("ul"
                               :list-style-type "circle")))))
      (with-current-html-buffer (buffer "*Dashboard*" 'base-mode)
        (markup:markup
         (:style (style buffer))
         (:style dashboard-style)
         (:div :id "container"
               (:div :style "height: 210px"
                     (:h1 :id "title" "Nyxt " (:span :style "color: lightgray" "browser ☺"))
                     (:h3 (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
                     (:a :class "button" :href (lisp-url `(nyxt::restore-session-by-name)) "🗁 Restore Session")
                     (:a :class "button" :href (lisp-url `(nyxt::execute-command)) "⚙ Execute Command")
                     (:a :class "button" :href (lisp-url `(nyxt::manual)) "🕮 Manual")
                     (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
               (:div :class "section" :style "flex: 3"
                     (:h3 "🏷 " (:b "Bookmarks"))
                     (:ul (list-bookmarks)))
               (:div :class "section" :style "flex: 5"
                     (:h3 "🗐 " (:b "Recent URLs"))
                     (:ul (history-html-list)))))))))
