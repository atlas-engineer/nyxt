(in-package :nyxt)

(defstruct variable-candidate
  (name))
(defmethod object-string ((variable variable-candidate))
  (string-downcase (format nil "~s" (variable-candidate-name variable))))
(defmethod object-display ((variable variable-candidate))
  (object-string variable))

(defstruct function-candidate
  (name))
(defmethod object-string ((fun function-candidate))
  (string-downcase (format nil "~s" (function-candidate-name fun))))
(defmethod object-display ((fun function-candidate))
  (object-string fun))

(defstruct class-candidate
  (name))
(defmethod object-string ((class class-candidate))
  (string-downcase (format nil "~s" (class-candidate-name class))))
(defmethod object-display ((class class-candidate))
  (object-string class))

(defun variable-completion-filter ()
  (let* ((variables (mapcar (lambda (v) (make-variable-candidate :name v))
                            (package-variables))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) variables))))

(defun function-completion-filter ()
  (let ((functions (mapcar (lambda (v) (make-function-candidate :name v))
                           (package-functions))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) functions))))

(defun class-completion-filter ()
  (let ((classes (mapcar (lambda (v) (make-class-candidate :name v))
                         (package-classes))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) classes))))

(defun slot-completion-filter ()
  (let ((slots (package-slots)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) slots))))

(define-command describe-variable ()
  "Inspect a variable and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :completion-function (variable-completion-filter)
                        :input-prompt "Describe variable")))
    (let* ((input (variable-candidate-name input))
           (help-buffer (nyxt/help-mode:help-mode
                         :activate t
                         :buffer (make-buffer
                                  :title (str:concat "*Help-"
                                                     (symbol-name input)
                                                     "*"))))
           (help-contents (markup:markup
                           (:h1 (format nil "~s" input)) ; Use FORMAT to keep package prefix.
                           (:pre (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:pre (object-display (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help)
      (set-current-buffer help-buffer))))

(declaim (ftype (function (command)) describe-command*))
(defun describe-command* (command)
  "Display NAME command documentation in a new focused buffer."
  (let* ((title (str:concat "*Help-" (symbol-name (sym command)) "*"))
         (help-buffer (nyxt/help-mode:help-mode
                       :activate t
                       :buffer (make-buffer :title title)))
         (key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                         (sym command)
                                         (all-keymaps))))
         (key-keymapname-pairs (mapcar (lambda (pair)
                                         (list (first pair)
                                               (keymap:name (second pair))))
                                       key-keymap-pairs))
         (source-file (getf (getf (swank:find-definition-for-thing (command-function command))
                                  :location)
                            :file))
         (help-contents (markup:markup
                         (:h1 (symbol-name (sym command))
                              (unless (eq (find-package :nyxt)
                                          (symbol-package (sym command)))
                                (format nil " (~a)"
                                        (package-name (symbol-package (sym command))))))
                         (:p (:pre ; See describe-slot* for why we use :pre.
                              ;; TODO: This only displays the first method,
                              ;; i.e. the first command of one of the modes.
                              ;; Ask for modes instead?
                                   (documentation (command-function command) t)))
                         (:h2 "Bindings")
                         (:p (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
                         (:h2 (format nil "Source (~a): " source-file))
                         (:pre (:code (let ((*print-case* :downcase))
                                        (write-to-string (sexp command)))))))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ffi-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

(define-command describe-function ()
  "Inspect a function and show it in a help buffer.
For generic functions, describe all the methods."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Describe function"
                        :completion-function (function-completion-filter))))
    (setf input (function-candidate-name input))
    (flet ((method-desc (method)
             (markup:markup
              (:h1 (symbol-name input) " " (write-to-string (mopu:method-specializers method)))
              (:pre (documentation method 't))
              (:h2 "Argument list")
              (:p (write-to-string (closer-mop:method-lambda-list method))))))
      (let* ((help-buffer (nyxt/help-mode:help-mode
                           :activate t
                           :buffer (make-buffer
                                    :title (str:concat "*Help-"
                                                       (symbol-name input)
                                                       "*"))))
             (help-contents (if (typep (symbol-function input) 'generic-function)
                                (apply #'str:concat (mapcar #'method-desc
                                                            (mopu:generic-function-methods
                                                             (symbol-function input))))
                                (str:concat
                                 (markup:markup
                                  (:h1 (format nil "~s" input) ; Use FORMAT to keep package prefix.
                                       (when (macro-function input) " (macro)"))
                                  (:pre (documentation input 'function))
                                  (:h2 "Argument list")
                                  (:p (write-to-string (mopu:function-arglist input))))
                                 #+sbcl
                                 (unless (macro-function input)
                                   (markup:markup
                                    (:h2 "Type")
                                    (:p (format nil "~s" (sb-introspect:function-type input))))))))
             (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                       (ps:lisp help-contents)))))
        (ffi-buffer-evaluate-javascript help-buffer insert-help)
        (set-current-buffer help-buffer)))))

(define-command describe-command ()
  "Inspect a command and show it in a help buffer.
A command is a special kind of function that can be called with
`execute-command' and can be bound to a key."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Describe command"
                        :completion-function (command-completion-filter))))
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
         (list (markup:markup (:li "Documentation: " (:pre (getf props :documentation)))))))))))

(define-command describe-class ()
  "Inspect a class and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Describe class"
                        :completion-function (class-completion-filter))))
    (let* ((input (class-candidate-name input))
           (help-buffer (nyxt/help-mode:help-mode
                         :activate t
                         :buffer (make-buffer
                                  :title (str:concat "*Help-"
                                                     (symbol-name input)
                                                     "*"))))
           (slots (class-public-slots input))
           (slot-descs (apply #'str:concat (mapcar (alex:rcurry #'describe-slot* input) slots)))
           (help-contents (str:concat
                           (markup:markup
                            (:h1 (symbol-name input))
                            (:p (:pre (documentation input 'type)))
                            (:h2 "Slots:"))
                           slot-descs))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help)
      (set-current-buffer help-buffer))))

(define-command describe-slot ()
  "Inspect a slot and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Describe slot"
                        :completion-function (slot-completion-filter))))
    (let* ((help-buffer (nyxt/help-mode:help-mode
                         :activate t
                         :buffer (make-buffer
                                  :title (str:concat "*Help-"
                                                     (symbol-name (name input))
                                                     "*"))))

           (help-contents (describe-slot* (name input) (class-sym input)
                                          :mention-class-p t))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help)
      (set-current-buffer help-buffer))))

(define-command describe-bindings ()
  "Show a buffer with the list of all known bindings for the current buffer."
  (let* ((title (str:concat "*Help-bindings"))
         (help-buffer (nyxt/help-mode:help-mode
                       :activate t
                       :buffer (make-buffer :title title)))
         (help-contents
           (markup:markup
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
                                             (:td (string-downcase bound-value)))))))))))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ffi-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

(defun tls-help (buffer url)
  "This function is invoked upon TLS certificate errors to give users
help on how to proceed."
  (declare (ignore buffer))             ; TODO: Display tls-help in buffer with TLS error.
  (let* ((help-buffer (nyxt/help-mode:help-mode
                       :activate t
                       :buffer (make-buffer :title "TLS Error")))
         (help-contents
           (markup:markup
            (:h1 (format nil "TLS Certificate Error: ~a" url))
            (:p "The address you are trying to visit has an invalid
certificate. By default Nyxt refuses to establish a secure connection
to a host with an erroneous certificate (e.g. self-signed ones). This
could mean that the address you are attempting the access is
compromised.")
            (:p "If you trust the address
nonetheless, you can add an exception for the current hostname with
=add-domain-to-certificate-whitelist=.  The =certificate-whitelist-mode= must be
active for the current buffer (which is the default).")
            (:p "You can persist hostname exceptions in your init
file, see the =add-domain-to-certificate-whitelist= documentation.")))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ffi-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

(defun describe-key-dispatch-input (event buffer window printable-p)
  "Display documentation of the value bound to the keys pressed by the user.
Cancel with 'escape escape'.
Input is not forwarded.
This function can be used as a `window' `input-dispatcher'."
  (declare (ignore event buffer printable-p))
  (handler-case
      (progn
        (with-accessors ((key-stack key-stack)) *browser*
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
               (echo "Unbound.")
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
      (setf (key-stack *browser*) nil)
      (setf (input-dispatcher window) #'dispatch-input-event)))
  ;; Never forward events.
  t)

(define-command describe-key ()
  "Display binding of user-inputted keys."
  (setf (input-dispatcher (current-window)) #'describe-key-dispatch-input)
  (echo "Press a key sequence to describe (cancel with 'escape escape'):"))

(defun evaluate (string)
  "Evaluate all expressions in string and return a list of values.
This does not use an implicit PROGN to allow evaluating top-level expressions."
  (with-input-from-string (input string)
    (loop for object = (read input nil :eof)
          until (eq object :eof)
          collect (eval object))))

(define-command command-evaluate ()     ; TODO: Rename to `evaluate-lisp'?
  "Evaluate a form."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Evaluate Lisp")))
    (let* ((result-buffer (nyxt/help-mode:help-mode
                           :activate t
                           :buffer (make-buffer
                                    ;; TODO: Reuse buffer / create REPL mode.
                                    :title "*List Evaluation*")))
           (results (handler-case (evaluate input)
                      (error (c) (list (format nil "Error: ~a" c)))))
           (result-contents (apply #'concatenate 'string
                                   (markup:markup
                                    (:h1 "Form")
                                    (:pre (:code input))
                                    (:h1 "Result"))
                                   (loop for result in results
                                         collect (markup:markup
                                                  (:pre (:code (object-display result)))))))
           (insert-results (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp result-contents)))))
      (ffi-buffer-evaluate-javascript result-buffer insert-results)
      (set-current-buffer result-buffer))))

(defun error-buffer (title text)
  "Print some help."
  (let* ((error-buffer (nyxt/help-mode:help-mode :activate t
                                                 :buffer (make-buffer :title title)))
         (error-contents
           (markup:markup
            (:h1 "Error occured:")
            (:p text)))
         (insert-error (ps:ps (setf (ps:@ document Body |innerHTML|)
                                    (ps:lisp error-contents)))))
    (ffi-buffer-evaluate-javascript error-buffer insert-error)
    error-buffer))

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

(define-command messages ()
  "Show the *Messages* buffer."
  (let ((buffer (find-if (lambda (b)
                           (string= "*Messages*" (title b)))
                         (buffer-list))))
    (unless buffer
      (setf buffer (nyxt/help-mode:help-mode :activate t
                                             :buffer (make-buffer :title "*Messages*"))))
    (let* ((content
             (apply #'markup:markup*
                    '(:h1 "Messages")
                    (mapcar (lambda (message)
                              (list :p message))
                            (reverse (messages-content *browser*)))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))

(define-command clear-messages ()       ; TODO: Move to lod-mode?
  "Clear the *Messages* buffer."
  (setf (messages-content *browser*) '())
  (echo "Messages cleared."))

(declaim (ftype (function (function-symbol &key (:modes list))) binding-keys))
(defun binding-keys (fn &key (modes (modes (current-buffer))))
  (let ((keymaps (cons (override-map (current-buffer))
                       (delete nil (mapcar #'keymap modes)))))
    (or (first (keymap:binding-keys fn keymaps))
        "UNBOUND")))

(define-command help ()
  "Print help information."
  (let ((help-buffer (nyxt/help-mode:help-mode :activate t
                                               :buffer (make-buffer :title "*Help*"))))
    (set-current-buffer help-buffer)
    (let* ((help-contents
             (markup:markup
              (:h1 "Getting started")
              (:p (:b "Warning: ") "Nyxt is under active development. Feel free to "
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
              (:h2 "Quickstart keys")
              (:ul
               (:li (:code (binding-keys 'set-url)) ": Load URL")
               (:li (:code (binding-keys 'set-url-new-buffer)) ": Load URL in new tab")
               (:li (:code (binding-keys 'switch-buffer-previous)) ", " (:code (binding-keys 'switch-buffer-next)) ": Switch tab")
               (:li (:code (binding-keys 'nyxt/web-mode:history-backwards)) ": Backwards history")
               (:li (:code (binding-keys 'nyxt/web-mode:history-forwards)) ": Forwards history")
               (:li (:code (binding-keys 'nyxt/web-mode:follow-hint)) ": Follow link in current buffer")
               (:li (:code (binding-keys 'nyxt/web-mode:follow-hint-new-buffer)) ": Follow link in new buffer")
               (:li (:code (binding-keys 'quit)) ": Quit")
               (:li (:code (binding-keys 'execute-command)) ": Run a command by name")
               (:li (:code (binding-keys 'describe-bindings)) ": List all bindings for the current tab"))
              (:p "Legend:")
              (:ul
               (:li (:code "control") " (" (:code "C") "): Control key")
               (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
               (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
               (:li (:code "shift") " (" (:code "s") "): Shift key"))

              (:p "Nyxt proposes several " (:i "binding schemes") ", for instance CUA, Emacs, VI."
                  " For instance, call the " (:code "vi-normal-mode") " command to switch to VI bindings."
                  " To enable it by default, see the command documentation with "
                  (:code (binding-keys 'execute-command) " describe-command") " (bound to "
                  (:code (binding-keys 'describe-command)) ").")

              (:h2 "Documentation")
              (:p "The " (:i "minibuffer") " lets you fuzzy-search all commands."
                  " Press " (:code (binding-keys 'execute-command))
                  " then type " (:code "describe") " to list all documentation-related commands."
                  " These commands can display the documentation of all Nyxt components.")
              (:p "An introduction to Nyxt core concepts can be consulted with the "
                  (:code "tutorial") " command (" (:code (binding-keys 'tutorial)) ").")
              (:p "For full documentation about Nyxt, how it works and how to configure it please see the "
                  (:code "manual") " command (" (:code (binding-keys 'manual)) ").")
              (:p "The manual covers the extensibility capabilities of Nyxt,
from the inspection of the internals to the the creation of new modes and
commands.")))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help))
    help-buffer))

(define-command tutorial ()
  "Show the tutorial."
  (let ((help-buffer (nyxt/help-mode:help-mode
                      :activate t
                      :buffer (make-buffer :title "*Tutorial*"))))
    (set-current-buffer help-buffer)
    (let* ((help-contents
             (str:concat
              (markup:markup
               (:h1 "Nyxt tutorial")
               (:p "The following tutorial introduces the core concepts and the
basic usage.  For more details, especially regarding the configuration, see
the "
                   (:code (command-markup 'manual)) "."))
              (tutorial-content)))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help))
    help-buffer))

(define-command manual ()
  "Show the manual."
  (let ((help-buffer (nyxt/help-mode:help-mode
                      :activate t
                      :buffer (make-buffer :title "*Manual*"))))
    (set-current-buffer help-buffer)
    (let* ((help-contents (manual-content))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help))
    help-buffer))

(define-command copy-system-information ()
  "Command to dump the system information and copy it to the clipboard"
  (let* ((*print-length* nil)
         (nyxt-information (format nil "Nyxt Version: ~a ~%Lisp Implementation: ~a ~%Lisp Version: ~a ~%Operating System: ~a ~a ~%Features: ~a"
                                   +version+ (lisp-implementation-type) (lisp-implementation-version)
                                   (software-type) (software-version) *features*)))
    (copy-to-clipboard nyxt-information)
    (log:info nyxt-information)
    (echo "System information copied to clipboard.")))
