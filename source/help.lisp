(in-package :next)

(defun variable-completion-filter ()
  (let ((variables (package-variables)))
    (lambda (input)
      (fuzzy-match input variables))))

(defun function-completion-filter ()
  (let ((functions (package-functions)))
    (lambda (input)
      (fuzzy-match input functions))))

(defun class-completion-filter ()
  (let ((classes (package-classes)))
    (lambda (input)
      (fuzzy-match input classes))))

(defun slot-completion-filter ()
  (let ((slots (package-slots)))
    (lambda (input)
      (fuzzy-match input slots))))

(define-command describe-variable ()
  "Inspect a variable and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :completion-function (variable-completion-filter)
                        :input-prompt "Describe variable")))
    (let* ((help-buffer (next/help-mode:help-mode
                         :activate t
                         :buffer (make-buffer
                                  :title (str:concat "*Help-"
                                                     (symbol-name input)
                                                     "*"))))
           (help-contents (markup:markup
                           (:h1 (symbol-name input))
                           (:p (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:p (write-to-string (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help)
      (set-current-buffer help-buffer))))

(declaim (ftype (function (command)) describe-command*))
(defun describe-command* (command)
  "Display NAME command documentation in a new focused buffer."
  (let* ((title (str:concat "*Help-" (symbol-name (sym command)) "*"))
         (help-buffer (next/help-mode:help-mode
                       :activate t
                       :buffer (make-buffer :title title)))
         (key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                         (command-function command)
                                         (all-keymaps))))
         (key-keymapname-pairs (mapcar (lambda (pair)
                                         (list (first pair)
                                               (keymap:name (second pair))))
                                       key-keymap-pairs))
         (source-file (getf (getf (swank:find-definition-for-thing (command-function command))
                                  :location)
                            :file))
         (help-contents (markup:markup
                         (:h1 (symbol-name (sym command)))
                         (:p (:pre ; See describe-slot* for why we use :pre.
                              ;; TODO: This only displays the first method,
                              ;; i.e. the first command of one of the modes.
                              ;; Ask for modes instead?
                              (documentation (command-function command) t)))
                         (:h2 "Bindings")
                         (:p (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
                         (:h2 (format nil "Source (~a): " source-file))
                         (:pre (:code (write-to-string (sexp command))))))
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
    (flet ((method-desc (method)
             (markup:markup
              (:h1 (symbol-name input) " " (write-to-string (mopu:method-specializers method)))
              (:pre (documentation method 't))
              (:h2 "Argument list")
              (:p (write-to-string (closer-mop:method-lambda-list method))))))
      (let* ((help-buffer (next/help-mode:help-mode
                           :activate t
                           :buffer (make-buffer
                                    :title (str:concat "*Help-"
                                                       (symbol-name input)
                                                       "*"))))
             (help-contents (if (typep (symbol-function input) 'generic-function)
                                (apply #'str:concat (mapcar #'method-desc (mopu:generic-function-methods (symbol-function input))))
                                (str:concat
                                 (markup:markup
                                  (:h1 (symbol-name input) (when (macro-function input) " (macro)"))
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
         (let* ((initform-string (write-to-string (getf props :initform)))
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
    (let* ((help-buffer (next/help-mode:help-mode
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
                            (:p (documentation input 'type))
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
    (let* ((help-buffer (next/help-mode:help-mode
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
         (help-buffer (next/help-mode:help-mode
                       :activate t
                       :buffer (make-buffer :title title)))
         (help-contents
                        (markup:markup
                         (:h1 "Bindings")
                         (:p
                          (loop for keymap in (current-keymaps)
                                collect (markup:markup
                                         (:p (keymap:name keymap))
                                         (:table
                                          (loop for keyspec being the hash-keys in (keymap:keymap-with-parents->map keymap)
                                                  using (hash-value bound-value)
                                                collect (markup:markup
                                                         (:tr
                                                          (:td keyspec)
                                                          (:td (string-downcase (sym (function-command bound-value)))))))))))))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ffi-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

(defun describe-key-dispatch-input (event buffer window printable-p)
  "Display bound value documentation.
Cancel with 'escape escape'.
Input is not forwarded.
This function can be used as a `window' `input-dispatcher'."
  (declare (ignore event buffer printable-p))
  (handler-case
      (progn
        (with-accessors ((key-stack key-stack)) *browser*
          (log:debug "Intercepted key ~a" (first (last key-stack)))
          (let ((escape-key (keymap:make-key :value "escape"))
                (bound-value (keymap:lookup-key key-stack (current-keymaps))))
            (cond
              ((and bound-value (not (keymap:keymap-p bound-value)))
               ;; TODO: Highlight hit bindings and display translation if any.
               ;; For this, we probably need to call `lookup-key' on key-stack.
               (describe-command* (function-command bound-value))
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
      (declare (ignore c))
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

(define-command command-evaluate ()
  "Evaluate a form."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Evaluate Lisp")))
    (let* ((result-buffer (next/help-mode:help-mode
                           :activate t
                           :buffer (make-buffer
                                    ;; TODO: Reuse buffer / create REPL mode.
                                    :title "*List Evaluation*")))
           (results (handler-case
                        (mapcar #'write-to-string (evaluate input))
                      (error (c) (list (format nil "Error: ~a" c)))))
           (result-contents (apply #'concatenate 'string
                                   (markup:markup
                                    (:h1 "Form")
                                    (:p input)
                                    (:h1 "Result"))
                                   (loop for result in results
                                         collect (markup:markup (:p result)))))
           (insert-results (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp result-contents)))))
      (ffi-buffer-evaluate-javascript result-buffer insert-results)
      (set-current-buffer result-buffer))))

(defun error-buffer (title text)
  "Print some help."
  (let* ((error-buffer (next/help-mode:help-mode :activate t
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

(define-command next-version ()
  "Version number of this version of Next.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(defclass messages-appender (log4cl-impl:appender)
  ())

(defmethod log4cl-impl:appender-do-append ((appender messages-appender) logger level log-func)
  (push
   `(:p ,(with-output-to-string (s)
           (log4cl-impl:layout-to-stream
            (slot-value appender 'log4cl-impl:layout) s logger level log-func)))
   (messages-content *browser*)))

(define-command messages ()
  "Show the *Messages* buffer."
  (let ((buffer (find-if (lambda (b)
                           (string= "*Messages*" (title b)))
                         (buffer-list))))
    (unless buffer
      (setf buffer (next/help-mode:help-mode :activate t
                                             :buffer (make-buffer :title "*Messages*"))))
    (let* ((content
             (apply #'markup:markup*
                    '(:h1 "Messages")
                    (reverse (messages-content *browser*))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))

(define-command clear-messages ()       ; TODO: Move to lod-mode?
  "Clear the *Messages* buffer."
  (setf (messages-content *browser*) '())
  (echo "Messages cleared."))

(defun binding-keys (fn)
  (let* ((buffer (current-buffer))
         (keymaps (cons (override-map buffer)
                        (delete nil (mapcar #'keymap (modes buffer))))))
    (or (first (keymap:binding-keys fn keymaps))
        "<NONE>")))

(define-command help ()
  "Print some help."
  (let ((help-buffer (next/help-mode:help-mode :activate t
                                               :buffer (make-buffer :title "*Help*"))))
    (set-current-buffer help-buffer)
    (let* ((help-contents
             (markup:markup
              (:h1 "Getting started")
              (:p (:b "Warning: ") "Next is under active development. Feel free to "
                  (:a :href "https://github.com/atlas-engineer/next/issues"
                      "report")
                  " bugs, instabilities or feature wishes.")
              (:p "You can help with Next development by supporting us in various ways:"
                  (:ul
                   (:li "Support continuous development on "
                        (:a :href "https://www.patreon.com/next_browser"
                            "Patreon")
                        ".")
                   (:li "Spread the word on social media and "
                        (:a :href "https://github.com/atlas-engineer/next"
                            "star the project on GitHub")
                        ".")))
              (:h2 "Quickstart keys")
              (:ul
               (:li (:code (binding-keys #'set-url)) ": Load URL")
               (:li (:code (binding-keys #'set-url-new-buffer)) ": Load URL in new tab")
               (:li (:code (binding-keys #'switch-buffer-previous)) ", " (:code (binding-keys #'switch-buffer-next)) ": Switch tab")
               (:li (:code (binding-keys #'next/web-mode:history-backwards)) ": Backwards history")
               (:li (:code (binding-keys #'next/web-mode:history-forwards)) ": Forwards history")
               (:li (:code (binding-keys #'follow-hint)) ": Follow link in current buffer")
               (:li (:code (binding-keys #'follow-hint-new-buffer)) ": Follow link in new buffer")
               (:li (:code (binding-keys #'quit)) ": Quit")
               (:li (:code (binding-keys #'execute-command)) ": Run a command by name")
               (:li (:code (binding-keys #'describe-bindings)) ": List all bindings for the current tab"))
              (:p "Legend:")
              (:ul
               (:li (:code "control") " (" (:code "C") "): Control key")
               (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
               (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
               (:li (:code "shift") " (" (:code "s") "): Shift key"))

              (:p "Next proposes several " (:i "binding schemes") ", for instance CUA, Emacs, VI."
                  " For instance, call the " (:code "vi-normal-mode") " command to switch to VI bindings."
                  " To enable it by default, see the command documentation with "
                  (:code (binding-keys #'execute-command) " describe-command") " (bound to "
                  (:code (binding-keys #'describe-command)) ").")

              (:h2 "Customize and extend Next")
              (:p "Customization is possible through the creation of a "
                  (:code "~/.config/next/init.lisp")
                  " file. From here you can override and redefine any of the functions by defining your init file as part of the "
                  (:code ":next")
                  " package. For more information please see: "
                  (:a :href "https://next.atlas.engineer/documentation#customization"
                      "customizing Next")
                  ".")
              (:h2 "Documentation")
              (:p "The " (:i "minibuffer") " lets you fuzzy-search all commands."
                  " Press " (:code (binding-keys #'execute-command))
                  " then type " (:code "describe") " to list all documentation-related commands."
                  " These commands can display the documentation of all Next components.")
              (:p "For full documentation about Next, how it works, and how to extend it please see the "
                  (:a :href "https://next.atlas.engineer/documentation"
                      "user manual")
                  ".")))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ffi-buffer-evaluate-javascript help-buffer insert-help))
    help-buffer))
