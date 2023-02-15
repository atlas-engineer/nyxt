;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class hook-description ()
  ((name ""
         :documentation "The hook name.")
   (value nil
          :type t
          :documentation "The hook value."))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun command-attributes (command &optional (buffer (active-buffer (current-window :no-rescan))))
  (let* ((bindings (keymaps:binding-keys
                    (name command)
                    (current-keymaps buffer))))
    `(("Name" ,(string-downcase (closer-mop:generic-function-name command)))
      ("Bindings" ,(format nil "~{~a~^, ~}" bindings))
      ("Docstring" ,(or (first (sera::lines (documentation command 'function)))
                        "") nil 4)
      ("Mode" ,(let ((package-name (str:downcase (uiop:symbol-package-name (closer-mop:generic-function-name command)))))
                 (if (sera:in package-name "nyxt" "nyxt-user")
                     ""
                     (str:replace-first "nyxt/" "" package-name)))))))

(define-class command-source (prompter:source)
  ((prompter:name "Commands")
   (global-p t
             :type boolean
             :documentation "Whether global commands are included in the suggestions.")
   (buffer (current-buffer)
           :type buffer)
   (prompter:constructor (lambda (source)
                           (sort-by-time
                            (list-commands
                             :global-p (global-p source)
                             :mode-symbols (mapcar #'sera:class-name-of (sera:filter #'enabled-p (modes (buffer source))))))))
   (prompter:active-attributes-keys '("Name" "Bindings" "Docstring")))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompter source to execute commands.
Global commands are listed if `global-p' is non-nil.
Mode commands of enabled modes are also listed.
While disabled-mode commands are not listed, it's still possible to call them
from a key binding.")
  (:metaclass user-class))

(defmethod predict-next-command ((browser browser))
  (alex:when-let ((prediction (analysis:predict (command-model browser)
                                                (list (last-command browser)))))
    (analysis:element prediction)))

(define-class predicted-command-source (prompter:source)
  ((prompter:name "Predicted Command")
   (prompter:constructor
    (lambda (source)
      (declare (ignore source))
      (list (predict-next-command *browser*)))))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompter source to predict commands.")
  (:metaclass user-class))

(define-command execute-predicted-command ()
  "Execute the predicted next command."
  (run-async (predict-next-command *browser*)))

(defmethod prompter:object-attributes ((command command) (source prompter:source))
  (declare (ignore source))
  (command-attributes command))

(define-class extended-command-source (prompter:source)
  ((prompter:name "Lisp expression")
   (prompter:filter-preprocessor
    (lambda (suggestions source input)
      (unless (uiop:emptyp input)
        (or
         (ignore-errors
          (let* ((proper-input (if (and (str:starts-with-p "(" input)
                                        (str:ends-with-p ")" input))
                                   input
                                   (str:concat "(" input ")")))
                 (expression (uiop:safe-read-from-string proper-input))
                 (symbol (symbol-name (first expression)))
                 (function (or (sera:and-let* ((suggestions suggestions)
                                               (prev (prompter:value (first suggestions)))
                                               (prev-symbol (first prev))
                                               (_ (equalp symbol (symbol-name prev-symbol))))
                                 prev-symbol)
                               (sym:resolve-symbol symbol :command (list-all-packages))
                               (sym:resolve-symbol symbol :function (list-all-packages)))))
            (when (fboundp function)
              (list (funcall (prompter:suggestion-maker source)
                             (cons function (rest expression))
                             source input)))))
         (ignore-errors
          (mapcar (lambda (s)
                    (funcall (prompter:suggestion-maker source) s source input))
                  (remove-if-not (alex:disjoin #'fboundp #'boundp)
                                 (mapcar (alex:rcurry #'uiop:safe-read-from-string
                                                      :package (find-package :nyxt))
                                         (first (swank:simple-completions input *package*))))))))))
   (buffer (current-buffer)
           :type buffer))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompter source to execute commands with arguments.
Includes all commands and modes, and adds arbitrary Lisp functions on top of that.")
  (:metaclass user-class))

(defmethod prompter:object-attributes ((extended-command list) (source extended-command-source))
  (declare (ignore source))
  (let ((function (symbol-function (first extended-command)))
        (*print-case* :downcase))
    `(("Expression" ,(format nil "~s" extended-command))
      ("Arguments" ,(remove #\newline (format nil "~{~a~^ ~}" (arglist function))))
      ("Documentation" ,(or (first (sera::lines (documentation function 'function)))
                            "") nil 4))))

(defmethod prompter:object-attributes ((extended-command symbol) (source extended-command-source))
  (declare (ignore source))
  (let ((*print-case* :downcase))
    (if (fboundp extended-command)
        (let ((function (symbol-function extended-command)))
          `(("Expression" ,(format nil "~s" extended-command))
            ("Arguments" ,(remove #\newline (format nil "~{~a~^ ~}" (arglist function))))
            ("Documentation" ,(or (first (sera::lines (documentation function 'function))) "") nil 4)))
        `(("Expression" ,(prini-to-string extended-command))
          ("Arguments" "")
          ("Documentation" ,(or (documentation extended-command 'variable) "") nil 4)))))

(define-command execute-command ()
  "Execute a command by name.

Also accepts arbitrary Lisp expressions (even without the outermost level of
parentheses), and lists the possible completions for the incomplete symbols,
together with the arglists and documentations of the functions typed in."
  (unless (active-prompt-buffers (current-window))
    (prompt
     :prompt "Execute command"
     :sources (list (make-instance
                     'command-source
                     :actions-on-return
                     (list (lambda-command run-command* (commands)
                             "Run the chosen command."
                             (let ((command (first commands)))
                               (setf (last-access command) (local-time:now))
                               (run-async command)))
                           (lambda-command describe-command* (commands)
                             "Show the documentation and other properties of this command."
                             (describe-command :command (name (first commands))))))
                    (make-instance
                     'extended-command-source
                     :actions-on-return
                     (lambda-command evaluate-lisp-expression* (exprs)
                       "Evaluate the inputted Lisp expression."
                       (run-thread "evaluator"
                         (let ((*interactive-p* t))
                           (echo "~s" (eval (first exprs)))))))
                    (make-instance
                     'predicted-command-source
                     :actions-on-return
                     (lambda-command run-command* (commands)
                       "Run the chosen command."
                       (alex:when-let ((command (first commands)))
                         (setf (last-access command) (time:now))
                         (run-async command)))))
     :hide-suggestion-count-p t)))

(defun parse-function-lambda-list-types (fn)
  #-sbcl
  (declare (ignore fn))
  #-sbcl
  (warn "Function type parsing is not supported on this Lisp implementation.")
  #+sbcl
  (let* ((types (second (sb-introspect:function-type fn)))
         (keywords '())
         (type-batches (sera:split-sequence-if (lambda (type)
                                                 (when (find type lambda-list-keywords)
                                                   (push type keywords)))
                                               types))
         (keyword-type-pairs (pairlis keywords (rest type-batches))))
    (alex:nreversef keywords)
    (values (first type-batches)
            (alex:assoc-value keyword-type-pairs '&optional)
            (alex:assoc-value keyword-type-pairs '&rest)
            (alex:assoc-value keyword-type-pairs '&key))))

(defun prompt-argument (prompt &optional type input)
  (let ((value
          (first
           (evaluate
            (prompt1
             :prompt (if type
                         (format nil "~a (~a)" prompt type)
                         prompt)
             :input (write-to-string input)
             :sources (make-instance 'prompter:raw-source
                                     :name "Evaluated input"))))))
    (if (or (not type)
            (typep value type))
        value
        (progn
          (echo "~s has type ~s, expected ~s."
                value (type-of value) type)
          (prompt-argument prompt type input)))))

(defun arglist (fn)
  "Like `swank-backend:arglist' but normalized the result for `alex:parse-ordinary-lambda-list'."
  #-ccl
  (swank-backend:arglist fn)
  #+ccl
  (let ((package (alex:if-let ((name (swank-backend:function-name fn)))
                   (symbol-package (if (listp name)
                                       ;; Closures are named '(:internal NAME)
                                       (second name)
                                       name))
                   *package*)))
    (delete 'ccl::&lexpr
            (mapcar (lambda (s)
                      (if (keywordp s) (intern (string s) package) s))
                    (swank-backend:arglist fn)))))

(define-command execute-extended-command (&optional command)
  "Prompt for arguments to pass to a given COMMAND.
User input is evaluated Lisp."
  ;; TODO: Add support for &rest arguments.
  (let* ((command (or command
                      (prompt1
                       :prompt "Execute extended command"
                       :sources 'command-source
                       :hide-suggestion-count-p t)))
         (lambda-list (arglist (slot-value command 'fn))))
    (multiple-value-match (alex:parse-ordinary-lambda-list lambda-list)
      ((required-arguments optional-arguments _ keyword-arguments)
       (multiple-value-match (parse-function-lambda-list-types (slot-value command 'fn))
         ((required-types optional-types _ keyword-types)
          (flet ((parse-args (params)
                   (mappend
                    (lambda-match
                      ((cons (and param (type symbol)) type)
                       (list (prompt-argument param type)))
                      ((cons (list (list keyword name) default _) type)
                       (list keyword
                             (prompt-argument name type default)))
                      ((cons (list name default _) type)
                       (list (prompt-argument name type default))))
                    params)))
            (setf (last-access command) (time:now))
            (run-async
             command
             (mappend #'parse-args
                      (list (pairlis required-arguments required-types)
                            (pairlis optional-arguments optional-types)
                            (pairlis keyword-arguments (mapcar #'second keyword-types))))))))))))

(defun get-hooks ()
  (flet ((list-hooks (object)
           (mapcar (lambda (hook)
                     (make-instance 'hook-description
                                    :name (str:downcase (closer-mop:slot-definition-name hook))
                                    :value (funcall (symbol-function (closer-mop:slot-definition-name hook))
                                                    object)))
                   (remove-if-not (lambda (s)
                                    (let ((name (closer-mop:slot-definition-name s)))
                                      (and (str:ends-with-p "-hook" (string name) :ignore-case t)
                                           (fboundp name))))
                                  (closer-mop:class-slots (class-of object))))))
    (let ((window-hooks (list-hooks (current-window)))
          (buffer-hooks (list-hooks (current-buffer)))
          (browser-hooks (list-hooks *browser*)))
      (append window-hooks
              buffer-hooks
              browser-hooks))))

(define-class hook-source (prompter:source)
  ((prompter:name "Hooks")
   (prompter:constructor (get-hooks))))

(defmethod prompter:object-attributes ((hook-description hook-description) (source hook-source))
  (declare (ignore source))
  `(("Name" ,(name hook-description))
    ("Value" ,(value hook-description))))

(define-class handler-source (prompter:source)
  ((prompter:name "Handlers")
   (hook :accessor hook
         :initarg :hook
         :documentation "The hook for which to retrieve handlers for.")
   (prompter:constructor (lambda (source)
                           (hooks:handlers (hook source))))))

(defmethod prompter:object-attributes ((handler hooks:handler) (source handler-source))
  (declare (ignore source))
  `(("Name" ,(str:downcase (hooks:name handler)))))

(define-class disabled-handler-source (handler-source)
  ((prompter:constructor (lambda (source)
                           (hooks:disabled-handlers (hook source))))))

(define-command-global disable-hook-handler ()
  "Remove handler(s) from a hook."
  (let* ((hook-desc (prompt1
                     :prompt "Hook where to disable handler"
                     :sources 'hook-source))
         (handler (prompt1
                   :prompt (format nil "Disable handler from ~a" (name hook-desc))
                   :sources (make-instance 'handler-source
                                           :hook (value hook-desc)))))
    (hooks:disable-hook (value hook-desc) handler)))

(define-command-global enable-hook-handler ()
  "Enable handler(s) from a hook."
  (let* ((hook-desc (prompt1
                     :prompt "Hook where to enable handler"
                     :sources 'hook-source))
         (handler (prompt1
                   :prompt (format nil "Enable handler from ~a" (name hook-desc))
                   :sources (make-instance 'disabled-handler-source
                                           :hook (value hook-desc)))))
    (hooks:enable-hook (value hook-desc) handler)))
