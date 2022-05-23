;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *command-list* '()
  "The list of known commands, for internal use only.")

(define-class command (standard-generic-function)
  ((visibility :mode
               :type (member :global :mode :anonymous)
               :reader t
               :writer nil
               :documentation "
- `:global'  means it will be listed in `command-source' when the global option is on.
This is mostly useful for third-party packages to define globally-accessible
commands without polluting the official Nyxt packages.

- `:mode' means the command is only listed in `command-source' when the
corresponding mode is active.

- `:anonymous' is for local definitions; the command is never listed in
`command-source'. ")
   (deprecated-p nil
                 :type boolean
                 :reader t
                 :writer nil
                 :documentation "Mark the command as superseded by something else.
If non-nil, report a warning before executing the command.")
   (last-access (local-time:now)
                :type local-time:timestamp
                :documentation "Last time this command was called from prompt buffer.
Useful to sort the commands by most recent use."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Commands are interactive functions.
(As in Emacs.)

Commands are funcallable.

We need a `command' class for multiple reasons:
- Identify commands uniquely.

- Customize prompt buffer display value with properties.

- Last access: This is useful to sort command by the time they were last
  called.  The only way to do this is to persist the command instances.

Since they are generic functions, they can be specialize with `:before',
`:after' and `:around' qualifiers, effectively acting as hooks.
These specializations are reserved to the user."))

(defmethod name ((command command))
  "A useful shortcut."
  (closer-mop:generic-function-name command))

(defmethod closer-mop:compute-effective-method :around ((command command) combin applicable)
  (declare (ignorable applicable combin))
  ;; TODO: Should `define-deprecated-command' report the version
  ;; number of deprecation?  Maybe OK to just remove all deprecated
  ;; commands on major releases.
  `(echo-warning "~a is deprecated." ,(name command))
  (call-next-method))

(defmethod initialize-instance :after ((command command) &key)
  (when (and (uiop:emptyp (documentation command 'function))
             (not (eq :anonymous (visibility command))))
    (error "Commands require documentation."))
  (unless (or (eq :anonymous (visibility command))
              (deprecated-p command))
    ;; Overwrite previous command:
    (setf *command-list* (delete (closer-mop:generic-function-name command) *command-list* :key #'closer-mop:generic-function-name))
    (push command *command-list*)))

(defun find-command (name)
  (find name *command-list* :key #'name))

;; TODO: Can we use `alex:named-lambda'?  How do we get the name then?
(export-always 'make-command)
(defun make-command (name lambda-expression &optional (visibility :anonymous))
  "Return an non-globally defined command named NAME."
  (let ((arglist (second lambda-expression))
        (doc (nth-value 2 (alex:parse-body (rest (rest lambda-expression)) :documentation t))))
    (sera:lret ((command (make-instance
                          'command
                          :name name
                          :lambda-list (generalize-lambda-list arglist)
                          :documentation doc
                          :visibility visibility)))
      (closer-mop:ensure-method command lambda-expression))))

(export-always 'lambda-command)
(defmacro lambda-command (name args &body body)
  "ARGS may only be a list of required arguments (optional and keyword argument
not allowed).

Example:

\(let ((source (make-my-source)))
  (lambda-command open-file* (files)
    \"Open files in some way.\"
    ;; Note that `source' is captured in the closure.
    (mapc (opener source) files)))"
  (alex:with-gensyms (closed-over-body)
    ;; Warning: `make-command' takes a lambda-expression as an unevaluated list,
    ;; thus the BODY environment is not that of the lexical environment
    ;; (closures would thus fail to close over).  To avoid this problem, we capture
    ;; the lexical environment in a lambda.
    ;;
    ;; Note that this relies on the assumption that ARGS is just a list of
    ;; _required arguments_, which is a same assumption for prompt buffer actions.
    ;; We could remove this limitation with some argument parsing.
    `(let ((,closed-over-body (lambda ,args ,@body)))
       (make-command ',name
                     (list 'lambda ',args (list 'apply ,closed-over-body  '(list ,@args)))))))

(export-always 'lambda-mapped-command)
(defmacro lambda-mapped-command (function-symbol)
  "Define a command which `mapcar's FUNCTION-SYMBOL over a list of arguments."
  (let ((name (intern (str:concat (string function-symbol) "-*"))))
    `(lambda-command ,name (arg-list)
       ,(documentation function-symbol 'function)
       (mapcar ',function-symbol arg-list))))

(export-always 'lambda-unmapped-command)
(defmacro lambda-unmapped-command (function-symbol)
  "Define a command which calls FUNCTION-SYMBOL over the first element of a list
of arguments."
  (let ((name (intern (str:concat (string function-symbol) "-1"))))
    `(lambda-command ,name (arg-list)
       ,(documentation function-symbol 'function)
       (,function-symbol (first arg-list)))))

(sera:eval-always
  (defun generalize-lambda-list (lambda-list)
    "Return a lambda-list compatible with generic-function definitions.
Generic function lambda lists differ from ordinary lambda list in some ways;
see HyperSpec '3.4.2 Generic Function Lambda Lists'."
    (multiple-value-bind (required optional rest keywords aok? aux key?)
        (alex:parse-ordinary-lambda-list lambda-list)
      (declare (ignore aux))
      (sera:unparse-ordinary-lambda-list required (mapcar #'first optional) rest (mapcar #'cadar keywords) aok? nil key?))))

(export-always 'define-command)
(defmacro define-command (name (&rest arglist) &body body)
  "Define new command NAME.
`define-command' syntax is similar to `defmethod'.

Example:

\(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  \"Play video in the currently open buffer.\"
  (uiop:run-program (list \"mpv\" (render-url (url buffer)))))"
  (let ((doc (or (nth-value 2 (alex:parse-body body :documentation t)) "")))
    `(progn
       (export-always ',name (symbol-package ',name))
       (prog1 (defgeneric ,name (,@(generalize-lambda-list arglist))
                (:documentation ,doc)
                (:method (,@arglist) ,@body)
                (:generic-function-class command))
         (setf (slot-value #',name 'visibility) :mode)))))

(export-always 'define-command-global)
(defmacro define-command-global (name (&rest arglist) &body body)
  "Like `define-command' but mark the command as global.
This means it will be listed in `command-source' when the global option is on.
This is mostly useful for third-party packages to define globally-accessible
commands without polluting Nyxt packages."
  `(prog1 (define-command ,name (,@arglist) ,@body)
     (setf (slot-value #',name 'visibility) :global)))

(export-always 'delete-command)
(defun delete-command (name)
  "Remove command NAME, if any.
Any function or macro definition of NAME is also removed,
regardless of whether NAME is defined as a command."
  (setf *command-list* (delete name *command-list* :key #'name))
  (fmakunbound name))

(defmacro define-deprecated-command (name (&rest arglist) &body body) ; TODO: Do we even need this?
  "Define NAME, a deprecated command.
This is just like a command.  It's recommended to explain why the function is
deprecated and by what in the docstring."
  `(prog1 (define-command ,name (,@arglist) ,@body)
     (setf (slot-value #',name 'visibility) :mode
           (slot-value #'name 'deprecated-p) t)))

(defun nyxt-packages ()                 ; TODO: Export a customizable *nyxt-packages* instead?
  "Return all package designators that start with 'nyxt' plus Nyxt own libraries."
  (mapcar #'package-name
          (append (delete-if
                   (lambda (p)
                     (not (str:starts-with-p "NYXT" (package-name p))))
                   (list-all-packages))
                  (mapcar #'find-package
                          '(class-star
                            download-manager
                            history-tree
                            keymap
                            scheme
                            password
                            analysis
                            text-buffer)))))

(defun package-defined-symbols (&optional (external-package-designators (nyxt-packages))
                                  (user-package-designators '(:nyxt-user)))
  "Return the list of all external symbols interned in EXTERNAL-PACKAGE-DESIGNATORS
and all (possibly unexported) symbols in USER-PACKAGE-DESIGNATORS."
  (let ((external-package-designators
          ;; This is for the case external-package-designators are passed nil.
          (or external-package-designators (nyxt-packages)))
        (symbols))
    (dolist (package (mapcar #'find-package external-package-designators))
      (do-external-symbols (s package symbols)
        (pushnew s symbols)))
    (dolist (package (mapcar #'find-package user-package-designators))
      (do-symbols (s package symbols)
        (when (eq (symbol-package s) package)
          (pushnew s symbols))))
    symbols))

(defun package-variables (&optional packages)
  "Return the list of variable symbols in Nyxt-related-packages."
  (delete-if (complement #'boundp) (package-defined-symbols packages)))

(defun package-functions (&optional packages)
  "Return the list of function symbols in Nyxt-related packages."
  (delete-if (complement #'fboundp) (package-defined-symbols packages)))

(defun package-classes (&optional packages)
  "Return the list of class symbols in Nyxt-related-packages."
  (delete-if (lambda (sym)
               (not (and (find-class sym nil)
                         ;; Discard non-standard objects such as structures or
                         ;; conditions because they don't have public slots.
                         (mopu:subclassp (find-class sym) (find-class 'standard-object)))))
             (package-defined-symbols packages)))

(define-class slot ()
  ((name nil
         :type (or symbol null))
   (class-sym nil
              :type (or symbol null)))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((slot slot))
  `(("Name" ,(string (name slot)))
    ("Class" ,(string (class-sym slot)))))

(defun exported-p (sym)
  (eq :external
      (nth-value 1 (find-symbol (string sym)
                                (symbol-package sym)))))

(defun class-public-slots (class-sym)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names class-sym)))

(defun package-slots (&optional packages)
  "Return the list of all slot symbols in `:nyxt' and `:nyxt-user' or other PACKAGES."
  (alex:mappend (lambda (class-sym)
                  (mapcar (lambda (slot) (make-instance 'slot
                                                        :name slot
                                                        :class-sym class-sym))
                          (class-public-slots class-sym)))
                (package-classes packages)))

(defun package-methods (&optional packages) ; TODO: Unused.  Remove?
  (loop for sym in (package-defined-symbols packages)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

(defun list-commands (&key global-p mode-symbols)
  "List commands.
Commands are instances of the `command' class.
When MODE-SYMBOLS are provided, list only the commands that belong to the
corresponding mode packages or of a parent mode packages.
Otherwise list all commands.
With MODE-SYMBOLS and GLOBAL-P, include global commands."
  ;; TODO: Make sure we list commands of inherited modes.
  (if mode-symbols
      (lpara:premove-if
       (lambda (command)
         (and (or (not global-p)
                  (not (eq :global (visibility command))))
              (notany
               (lambda (mode-symbol)
                 (eq (symbol-package (name command))
                     (symbol-package mode-symbol)))
               mode-symbols)))
       *command-list*)
      *command-list*))

(defun run-command (command &optional args)
  ;; Bind current buffer for the duration of the command.  This
  ;; way, if the user switches buffer after running a command
  ;; but before command termination, `current-buffer' will
  ;; return the buffer from which the command was invoked.
  (with-current-buffer (current-buffer)
    (let ((*interactive-p* t))
      (handler-case (apply #'funcall command args)
        (nyxt-prompt-buffer-canceled ()
          (log:debug "Prompt buffer interrupted")
          nil)))))

(defun run (command &optional args)
  "Run COMMAND over ARGS and return its result.
This is blocking, see `run-async' for an asynchronous way to run commands."
  (let ((channel (make-channel 1)))
    (run-thread "run command"
      (calispel:! channel (run-command command args)))
    (calispel:? channel)))

(defun run-async (command &optional args)
  "Run COMMAND over ARGS asynchronously.
See `run' for a way to run commands in a synchronous fashion and return the
result."
  (run-thread "run-async command"
    (run-command command args)))

(define-command forward-to-renderer (&key (window (current-window))
                                     (buffer (current-buffer)))
  "A command that forwards the last key press to the renderer.
This is useful to override bindings to be forwarded to the renderer."
  (ffi-generate-input-event window (last-event buffer)))

(define-command nothing ()                 ; TODO: Replace with ESCAPE special command that allows dispatched to cancel current key stack.
  "A command that does nothing.
This is useful to override bindings to do nothing."
  (values))
