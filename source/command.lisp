;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *command-list* '()
  "The list of known commands, for internal use only.")

(define-class command (standard-generic-function)
  ((visibility
    :mode
    :type (member :global :mode :anonymous)
    :reader t
    :writer nil
    :documentation "Sets whether command is listed in `command-source'.

- `:global' always lists it.  This is mostly useful for third-party packages to
define globally-accessible commands without polluting the official Nyxt
packages.

- `:mode' lists it when the corresponding mode is active.

- `:anonymous' never lists it.")
   (deprecated-p
    nil
    :type boolean
    :reader t
    :writer nil
    :documentation "Whether a warning before executing a deprecated command is
signalled.")
   (last-access
    (local-time:now)
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

(defun initialize-command (command lambda-expression)
  (when (uiop:emptyp (closer-mop:generic-function-name command))
    (alex:required-argument 'name))
  (when lambda-expression
    ;; `closer-mop:ensure-method' calls `add-method' which reinitializes the
    ;; command / generic function, thus running `initialize-command' twice each
    ;; time.  TODO: Can we avoid this?
    (closer-mop:ensure-method command lambda-expression)
    (when (uiop:emptyp (documentation command t))
      (let ((doc (nth-value 2 (alex:parse-body (rest (rest lambda-expression)) :documentation t))))
        (if (and (uiop:emptyp doc)
                 (not (eq :anonymous (visibility command))))
            (error "Command ~a requires documentation." (name command))
            (setf (documentation command 'function) doc)))))
  (unless (or (eq :anonymous (visibility command))
              (deprecated-p command))
    ;; Overwrite previous command:
    (setf *command-list* (delete (closer-mop:generic-function-name command) *command-list*
                                 :key #'closer-mop:generic-function-name))
    (push command *command-list*)))

(defmethod initialize-instance :after ((command command) &key lambda-expression &allow-other-keys)
  (initialize-command command lambda-expression))
(defmethod reinitialize-instance :after ((command command) &key lambda-expression &allow-other-keys)
  (initialize-command command lambda-expression))

(defun find-command (name)
  (find name *command-list* :key #'name))

;; TODO: Can we use `alex:named-lambda'?  How do we get the name then?
(export-always 'make-command)
(defun make-command (name lambda-expression &optional (visibility :anonymous))
  "Return an non-globally defined command named NAME."
  (make-instance 'command :name name
                          :lambda-expression lambda-expression
                          :visibility visibility))

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
       ;; Warning: We use `defgeneric' instead of `make-instance' (or even
       ;; `ensure-generic-function') so that the compiler stores source location
       ;; information (for "go to definition" to work.
       (sera:lret ((gf (defgeneric ,name (,@(generalize-lambda-list arglist))
                         (:documentation ,doc)
                         (:method (,@arglist) ,@body)
                         (:generic-function-class command))))
         (setf (slot-value gf 'visibility) :mode)))))

(export-always 'define-command-global)
(defmacro define-command-global (name (&rest arglist) &body body)
  "Like `define-command' but mark the command as global.
This means it will be listed in `command-source' when the global option is on.
This is mostly useful for third-party packages to define globally-accessible
commands without polluting Nyxt packages."
  `(sera:lret ((cmd (define-command ,name (,@arglist) ,@body)))
     (setf (slot-value cmd 'visibility) :global)))

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

(-> list-all-maybe-subpackages () (list-of package))
(defun list-all-maybe-subpackages ()
  (sera:filter (lambda (pkg) (find #\/ (package-name pkg)))
               (list-all-packages)))

(-> subpackages (trivial-types:package-designator) (list-of package))
(defun subpackages (package)
  (sera:filter (lambda (p) (subpackage-p p package))
               (list-all-maybe-subpackages)))

(-> subpackage-p (trivial-types:package-designator trivial-types:package-designator) boolean)
(defun subpackage-p (subpackage package)
  "Return non-nil if SUBPACKAGE is a subpackage of PACKAGE or is PACKAGE itself.
A subpackage has a name that starts with that of PACKAGE followed by a '/' separator."
  (or (eq (find-package subpackage) (find-package package))
      (sera:string-prefix-p (uiop:strcat (package-name package) "/")
                            (package-name subpackage))))

(-> nyxt-subpackage-p (trivial-types:package-designator) boolean)
(defun nyxt-subpackage-p (package)
  "Return non-nil if PACKAGE is a sub-package of `nyxt'."
  (subpackage-p package :nyxt))

(-> nyxt-user-subpackage-p (trivial-types:package-designator) boolean)
(defun nyxt-user-subpackage-p (package)
  "Return non-nil if PACKAGE is a sub-package of `nyxt' or `nyxt-user'."
  (subpackage-p package :nyxt-user))

(defvar *nyxt-extra-packages* (mapcar #'find-package
                                      '(class-star
                                        download-manager
                                        history-tree
                                        password
                                        analysis
                                        text-buffer))
  "Packages to append to the result of `nyxt-packages'.")

(defun nyxt-packages ()
  "Return the Nyxt package, all its subpackages plus what's in `*nyxt-extra-packages*'.
See also `nyxt-user-packages'."
  (cons
   (find-package :nyxt)
   (sera:filter #'nyxt-subpackage-p
                (list-all-packages))))

(defun nyxt-user-packages ()
  "Return the Nyxt package, the `:nyxt-user', all their subpackages plus what's
in `*nyxt-extra-packages*'.
See also `nyxt-packages'."
  (cons
   (find-package :nyxt-user)
   (sera:filter #'nyxt-user-subpackage-p
                (list-all-packages))))

(defun package-symbols (&optional (packages (nyxt-packages))
                          (user-packages (nyxt-user-packages)))
  "Return the list of all external symbols from PACKAGES
and all (possibly unexported) symbols from USER-PACKAGES.

If PACKAGES is NIL, return all external symbols in all packages."
  (let* ((user-packages (mapcar #'find-package (alex:ensure-list user-packages)))
         (packages (or (alex:ensure-list packages)
                       (set-difference
                        (list-all-packages)
                        user-packages)))
         (symbols))
    (dolist (package (mapcar #'find-package packages))
      (do-external-symbols (s package symbols)
        (push s symbols)))
    (dolist (package user-packages)
      (do-symbols (s package symbols)
        (when (eq (symbol-package s) package)
          (push s symbols))))
    (delete-duplicates symbols)))

(defun package-variables (&optional (packages (nyxt-packages))
                            (user-packages (nyxt-user-packages)))
  "Return the list of variable symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (delete-if (complement #'boundp) (package-symbols packages user-packages)))

(defun package-functions (&optional (packages (nyxt-packages))
                            (user-packages (nyxt-user-packages)))
  "Return the list of function symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (delete-if (complement #'fboundp) (package-symbols packages user-packages)))

(defun package-classes (&optional (packages (nyxt-packages))
                          (user-packages (nyxt-user-packages)))
  "Return the list of class symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (delete-if (lambda (sym)
               (not (and (find-class sym nil)
                         ;; Discard non-standard objects such as structures or
                         ;; conditions because they don't have public slots.
                         (mopu:subclassp (find-class sym) (find-class 'standard-object)))))
             (package-symbols packages user-packages)))

(define-class slot ()
  ((name nil
         :type (or symbol null))
   (class-sym nil
              :type (or symbol null)))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun exported-p (sym)
  (eq :external
      (nth-value 1 (find-symbol (string sym)
                                (symbol-package sym)))))

(defun class-public-slots (class-sym)
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names class-sym)))

(defmethod prompter:object-attributes ((slot slot) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(string (name slot)))
    ("Class" ,(string (class-sym slot)))))

(defun package-slots (&optional (packages (nyxt-packages))
                        (user-packages (nyxt-user-packages)))
  "Return the list of all slot symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (mappend (lambda (class-sym)
                  (mapcar (lambda (slot) (make-instance 'slot
                                                        :name slot
                                                        :class-sym class-sym))
                          (class-public-slots class-sym)))
                (package-classes packages user-packages)))

(defun package-methods (&optional (packages (nyxt-packages)) ; TODO: Unused.  Remove?
                          (user-packages (nyxt-user-packages)))
  "Return the list of all method symbols in PACKAGES and USER-PACKAGES.
See `package-symbols' for details on the arguments."
  (loop for sym in (package-symbols packages user-packages)
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
                 (or (eq (symbol-package (name command))
                         (symbol-package mode-symbol))
                     (member
                      (symbol-package (name command))
                      (mapcar #'symbol-package
                              (sera:filter (lambda (s) (typep s 'mode-symbol))
                                           (mapcar #'class-name (mopu:superclasses mode-symbol)))))))
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
        (prompt-buffer-canceled ()
          (log:debug "Prompt buffer interrupted")
          nil)))))

(defun run (command &optional args)
  "Run COMMAND over ARGS and return its result.
This is blocking, see `run-async' for an asynchronous way to run commands."
  (let ((channel (make-channel 1))
        (error-channel (make-channel 1)))
    (run-thread "run command"
      ;; TODO: This `handler-case' overlaps with `with-protect' from `run-thread'.  Factor them!
      (handler-case (calispel:! channel (run-command command args))
        (condition (c)
          (calispel:! error-channel c))))
    (calispel:fair-alt
      ((calispel:? channel result)
       result)
      ((calispel:? error-channel c)
       (echo-warning "Error when running ~a: ~a" command c)))))

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
