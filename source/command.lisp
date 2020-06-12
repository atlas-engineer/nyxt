(in-package :next)

;; We need a `command' class for multiple reasons:
;; - Identify commands uniquely (although being a member of `*command-list*' is enough).
;;
;; - Customize minibuffer display value with `object-string'.
;;
;; - Access-time: This is useful to sort command by the time they were last
;;   called.  The only way to do this is to persist the command instances.
(defclass command ()
  ((sym :accessor sym :initarg :sym :type symbol :initform nil) ; TODO: Make constructor?
   (pkg :accessor pkg :initarg :pkg :type package :initform nil)
   (sexp :accessor sexp :initarg :sexp :type sexp :initform nil)
   (access-time :accessor access-time :type integer :initform 0
                :documentation "Last time this command was called from minibuffer.
This can be used to order the commands.")))

(define-condition documentation-style-warning (style-warning)
  ((name :initarg :name :reader name)
   (subject-type :initarg :subject-type :reader subject-type))
  (:report
   (lambda (condition stream)
     (format stream
             "~:(~A~) ~A doesn't have a documentation string"
             (subject-type condition)
             (name condition)))))

(define-condition command-documentation-style-warning
    (documentation-style-warning)
  ((subject-type :initform 'command)))

(export-always 'define-command)
(defmacro define-command (name (&rest arglist) &body body)
  "Define new command NAME.
`define-command' has a syntax similar to `defun'.
ARGLIST must be a list of optional arguments or key arguments.
This macro also defines two hooks, NAME-before-hook and NAME-after-hook.
When run, the command always returns the last expression of BODY.

Example:

\(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  \"Play video in the currently open buffer.\"
  (uiop:run-program (list \"mpv\" (url buffer))))"
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body))
        (before-hook (intern (str:concat (symbol-name name) "-BEFORE-HOOK")))
        (after-hook (intern (str:concat (symbol-name name) "-AFTER-HOOK"))))
    `(progn
       (export-always ',before-hook)
       (defparameter ,before-hook (hooks:make-hook-void))
       (export-always ',after-hook)
       (defparameter ,after-hook (hooks:make-hook-void))
       (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                         (eq (pkg c) *package*)))
                        *command-list*)
         (push (make-instance 'command
                              :sym ',name
                              :pkg *package*
                              :sexp '(define-command (,@arglist) ,@body))
               *command-list*))
       (export-always ',name)
       ;; We use defun to define the command instead of storing a lambda because we want
       ;; to be able to call the foo command from Lisp with (FOO ...).
       (defun ,name ,arglist
         ,documentation
         (handler-case
             (progn
               (hooks:run-hook ,before-hook)
               (log:debug "Calling command ~a." ',name)
               ;; TODO: How can we print the arglist as well?
               ;; (log:debug "Calling command (~a ~a)." ',name (list ,@arglist))
               (prog1
                   (progn
                     ,@body)
                 (hooks:run-hook ,after-hook)))
           (next-condition (c)
             (format t "~s" c)))))))

;; TODO: Update define-deprecated-command
(defmacro define-deprecated-command (name (&rest arglist) &body body)
  "Define NAME, a deprecated command.
This is just like a command.  It's recommended to explain why the function is
deprecated and by what in the docstring."
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body)))
    `(progn
       (define-command ,name ,arglist
         ,documentation
         (progn
           ;; TODO: Implement `warn'.
           (echo-warning "~a is deprecated." ',name)
           ,@body)))))

(defun next-packages ()                 ; TODO: Export a customizable *next-packages* instead?
  "Return all package designators that start with 'next' plus Next own libraries."
  (mapcar #'package-name
          (append (delete-if
                   (lambda (p)
                     (not (str:starts-with-p "NEXT" (package-name p))))
                   (list-all-packages))
                  (mapcar #'find-package
                          '(download-manager
                            history-tree
                            hooks
                            keymap
                            scheme
                            password
                            text-analysis)))))

(defun package-defined-symbols (&optional (external-package-designators (next-packages))
                                  (user-package-designators '(:next-user)))
  "Return the list of all external symbols interned in EXTERNAL-PACKAGE-DESIGNATORS
and all (possibly unexported) symbols in USER-PACKAGE-DESIGNATORS."
  (let ((symbols))
    (dolist (package (mapcar #'find-package external-package-designators))
      (do-external-symbols (s package symbols)
        (pushnew s symbols)))
    (dolist (package (mapcar #'find-package user-package-designators))
      (do-symbols (s package symbols)
        (when (eq (symbol-package s) package)
          (pushnew s symbols))))
    symbols))

(defun package-variables ()
  "Return the list of variable symbols in Next-related-packages."
  (delete-if (complement #'boundp) (package-defined-symbols)))

(defun package-functions ()
  "Return the list of function symbols in Next-related packages."
  (delete-if (complement #'fboundp) (package-defined-symbols)))

(defun package-classes ()
  "Return the list of class symbols in Next-related-packages."
  (delete-if (complement (alex:rcurry #'find-class nil)) (package-defined-symbols)))

(defclass slot ()
  ((name :initarg :name
         :accessor name
         :type symbol)
   (class-sym :initarg :class-sym
              :accessor class-sym
              :type symbol)))

(defmethod object-string ((slot slot))
  (string-downcase (write-to-string (name slot))))

(defmethod object-display ((slot slot))
  (string-downcase (format nil "~s (~s)"
                           (name slot)
                           (class-sym slot))))

(defun exported-p (sym)
  (eq :external
      (nth-value 1 (find-symbol (string sym)
                                (symbol-package sym)))))

(defun class-public-slots (class-sym)
  "Return list of slot symbols that have at least a reader or a writer."
  (delete-if
   (lambda (slot-name)
     (let ((props (mopu:slot-properties (find-class class-sym) slot-name)))
       (or (and (not (getf props :writers))
                (not (getf props :readers)))
           (not (exported-p slot-name)))))
   (mopu:slot-names class-sym)))

(defun package-slots ()
  "Return the list of all slot symbols in `:next' and `:next-user'."
  (alex:mappend (lambda (class-sym)
                  (mapcar (lambda (slot) (make-instance 'slot
                                                        :name slot
                                                        :class-sym class-sym))
                          (class-public-slots class-sym)))
                (package-classes)))

(defun package-methods ()               ; TODO: Unused.  Remove?
  (loop for sym in (package-defined-symbols)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

(defmethod mode-toggler-p ((command command))
  "Return non-nil if COMMAND is a mode toggler.
A mode toggler is a command of the same name as its associated mode."
  (ignore-errors
   (closer-mop:subclassp (find-class (sym command) nil)
                         (find-class 'root-mode))))

(defun list-commands (&rest mode-symbols)
  "List commands.
Commands are instances of the `command' class.  When MODE-SYMBOLS are provided,
list only the commands that belong to the corresponding mode packages or of a
parent mode packages.  Otherwise list all commands.

If 'BASE-MODE is in MODE-SYMBOLS, mode togglers and commands from the
`next-user' package are included.  This is useful since mode togglers are
usually part of their own mode / package and would not be listed otherwise.
For `next-user' commands, users expect them to be listed out of the box without
extra fiddling."
  ;; TODO: Make sure we list commands of inherited modes.
  (let ((list-togglers-p (member 'base-mode mode-symbols)))
    (if mode-symbols
        (remove-if (lambda (c)
                     (and (or (not list-togglers-p)
                              (and (not (mode-toggler-p c))
                                   (not (eq (find-package 'next-user)
                                            (symbol-package (sym c))))))
                          (notany (lambda (m)
                                    (eq (pkg c)
                                        (match m
                                          ;; root-mode does not have a mode-command.
                                          ('root-mode nil)
                                          (_ (match (mode-command m)
                                               (nil nil)
                                               (mc (pkg mc)))))))
                                  mode-symbols)))
                   *command-list*)
        *command-list*)))

(defmethod object-string ((command command))
  (str:downcase (sym command)))

(defmethod object-display ((command command))
  (command-display command))

(defun command-completion-filter (&optional mode-symbols)
  (let* ((commands
           (sort (apply #'list-commands mode-symbols)
                 (lambda (c1 c2)
                   (> (access-time c1) (access-time c2)))))
         (pretty-commands (mapcar #'command-display commands)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) commands :candidates-display pretty-commands))))

(defmethod command-function ((command command))
  "Return the function associated to COMMAND.
This function can be `funcall'ed."
  (symbol-function (find-symbol
                    (string (sym command))
                    (pkg command))))

(defun command-display (command)
  ;; Use `(current-window :no-rescan)' or else the minibuffer will stutter
  ;; because of the RPC calls.
  (let* ((buffer (active-buffer (current-window :no-rescan)))
         (scheme-name (keymap-scheme-name buffer))
         (bindings '()))
    (loop for mode in (modes buffer)
          for scheme-keymap = (keymap:get-keymap scheme-name (keymap-scheme mode))
          when scheme-keymap
            do (setf bindings (keymap:binding-keys (sym command) scheme-keymap))
          when (not (null bindings))
            return bindings)
    (format nil "~a~a~a"
            (str:downcase (sym command))
            (if bindings
                (format nil " (~{~a~^, ~})" bindings)
                "")
            (match (object-string (pkg command))
              ((or "" "next" "next-user") "")
              (a (format nil " [~a]" a))))))

(declaim (ftype (function (function) (or null command)) function-command))
(defun function-command (function)
  "Return the command associated to FUNCTION, if any."
  (find-if (lambda (cmd)
             (eq function (command-function cmd)))
           (list-commands)))


(defmethod run ((command command) &rest args)
  "Run COMMAND over ARGS."
  (apply #'funcall-safely (command-function command) args))

(define-command execute-command ()
  "Execute a command by name."
  (unless (active-minibuffers (current-window))
    (with-result (command (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Execute command"
                            :completion-function (command-completion-filter
                                                  (mapcar (alex:compose #'class-name #'class-of)
                                                          (modes (current-buffer))))
                            :show-completion-count-p nil)))
      (setf (access-time command) (get-internal-real-time))
      (run command))))

(define-command noop ()                 ; TODO: Replace with ESCAPE special command that allows dispatched to cancel current key stack.
  "A command that does nothing.
This is useful to override bindings to do nothing."
  (values))

(defclass hook-description ()
  ((name :accessor name :initarg :name
         :initform ""
         :type string
         :documentation "The hook name.")
   (value :accessor value :initarg :value
          :documentation "The hook value.")))

(defmethod object-string ((hook-desc hook-description))
  (name hook-desc))
(defmethod object-display ((hook-desc hook-description))
  (name hook-desc))

(defmethod object-string ((handler hooks:handler))
  (str:downcase (hooks:name handler)))
(defmethod object-display ((handler hooks:handler))
  (str:downcase (hooks:name handler)))

(defun hook-completion-filter ()
  (flet ((list-hooks (object)
           (mapcar (lambda (hook)
                     (make-instance 'hook-description
                                    :name (str:downcase (closer-mop:slot-definition-name hook))
                                    :value (funcall (symbol-function (closer-mop:slot-definition-name hook))
                                                    object)))
                   (delete-if-not (lambda (s)
                                    (let ((name (closer-mop:slot-definition-name s)))
                                      (and (str:ends-with-p "-hook" (string name) :ignore-case t)
                                           (fboundp name))))
                                  (closer-mop:class-slots (class-of object))))))
    (let ((window-hooks (list-hooks (current-window)))
          (buffer-hooks (list-hooks (current-buffer)))
          (browser-hooks (list-hooks *browser*)))
      (lambda (minibuffer)
        (fuzzy-match (input-buffer minibuffer)
                     (append window-hooks
                             buffer-hooks
                             browser-hooks))))))

(defun handler-completion-filter (hook)
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer)
                 (hooks:handlers hook))))

(defun disabled-handler-completion-filter (hook)
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer)
                 (hooks:disabled-handlers hook))))

(define-command disable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to disable handler"
                            :completion-function (hook-completion-filter))))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Disable handler from ~a" (name hook-desc))
                    :completion-function (handler-completion-filter (value hook-desc)))))
      (hooks:disable-hook (value hook-desc) handler))))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to enable handler"
                            :completion-function (hook-completion-filter))))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Enable handler from ~a" (name hook-desc))
                    :completion-function (disabled-handler-completion-filter (value hook-desc)))))
      (hooks:enable-hook (value hook-desc) handler))))
