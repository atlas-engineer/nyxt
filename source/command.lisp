;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *command-list* '()
  "The list of known commands, for internal use only.")

;; We need a `command' class for multiple reasons:
;; - Identify commands uniquely (although being a member of `*command-list*' is enough).
;;
;; - Customize minibuffer display value with `object-string'.
;;
;; - Access-time: This is useful to sort command by the time they were last
;;   called.  The only way to do this is to persist the command instances.
(define-class command ()
  ((sym nil
        :type (or symbol null))
   (pkg nil
        :type (or package null))
   (sexp nil
         :type t)
   (access-time 0
                :type integer
                :documentation "Last time this command was called from minibuffer.
This can be used to order the commands."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

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
  (uiop:run-program (list \"mpv\" (object-string (url buffer)))))"
  (let ((documentation (if (stringp (first body))
                           (prog1
                               (list (first body))
                             (setf body (rest body)))
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (declares (when (and (listp (first body))
                             (eq 'declare (first (first body))))
                    (prog1
                        (first body)
                      (setf body (rest body)))))
        (before-hook (intern (str:concat (symbol-name name) "-BEFORE-HOOK")))
        (after-hook (intern (str:concat (symbol-name name) "-AFTER-HOOK"))))
    `(progn
       (export-always ',before-hook)
       (defparameter ,before-hook (hooks:make-hook-void))
       (export-always ',after-hook)
       (defparameter ,after-hook (hooks:make-hook-void))
       (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                         (eq (pkg c) (symbol-package ',name))))
                        *command-list*)
         (push (make-instance 'command
                              :sym ',name
                              :pkg (symbol-package ',name)
                              :sexp '(define-command (,@arglist) ,@body))
               *command-list*))
       (export-always ',name (symbol-package ',name))
       ;; We define the function at compile-time so that macros from the same
       ;; file can find the symbol function.
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ;; We use defun to define the command instead of storing a lambda because we want
         ;; to be able to call the foo command from Lisp with (FOO ...).
         (defun ,name ,arglist
           ,@documentation
           ,declares
           (handler-case
               (progn
                 (hooks:run-hook ,before-hook)
                 ;; (log:debug "Calling command ~a." ',name)
                 ;; TODO: How can we print the arglist as well?
                 ;; (log:debug "Calling command (~a ~a)." ',name (list ,@arglist))
                 (prog1
                     (progn
                       ,@body)
                   (hooks:run-hook ,after-hook)))
             (nyxt-condition (c)
               (format t "~s" c))))))))

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
  "Return the list of variable symbols in Nyxt-related-packages."
  (delete-if (complement #'boundp) (package-defined-symbols)))

(defun package-functions ()
  "Return the list of function symbols in Nyxt-related packages."
  (delete-if (complement #'fboundp) (package-defined-symbols)))

(defun package-classes ()
  "Return the list of class symbols in Nyxt-related-packages."
  (delete-if (lambda (sym)
               (not (and (find-class sym nil)
                         ;; Discard non-standard objects such as structures or
                         ;; conditions because they don't have public slots.
                         (mopu:subclassp (find-class sym) (find-class 'standard-object)))))
             (package-defined-symbols)))

(define-class slot ()
  ((name nil
         :type (or symbol null))
   (class-sym nil
              :type (or symbol null)))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

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
  "Return the list of exported slots."
  (delete-if
   (complement #'exported-p)
   (mopu:slot-names class-sym)))

(defun package-slots ()
  "Return the list of all slot symbols in `:nyxt' and `:nyxt-user'."
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
`nyxt-user' package are included.  This is useful since mode togglers are
usually part of their own mode / package and would not be listed otherwise.
For `nyxt-user' commands, users expect them to be listed out of the box without
extra fiddling."
  ;; TODO: Make sure we list commands of inherited modes.
  (let ((list-togglers-p (member 'base-mode mode-symbols)))
    (if mode-symbols
        (remove-if (lambda (c)
                     (and (or (not list-togglers-p)
                              (and (not (mode-toggler-p c))
                                   (not (eq (find-package 'nyxt-user)
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

(defmethod command-function ((command command))
  "Return the function associated to COMMAND.
This function can be `funcall'ed."
  (symbol-function (find-symbol
                    (string (sym command))
                    (pkg command))))

(defun command-display (command)
  ;; Use `(current-window :no-rescan)' or else the minibuffer will stutter
  ;; because of the FFI calls.
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
              ((or "" "nyxt" "nyxt-user") "")
              (a (format nil " [~a]" a))))))

(declaim (ftype (function (function) (or null command)) function-command))
(defun function-command (function)
  "Return the command associated to FUNCTION, if any."
  (find-if (lambda (cmd)
             (eq function (command-function cmd)))
           (list-commands)))


(defmethod run ((command command) &rest args)
  "Run COMMAND over ARGS and return its result.
This is blocking, see `run-async' for an asynchronous way to run commands."
  (let ((channel (make-channel 1)))
    (pexec ()
      (calispel:! channel
               ;; Bind current buffer for the duration of the command.  This
               ;; way, if the user switches buffer after running a command
               ;; but before command termination, `current-buffer' will
               ;; return the buffer from which the command was invoked.
               (with-current-buffer (current-buffer)
                 (apply #'funcall-safely (command-function command) args))))
    (calispel:? channel)))

(defmethod run-async ((command command) &rest args)
  "Run COMMAND over ARGS asynchronously.
See `run' for a way to run commands in a synchronous fashion and return the
result."
  (pexec ()
    (with-current-buffer (current-buffer) ; See `run' for why we bind current buffer.
      (apply #'funcall-safely (command-function command) args))))

(define-command noop ()                 ; TODO: Replace with ESCAPE special command that allows dispatched to cancel current key stack.
  "A command that does nothing.
This is useful to override bindings to do nothing."
  (values))
